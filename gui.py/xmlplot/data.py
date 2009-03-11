# Import modules from standard Python library
import os, sys, re, datetime, shutil, StringIO, types

# Import additional third party modules
import numpy

# Import our custom modules
import common, xmlstore.util, xmlstore.xmlstore

def getNetCDFFile(path):
    """Returns a NetCDFFile file object representing the NetCDF file
    at the specified path. The returned object follows
    Scientific.IO.NetCDFFile conventions.
    
    Note: this is the *only* function that needs to know which NetCDF
    module to use. All other functions just operate on an object
    returned by this function, and expect this object to follow
    Scientific.IO.NetCDFFile conventions. Thus adding/replacing a module
    for NetCDF support should only require a chnage in this function.
    """

    # First import NetCDF file format support (we do this here rather
    # than on import, because this module can be useful without NetCDF
    # support as well).

    # First check if the file exists in the first place.
    if not os.path.isfile(path):
        raise Exception('"%s" is not an existing file.' % path)
    
    # We prefer ScientificPython. Try that first.
    ready = True
    try:
        import Scientific.IO.NetCDF
    except Exception,e:
        error = 'Cannot load Scientific.IO.NetCDF. Reason: %s.\n' % str(e)
        ready = False
    if ready:    
        try:
            nc = Scientific.IO.NetCDF.NetCDFFile(path)
        except Exception, e:
            raise Exception('An error occured while opening the NetCDF file "%s": %s' % (path,str(e)))
        return nc

    # ScientificPython failed. Try netCDF4 instead.
    ready = True
    try:
        import netCDF4
    except Exception,e:
        error += 'Cannot load netCDF4. Reason: %s.\n' % str(e)
        ready = False
    if ready:
        try:
            nc = netCDF4.Dataset(path)
        except Exception, e:
            raise Exception('An error occured while opening the NetCDF file "%s": %s' % (path,str(e)))
        return nc
    
    # ScientificPython and netCDF4 failed. Try pynetcdf instead.
    try:
        import pynetcdf
    except Exception,e:
        error += 'Cannot load pynetcdf. Reason: %s.\n' % str(e)
        raise Exception('Cannot load a module for NetCDF reading. Please install either ScientificPython, netCDF4 or pynetcdf.' % (error,))
    pyver = sys.version_info
    if (pyver[0]==2 and pyver[1]>=5) or pyver[0]>2:
        print '%sWe will use pynetcdf for NetCDF support. Note though that pynetcdf has known incompatibilities with Python 2.5 and higher, and you are using Python %i.%i.%i.' % (error,pyver[0],pyver[1],pyver[2])
    try:
        nc = pynetcdf.NetCDFFile(path)
    except Exception, e:
        raise Exception('An error occured while opening the NetCDF file "%s": %s' % (path,str(e)))
    return nc


class LinkedFileVariableStore(common.VariableStore,xmlstore.datatypes.DataFileEx):

    # XML store-derived class for storing (cached) metadata of a data file,
    # such as coordinate ranges.
    # This is implemented as XML store (rather than Python object) because it
    # needs to be saved in a descriptive form along with the data files themselves.
    class DataFileCache(xmlstore.xmlstore.TypedStore):
        def __init__(self,valueroot=None,adddefault = True):
            schemadom = os.path.join(common.getDataRoot(),'schemas/datafilecache/0001.xml')
            xmlstore.xmlstore.TypedStore.__init__(self,schemadom,valueroot,adddefault=adddefault)

        schemadict = None
        @staticmethod
        def getDefaultSchemas():
            if LinkedFileVariableStore.DataFileCache.schemadict==None:
                LinkedFileVariableStore.DataFileCache.schemadict = xmlstore.xmlstore.ShortcutDictionary.fromDirectory(os.path.join(common.getDataRoot(),'schemas/datafilecache'))
            return LinkedFileVariableStore.DataFileCache.schemadict

    class LinkedFileVariable(common.Variable):

        def __init__(self,store,data,index):
            common.Variable.__init__(self,store)
            self.store = store
            self.data = data
            self.index = index

        def getName_raw(self):
            return self.data[0]

        def getLongName(self):
            return self.data[1]

        def getUnit(self):
            return self.data[2]
            
        def getDimensions_raw(self):
            return self.store.dimensionorder[:]

        def getSlice(self,bounds):
            assert False, 'This function must be implemented by inheriting class.'
            
    @classmethod
    def createTypedStore(ownclass):
        return LinkedFileVariableStore.DataFileCache()

    linkedfilename = 'linkedfile_metadata.xml'
    rootnodename = 'DataFile'

    @classmethod
    def createObject(ownclass,datafile,context,infonode,nodename):
        finfo = xmlstore.util.findDescendantNode(infonode,['fileinfo'])
        assert finfo!=None, 'Node "%s" lacks "fileinfo" attribute.' % node
        store = None
        type = finfo.getAttribute('type')
        if type=='pointsintime':
            store = LinkedMatrix(datafile,context,infonode,nodename,type=0,dimensions={'time':{'label':'time','datatype':'datetime','preferredaxis':'x'}},dimensionorder=('time',))
        elif type=='profilesintime':
            store = LinkedProfilesInTime(datafile,context,infonode,nodename,dimensions={'time':{'label':'time','datatype':'datetime','preferredaxis':'x'},'z':{'label':'depth','unit':'m','preferredaxis':'y'}},dimensionorder=('time','z'))
        elif type=='singleprofile' or type=='verticalgrid':
            store = LinkedMatrix(datafile,context,infonode,nodename,type=1)
        else:
            assert False, 'Linked file has unknown type "%s".' % node.type
        return store
        
    # Dictionary linking our data type names to MatPlotLib data types.
    # Note that times are stored as numeric values (via matplotlib.dates.date2num)
    mpldatatypes = {'datetime':numpy.float64,
                    'float':   numpy.float32,
                    'float32': numpy.float32,
                    'float64': numpy.float64}

    def __init__(self,datafile,context,infonode,nodename,dimensions={},dimensionorder=(),variables=[],datatype='float',defaultfilename='data'):
    
        common.VariableStore.__init__(self)
        xmlstore.datatypes.DataFileEx.__init__(self,datafile,context,infonode,nodename)

        # Copy data from supplied dimensions and variables
        self.dimensions = {}
        for dimname,dimdata in dimensions.iteritems():
            self.dimensions[dimname] = common.VariableStore.getDimensionInfo_raw(self,None)
            self.dimensions[dimname].update(dimdata)
        self.vardata = list(variables)
        self.dimensionorder = list(dimensionorder)
        
        # Supplement dimensions and variables with information in
        # supplied XML node (if any)
        self.filename = defaultfilename
        if infonode!=None:
            finfo = xmlstore.util.findDescendantNode(infonode,['fileinfo'])
            self.filename = infonode.getAttribute('name')
            if finfo.hasAttribute('datatype'): datatype = finfo.getAttribute('datatype')

            # Get variables
            fvars = xmlstore.util.findDescendantNode(finfo,['filevariables'])
            if fvars!=None:
                for ch in fvars.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filevariable':
                        longname = ch.getAttribute('label')
                        unit = ch.getAttribute('unit')
                        assert ch.hasAttribute('name'), '"name" attribute of filevariable is missing, label = %s.' % longname
                        name = ch.getAttribute('name')
                        self.vardata.append((name,longname,unit))

            # Get dimensions
            fdims = xmlstore.util.findDescendantNode(finfo,['filedimensions'])
            if fdims!=None:
                for ch in fdims.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filedimension':
                        dimdata = common.VariableStore.getDimensionInfo_raw(self,None)
                        if ch.hasAttribute('label'):         dimdata['label']         = ch.getAttribute('label')
                        if ch.hasAttribute('unit'):          dimdata['unit']          = ch.getAttribute('unit')
                        if ch.hasAttribute('datatype'):      dimdata['datatype']      = ch.getAttribute('datatype')
                        if ch.hasAttribute('preferredaxis'): dimdata['preferredaxis'] = ch.getAttribute('preferredaxis')
                        id = ch.getAttribute('name')
                        if id=='': id = dimdata['label']
                        self.dimensions[id] = dimdata
                        self.dimensionorder.append(id)

        self.data = None
        self.datatype = datatype
    
    def copy(self):
        """Returns a copy of the LinkedFileVariableStore object.
        Currently this copies descriptive metadata, but no actual values.
        """
        return LinkedFileVariableStore(None,None,None,None,self.dimensions,self.dimensionorder,self.vardata,self.datatype,defaultfilename=self.filename)
        
    def clear(self,clearfile=True):
        """Clears all data, and by default also clears the original datafile
        (if any). The metadata set on the object will be updated accordingly.
        """
        self.dataChanged(clearfile=clearfile)
        
    def setDataFile(self,datafile=None,cleardata=True):
        """Attaches a new data file as source of data. This will clear all
        metadata set on the object, and by default it will also clear any
        parsed data.
        """ 
        xmlstore.datatypes.DataFileEx.setDataFile(self,datafile)
        if cleardata: self.data = None
        
    def setData(self,data,clearfile=True):
        """Sets a new data block, automatically updating the metadata set on
        the object. By default it will clear the original datafile (if any).
        """
        self.data = data
        self.dataChanged(clearfile=clearfile)
        
    def dataChanged(self,clearfile=True):
        """Event handler, to be called just after the data has changed.
        """
        if clearfile: self.setDataFile(None,cleardata=False)
        if self.data==None: return
        
        #print '%s - caching validation result and dimension boundaries.' % self.filename
        metadata = self.getMetaData()
        for dimname in self.getDimensionNames():
            dimnode = metadata['Dimensions'].getChildById('Dimension',id=dimname,create=True)
            assert dimnode!=None, 'Failed to create Dimension node for %s.' % dimname
            dimrange = self.calculateDimensionRange(dimname)
            if dimrange==None: continue
            minval,maxval = dimrange
            if self.getDimensionInfo_raw(dimname)['datatype']=='datetime':
                dimnode['IsTimeDimension'].setValue(True)
                dimnode['MinimumTime'].setValue(common.num2date(minval))
                dimnode['MaximumTime'].setValue(common.num2date(maxval))
            else:
                dimnode['IsTimeDimension'].setValue(False)
                dimnode['Minimum'].setValue(minval)
                dimnode['Maximum'].setValue(maxval)
        metadata['Valid'].setValue(True)

    def getDimensionNames(self):
        """Returns the names of data dimensions.
        """
        return self.dimensionorder[:]
        
    def getDimensionInfo_raw(self,dimname):
        """Returns information on the specified data dimension.
        see VariableStore.getDimensionInfo for the type of
        information returned.
        """
        return self.dimensions[dimname]
        
    def getDimensionRange(self,dimname):
        """Returns the range, i.e., the tuple (minimum, maximum) of the
        specified dimension.
        """
        if self.data==None and (self.datafile==None or not self.datafile.isValid()): return None
        
        metadata = self.getMetaData()
        dimnode = metadata['Dimensions'].getChildById('Dimension',dimname)
        if dimnode==None:
            try:
                self.getData()
            except Exception,e:
                pass
            dimnode = metadata['Dimensions'].getChildById('Dimension',dimname)
            assert dimnode!=None, 'Cannot locate node for dimension %s in data file cache.' % dimname
            
        if metadata['Valid'].getValue()==False: return None

        #print '%s - using cached bounds for %s.' % (self.filename,dimname)
        if dimnode['IsTimeDimension'].getValue():
            minval = dimnode['MinimumTime'].getValue()
            maxval = dimnode['MaximumTime'].getValue()
        else:
            minval = dimnode['Minimum'].getValue()
            maxval = dimnode['Maximum'].getValue()
        if minval==None and maxval==None: return None
        return (minval,maxval)
            
    def validate(self,callback=None):
        if self.data==None and (self.datafile==None or not self.datafile.isValid()): return False
        metadata = self.getMetaData()
        valid = metadata['Valid'].getValue()
        if valid==None:
            try:
                self.getData(callback=callback)
            except Exception,e:
                pass
            valid = metadata['Valid'].getValue()
            assert valid!=None, 'Information on validity of data file %s not in data file cache.' % self.filename
        #print '%s - using cached validation result.' % self.filename
        return valid
    
    def getVariableNames_raw(self):
        """Returns the names of all variables in the store.
        """
        return [data[0] for data in self.vardata]

    def getVariableLongNames_raw(self):
        """Returns the long name of the specified variable.
        """
        return dict([(data[0],data[1]) for data in self.vardata])

    def getVariable_raw(self,varname):
        """Returns the specified variable as LinkedFileVariable object.
        """
        for (index,data) in enumerate(self.vardata):
            if data[0]==varname:
                return self.variableclass(self,data,index)
        return None
        
    def loadFromFile(self,path):
        datafile = xmlstore.datatypes.DataContainerDirectory.DataFileFile(path)
        self.setDataFile(datafile)
        datafile.release()
        
    def saveToFile(self,path,callback=None):
        """Saves the current data to file."""
        if self.datafile!=None:
            self.datafile.saveToFile(path)
        else:
            f = open(path,'w')
            self.writeData(f,callback=callback)
            f.close()
            
    def getDataFile(self,callback=None):
        if self.datafile==None:
            assert self.data!=None, 'getDataFile called with both the data file and the data in memory are not set.'
        
            # Data not present as data file object. Create one in memory on the spot.
            target = StringIO.StringIO()
            self.writeData(target,callback=callback)
            self.datafile = xmlstore.datatypes.DataFileMemory(target.getvalue(),self.filename+'.dat')
            target.close()
        return self.datafile.addref()
        
    def writeData(self,target,callback=None):
        """Writes the current data to a file-like object."""
        assert False, 'writeData must be implemented by derived class.'
        
    def getData(self,callback=None):
        if self.data==None and self.datafile!=None:
            try:
                data = self.parseDataFile(callback)
            except Exception,e:
                self.getMetaData()['Valid'].setValue(False)
                raise
            self.setData(data,clearfile=False)
        return self.data
        
    def parseDataFile(self,callback=None):
        assert False, 'parseDataFile must be implemented by derived class.'

class LinkedMatrix(LinkedFileVariableStore):

    class LinkedMatrixVariable(LinkedFileVariableStore.LinkedFileVariable):
        def getSlice(self,bounds):
            slice = self.Slice(self.getDimensions())
            
            # Get a reference to all data, and stop if the coordinate dimension is empty.
            data = self.store.getData()
            if data[0].shape[0]==0: return slice

            if slice.ndim==1:
                slice.coords[0] = data[0][:]
            slice.data = data[-1][:,self.index]
            slice.generateStaggered()
            return slice

        def getShape(self):
            data = self.store.getData()
            if data[0].shape[0]==0: return tuple()
            return data[-1][:,self.index].shape

    def __init__(self,datafile=None,context=None,infonode=None,nodename=None,type=0,dimensions={},dimensionorder=(),variables=[],defaultfilename='data'):
        LinkedFileVariableStore.__init__(self,datafile,context,infonode,nodename,dimensions,dimensionorder,variables,defaultfilename=defaultfilename)
        self.variableclass = self.LinkedMatrixVariable
        assert len(self.dimensions)<=1, 'Linkedmatrix objects can only be used with 0 or 1 coordinate dimensions, but %i are present.' % len(self.dimensions)
        self.type = type
        
    def copy(self):
        """Returns a copy of the LinkedMatrix object.
        Currently this copies descriptive metadata, but no actual values.
        """
        return LinkedMatrix(dimensions=self.dimensions,dimensionorder=self.dimensionorder,variables=self.vardata,type=self.type,defaultfilename=self.filename)

    def clear(self,clearfile=True):
        """Clears all contained data."""
        self.data = []
        if len(self.dimensions)==1:
            dimdatatype = self.dimensions[self.dimensionorder[0]]['datatype']
            self.data.append(numpy.empty((0,),self.mpldatatypes[dimdatatype]))
        self.data.append(numpy.empty((0,len(self.vardata)),self.mpldatatypes[self.datatype]))
        LinkedFileVariableStore.clear(self,clearfile=clearfile)
        
    def calculateDimensionRange(self,dimname):
        ind = self.dimensionorder.index(dimname)
        dimdata = self.getData()[ind]
        if 0 in dimdata.shape: return None
        return (dimdata.min(),dimdata.max())

    def parseDataFile(self,callback=None):
        if self.datafile==None or not self.datafile.isValid(): return None

        if self.type==0:
            # Unknown number of rows
            res = self.loadDataFile_UnknownCount(callback)
        elif self.type==1:
            # Known number of rows
            res = self.loadDataFile_KnownCount(callback)
        else:
            assert False, 'unknown LinkedMatrix type %i.' % self.type

        return res
        
    def loadDataFile_KnownCount(self,callback):
        """Loads data from a DataFile object."""
        # Get number of dimensions and variables.
        dimcount = len(self.dimensions)
        varcount = len(self.vardata)

        # Get the size of the file (in bytes, may be None if the size is not known)
        # This will be used in combination with the position of the file pointer to report progress.
        filesize = float(self.datafile.getSize())
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()

        # First line contains number of observations to follow.
        line = f.readline()
        if line=='':
            raise Exception('File is empty. Expected number of observations on first line.')
        obscount = int(line)

        # Allocate arrays for storage of coordinates and variable values
        values = numpy.empty((obscount,varcount),self.mpldatatypes[self.datatype])
        if dimcount==1:
            # One coordinate dimension present; allocate an array for its values.
            dimtype = self.dimensions[self.dimensionorder[0]]['datatype']
            dimisdate = (dimtype=='datetime')
            if dimisdate:
                datetimere = re.compile('(\d\d\d\d).(\d\d).(\d\d) (\d\d).(\d\d).(\d\d)')
                prevdate = None
            dimvalues = numpy.empty((obscount,),self.mpldatatypes[dimtype])

        for irow in range(values.shape[0]):
            # Read a line (stop if end-of-file was reached)
            line = f.readline()
            if line=='':
                raise Exception('End-of-file reached after line %i, but expecting still %i more rows of observations.' % (irow+1,values.shape[0]-irow))
            iline = irow+2  # One-based line index
            
            if dimcount==1:
                if dimisdate:
                    # Read the date + time
                    datematch = datetimere.match(line)
                    if datematch==None:
                        raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
                    refvals = map(int,datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                    dimvalue = xmlstore.util.dateTimeFromTuple(refvals)
                    if prevdate!=None and dimvalue<prevdate:
                        raise Exception('Line %i: observation time %s lies before previous observation time %s. Times should be increasing.' % (iline,xmlstore.util.formatDateTime(dimvalue),xmlstore.util.formatDateTime(prevdate)))
                    prevdate = dimvalue
                    dimvalue = common.date2num(dimvalue)
                
                    # Read variable values.
                    data = line[datematch.end()+1:].split()
                else:
                    # Split line, convert values to floats and store first as coordinate.
                    data = map(float,line.split())
                    dimvalue = data.pop(0)
            else:
                data = map(float,line.split())

            if len(data)<varcount:
                raise Exception('Line %i contains only %i observations, where %i are expected.' % (iline,len(data),varcount))
            
            # Store time and values.
            if dimcount==1: dimvalues[irow] = dimvalue
            values[irow,:] = data[:varcount]
            
            # Inform caller about progress
            if callback!=None and iline%1000==0:
                progress = None
                if filesize!=None:
                    try:
                        progress = float(f.tell())/filesize
                    except AttributeError:
                        progress = None
                callback(progress,'read %i lines.' % iline)
            
        # Close data file
        f.close()

        # Succeeded in reading the data: store them internally.
        if dimcount==1:
            return [dimvalues,values]
        else:
            return [values]

    def loadDataFile_UnknownCount(self,callback):
        varcount = len(self.vardata)
        
        # Get the size of the file (in bytes, may be None if the size is not known)
        # This will be used in combination with the position of the file pointer to report progress.
        filesize = float(self.datafile.getSize())
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()

        # Compile regular expression for reading dates.
        datetimere = re.compile('(\d\d\d\d).(\d\d).(\d\d) (\d\d).(\d\d).(\d\d)')
        
        # Get the data type to use for the dimension
        dimdatatype = self.dimensions[self.dimensionorder[0]]['datatype']
        
        # Size of one memory slab
        buffersize = 1000

        times = []
        values = []
        iline = 0
        while True:
            # Read a line (stop if end-of-file was reached)
            line = f.readline()
            if line=='': break

            # Calculate position in current memory slab, create new slab if needed.
            ipos = iline%buffersize
            if ipos==0:
                times.append(numpy.empty((buffersize,),         self.mpldatatypes[dimdatatype]))
                values.append(numpy.empty((buffersize,varcount),self.mpldatatypes[self.datatype]))

            # Increment current line number
            iline += 1
            
            # Read the date + time
            datematch = datetimere.match(line)
            if datematch==None:
                raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
            refvals = map(int,datematch.groups()) # Convert matched strings into integers
            curdate = xmlstore.util.dateTimeFromTuple(refvals)
            times[-1][ipos] = common.date2num(curdate)
            
            # Read values.
            data = line[datematch.end()+1:].split()
            if len(data)<varcount:
                raise Exception('Line %i contains only %i observations, where %i are expected.' % (iline,len(data),varcount))
            values[-1][ipos,:] = map(float,data[:varcount])
            
            # Inform caller about progress
            if callback!=None and iline%1000==0:
                progress = None
                if filesize!=None:
                    try:
                        progress = float(f.tell())/filesize
                    except AttributeError:
                        progress = None
                callback(progress,'read %i lines.' % iline)

        if len(times)>0:
            # Delete unused rows in last memory slab.
            times [-1] = times [-1][0:iline%buffersize]
            values[-1] = values[-1][0:iline%buffersize,:]
        
            # Concatenate memory slab.
            times = numpy.concatenate(times,axis=0)
            values = numpy.concatenate(values,axis=0)
        else:
            # No data read: create empty time and value arrays
            times = numpy.zeros((0,),self.mpldatatypes[dimdatatype])
            values = numpy.zeros((0,varcount),self.mpldatatypes[self.datatype])
            
        # Close data file
        f.close()

        # Succeeded in reading the data: store them internally.
        return [times,values]

    def writeData(self,target,callback=None):
        """Writes the current data to a file-like object."""
        # Get number of dimensions and variables, and get shortcuts to the data.
        dimcount = len(self.dimensions)
        data = self.getData()
        if dimcount==1:
            # One coordinate dimension present; get the data type of that dimension.
            dimdata = data[0]
            dimtype = self.dimensions.values()[0]['datatype']
            dimisdate = (dimtype=='datetime')
            if dimisdate: dimdata = common.num2date(dimdata)
        varcount = len(self.vardata)
        vardata = data[-1]
        
        if self.type==1:
            # Write first line with number of observations.
            target.write('%i\n' % vardata.shape[0])
        
        # Write lines with observations.
        for iline in range(vardata.shape[0]):
            if dimcount==1:
                if dimisdate:
                    target.write(xmlstore.util.formatDateTime(dimdata[iline],iso=True))
                else:
                    target.write('%.12g' % dimdata[iline])
            for ivar in range(varcount):
                target.write('\t%.12g' % vardata[iline,ivar])
            target.write('\n')
            if callback!=None and iline%1000==0:
                callback(float(iline)/vardata.shape[0],'wrote %i lines.' % iline)

class LinkedProfilesInTime(LinkedFileVariableStore):

    class LinkedProfilesInTimeVariable(LinkedFileVariableStore.LinkedFileVariable):
        def getSlice(self,bounds):
            varslice = self.Slice(self.getDimensions())

            data = self.store.getGriddedData()
            if data[0].shape[0]==0: return varslice

            timebounds = common.findIndices((bounds[0].start,bounds[0].stop),data[0])
            varslice.coords[0] = data[0][timebounds[0]:timebounds[1]+1]
            varslice.coords[1] = data[1]
            varslice.data = data[2][timebounds[0]:timebounds[1]+1,:,self.index]
            varslice.generateStaggered()
                    
            return varslice

        def getShape(self):
            data = self.store.getGriddedData()
            if data[0].shape[0]==0: return tuple()
            return data[-1][:,:,self.index].shape

    def __init__(self,datafile,context,infonode,nodename,dimensions=[],dimensionorder=(),variables=[],defaultfilename='data'):
        LinkedFileVariableStore.__init__(self,datafile,context,infonode,nodename,dimensions,dimensionorder,variables,defaultfilename=defaultfilename)
        self.variableclass = self.LinkedProfilesInTimeVariable
        
    def copy(self):
        """Returns a copy of the LinkedProfilesInTime object.
        Currently this copies descriptive metadata, but no actual values.
        """
        return LinkedProfilesInTime(None,None,None,None,dimensions=self.dimensions,dimensionorder=self.dimensionorder,variables=self.vardata,defaultfilename=self.filename)

    def setDataFile(self,datafile=None,cleardata=True):
        LinkedFileVariableStore.setDataFile(self,datafile,cleardata=cleardata)
        if cleardata: self.griddeddata = None

    def clear(self,clearfile=True):
        self.data = (numpy.empty((0,)),[],[])
        LinkedFileVariableStore.clear(self,clearfile=clearfile)

    def dataChanged(self,clearfile=True):
        """Event handler, must be called by external actors when they change the data."""
        self.griddeddata = None
        LinkedFileVariableStore.dataChanged(self,clearfile=clearfile)

    def calculateDimensionRange(self,dimname):
        ind = self.dimensionorder.index(dimname)
        dimdata = self.getData()[ind]
        if len(dimdata)==0: return None
        if ind==0:
            return (dimdata.min(),dimdata.max())
        else:
            dimmin,dimmax = dimdata[0].min(),dimdata[0].max()
            for iobs in range(1,len(dimdata)):
                dimmin = min(dimmin,dimdata[iobs].min())
                dimmax = max(dimmin,dimdata[iobs].max())
            return (dimmin,dimmax)
                
    def writeData(self,target,callback=None):
        """Writes the current data to a file-like object."""
        varcount = len(self.vardata)
        data = self.getData()
        assert data!=None, 'Cannot write data to file, because data is set to None.'
        times,depths,values = data
        for itime in range(times.shape[0]):
            target.write(xmlstore.util.formatDateTime(common.num2date(times[itime]),iso=True))
            curdepths = depths[itime]
            curdata = values[itime]
            depthcount = len(curdepths)
            target.write('\t%i\t1\n' % depthcount)
            for idepth in range(depthcount):
                target.write('%.9g' % curdepths[idepth])
                for ivar in range(varcount):
                    target.write('\t%.9g' % curdata[idepth,ivar])
                target.write('\n')
        
    def getGriddedData(self,callback=None):
        data = self.getData()
        if self.griddeddata==None:
            times,depths,values = data
            
            varcount = len(self.vardata)
            
            # Find unique depth levels.
            uniquedepths = {}
            for ds in depths:
                for d in ds: uniquedepths[d] = True
            
            # Create depth grid to interpolate on to. Use the observation depths if less than 200,
            # otherwise create a equidistant 200-point grid between the minimum and maximum depth.
            uniquedepths = uniquedepths.keys()
            uniquedepths.sort()
            if len(uniquedepths)<200:
                depthdatatype = self.dimensions[self.dimensionorder[1]]['datatype']
                depthgrid = numpy.array(uniquedepths,self.mpldatatypes[depthdatatype])
            else:
                depthgrid = numpy.linspace(uniquedepths[0],uniquedepths[-1],200)
                
            # Grid observed profiles to depth grid.
            griddedvalues = numpy.empty((times.shape[0],depthgrid.shape[0],varcount),self.mpldatatypes[self.datatype])
            for it in range(len(times)):
                griddedvalues[it,:,:] = common.interp1(depths[it],values[it],depthgrid)
                if callback!=None and (it+1)%20==0:
                    callback(float(it+1)/len(times),'gridded %i profiles.' % (it+1))
                
            # Store time grid, depth grid and observations.
            self.griddeddata = (times,depthgrid,griddedvalues)
            
        return self.griddeddata

    def parseDataFile(self,callback=None):
        if self.datafile==None or not self.datafile.isValid(): return None
        
        varcount = len(self.vardata)
        
        # Get the size of the file (in bytes, may be None if the size is not known)
        # This will be used in combination with the position of the file pointer to report progress.
        filesize = float(self.datafile.getSize())
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()

        # Compile regular expression for reading dates.
        datetimere = re.compile('(\d\d\d\d).(\d\d).(\d\d) (\d\d).(\d\d).(\d\d)')

        times = []
        depths = []
        values = []
        iline = 0
        while True:
            # Read a line (stop if end-of-file was reached)
            line = f.readline()
            if line=='': break
            iline += 1
            
            # Read date & time
            datematch = datetimere.match(line)
            if datematch==None:
                raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
            refvals = map(int,datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
            curdate = xmlstore.util.dateTimeFromTuple(refvals)
            curdate = common.date2num(curdate)

            # Get the number of observations and the depth direction.
            (depthcount,updown) = map(int, line[datematch.end()+1:].split())

            # Create arrays that will contains depths and observed values.
            depthdatatype = self.dimensions[self.dimensionorder[1]]['datatype']
            curdepths = numpy.empty((depthcount,),self.mpldatatypes[depthdatatype])
            curvalues = numpy.empty((depthcount,varcount),self.mpldatatypes[self.datatype])
            
            # Depths can be increasing (updown==1) or decreasing (updown!=1)
            if updown==1:
                depthindices = range(0,depthcount,1)
            else:
                depthindices = range(depthcount-1,-1,-1)
            
            # Now parse the specified number of observations to create the profiles.
            prevdepth = None
            for idepthline in depthindices:
                if callback!=None and iline%1000==0:
                    pos = f.tell()
                    callback(pos/filesize,'processed %i lines.' % iline)
                    
                # Read line
                line = f.readline()
                if line=='':
                    raise Exception('Premature end-of-file after line %i; expected %i more observations.' % (iline,depthcount-depthindices.index(idepthline)))
                iline += 1
                
                # Read values (depth followed by data) and check.
                linedata = map(float,line.split())
                if len(linedata)<varcount+1:
                    raise Exception('Line %i contains only %i value(s), where %i (1 time and %i observations) are expected.' % (iline,len(linedata),varcount+1,varcount))
                if prevdepth!=None:
                    if linedata[0]==prevdepth:
                        raise Exception('Found duplicate observation for depth %.4f at line %i.' % (linedata[0],iline))
                    if updown==1:
                        if linedata[0]<prevdepth:
                            raise Exception('Observation depth decreases from %.4f to %.4f at line %i, but the profile depth was set to increase from first to last observation.' % (prevdepth,linedata[0],iline))
                    elif linedata[0]>prevdepth:
                        raise Exception('Observation depth increases from %.4f to %.4f at line %i, but the profile depth was set to decrease from first to last observation.' % (prevdepth,linedata[0],iline))
                prevdepth = linedata[0]
                
                # Store current observation
                curdepths[idepthline] = linedata[0]
                curvalues[idepthline,:] = linedata[1:varcount+1]
                
            # Append the profiles for the current time to the list.
            times.append(curdate)
            depths.append(curdepths)
            values.append(curvalues)
            
            # Inform caller about progress.
            if callback!=None and iline%1000==0:
                pos = f.tell()
                callback(pos/filesize,'processed %i lines.' % iline)
                
        # Convert sequence with times to numpy array.
        timedatatype = self.dimensions[self.dimensionorder[0]]['datatype']
        times = numpy.array(times,self.mpldatatypes[timedatatype])
        
        # Close data file
        f.close()

        # Succeeded in reading the data: store them internally.
        return [times,depths,values]

class NetCDFStore(common.VariableStore,xmlstore.util.referencedobject):
    """Class encapsulating a NetCDF file.
    
    The file is expected to follow the COARDS convention.
    """
    
    conventions = []
    
    @staticmethod
    def registerConvention(convention):
        NetCDFStore.conventions.append(convention)
    
    @staticmethod
    def loadUnknownConvention(path):
        for convention in NetCDFStore.conventions:
            if convention.testFile(path): return convention(path)
        return NetCDFStore(path)
    
    class NetCDFVariable(common.Variable):
        def __init__(self,store,ncvarname):
            common.Variable.__init__(self,store)
            self.ncvarname = str(ncvarname)
            
        def __str__(self):
            return self.store.datafile+'/'+self.varname

        def getName_raw(self):
            return self.ncvarname

        def getLongName(self):
            nc = self.store.getcdf()
            ncvar = nc.variables[self.ncvarname]
            if hasattr(ncvar,'long_name'):
                return ncvar.long_name
            else:
                return self.getName()

        def getUnit(self):
            nc = self.store.getcdf()
            ncvar = nc.variables[self.ncvarname]
            if not hasattr(ncvar,'units'): return ''
            return common.convertUnitToUnicode(ncvar.units)
            
        def getProperties(self):
            nc = self.store.getcdf()
            ncvar = nc.variables[self.ncvarname]
            props = {}
            for key in dir(ncvar):
                if key not in ('assignValue','getValue','typecode'):
                    props[key] = getattr(ncvar,key)
            return props
            
        def getDimensions_raw(self,reassign=True):
          nc = self.store.getcdf()
          ncvar = nc.variables[self.ncvarname]
          rawdims = list(ncvar.dimensions)
          
          if reassign:
              # Re-assign dimensions based on the "coordinates" attribute of the variable.
              if hasattr(ncvar,'coordinates'):
                coords = reversed(ncvar.coordinates.split())
                coordsdims = nc.variables[coords[0]].dimensions
                inextcoorddim = 0
                for irdim,rdim in enumerate(rawdims):
                    if rdim in coordsdims:
                        rawdims[irdim] = coordsdims[inextcoorddim]
                        inextcoorddim += 1
                        
              # Re-assign dimensions based on globally specified re-assignments
              for idim,dim in enumerate(rawdims):
                rawdims[idim] = self.store.reassigneddims.get(dim,dim)
                
              # Undo re-assignments if not all coordinate dimensions are used by this variable.
              for idim in range(len(rawdims)):
                if rawdims[idim]==self.ncvarname or rawdims[idim]==ncvar.dimensions[idim]: continue
                cdims = self.store.getVariable_raw(rawdims[idim]).getDimensions_raw(reassign=False)
                for cdim in cdims:
                    if cdim not in ncvar.dimensions:
                        rawdims[idim] = ncvar.dimensions[idim]
                        break
            
          return tuple(rawdims)
          
        def getShape(self):
            nc = self.store.getcdf()
            ncvar = nc.variables[self.ncvarname]
            return ncvar.shape
          
        def hasReversedDimensions(self):
            return True

        def translateSliceSpecification(self,bounds):
          dimnames = list(self.getDimensions_raw())
          shape = self.getShape()

          # Process Ellipsis (if present) and check whether the number of boundaries matches the number of dimensions.
          bounds = common.processEllipsis(bounds,len(dimnames))
          assert len(bounds)==len(dimnames), 'Number of boundaries (%i) does not match number of dimensions (%i).' % (len(bounds),len(dimnames))
                    
          # Convert bounds to list of slice objects.
          # Non-integer bounds are initially ignored; after retrieving the coordinate arrays, these are filled in.
          boundindices,floatslices,floatindices = [],[],[]
          for idim,bound in enumerate(bounds):
            assert not (isinstance(bound,slice) and bound.step!=None),'Step argument is not yet supported.'
            if isinstance(bound,int):
                # Integer value provided as index
                assert bound>=0,          'Slice index %i lies below lower boundary of dimension %s (0).' % (bound,dimnames[idim])
                assert bound<shape[idim], 'Slice index %i exceeds upper boundary of dimension %s (%i).' % (bound,dimnames[idim],shape[idim]-1)
                boundindices.append(bound)
            elif not isinstance(bound,slice):
                # Floating point value provided as index
                boundindices.append(slice(0,shape[idim]))
                floatindices.append(idim)
            elif not (isinstance(bound.start,(int,types.NoneType)) and isinstance(bound.stop,(int,types.NoneType))):
                # Non-integer slice specification (e.g., using floating point numbers or datetime objects).
                boundindices.append(slice(0,shape[idim]))
                floatslices.append(idim)
            else:
                # Normal (integer-based) slice specification
                start,stop,step = bound.indices(shape[idim])
                boundindices.append(slice(start,stop))

          # Translate slices based on non-integer values (e.g. floating point values, dates)
          # to slices based on integers.
          for idim in floatslices:
            dimname = dimnames[idim]
            
            # Get the entire coordinate array
            coordvar = self.store.getVariable_raw(dimname)
            coorddims = list(coordvar.getDimensions())
            coords = coordvar.getSlice([boundindices[dimnames.index(cd)] for cd in coorddims], dataonly=True, cache=True)
            istart,istop = common.getboundindices(coords,coorddims.index(dimname),bounds[idim].start,bounds[idim].stop)
            boundindices[idim] = slice(istart,istop)

          # Translate indices based on non-integer values (e.g. floating point values, dates)
          # to integer indices.
          floatdimnames = [dimnames[idim] for idim in floatindices]
          newshape = [shape[idim] for idim in floatindices]
          summeddistance = numpy.zeros(newshape,dtype=numpy.float)
          for idim in floatindices:
            dimname = dimnames[idim]
            coordvar = self.store.getVariable_raw(dimname)
            coorddims = list(coordvar.getDimensions())
            for cd in coorddims:
                assert cd in dimnames,'Coordinate %s depends on %s, but the variable %s itself does not depend on %s.' % (dimname,cd,self.getName(),cd)
                assert cd in floatdimnames,'A float index is provided for dimension %s, but not for dimension %s on which %s depends.' % (dimname,cd,dimname)
            coords = coordvar.getSlice([boundindices[dimnames.index(cd)] for cd in coorddims], dataonly=True, cache=True)
            coords = common.broadcastSelective(coords,coorddims,newshape,floatdimnames)
            summeddistance += numpy.abs(coords-bounds[idim])
          indices = numpy.unravel_index(summeddistance.argmin(), newshape)
          for idim,index in zip(floatindices,indices): boundindices[idim] = index
          
          return tuple(boundindices)
          
        def getNcData(self,bounds=None):
          # Get NetCDF file and variable objects.
          nc = self.store.getcdf()
          ncvar = nc.variables[self.ncvarname]

          try:
            if bounds:
                # Bounds provided - read a slice.
                dat = numpy.asarray(ncvar[bounds])
            elif len(ncvar.shape)>0:
                # Variable is non-scalar - read all data.
                dat = numpy.asarray(ncvar[(slice(None),)*len(ncvar.shape)])
            else:
                # Variable is a scalar - read all data.
                dat = numpy.asarray(ncvar.getValue())
          except Exception, e:
            raise Exception('Unable to read values for NetCDF variable "%s". Error: %s' % (self.getName(),str(e)))

          # Start without mask, and define function for creating/updating mask
          mask = None
          def addmask(mask,newmask):
              if mask==None:
                  mask = numpy.empty(dat.shape,dtype=numpy.bool)
                  mask.fill(False)
              return numpy.logical_or(mask,newmask)

          # Process the various COARDS/CF variable attributes for missing data.
          if self.store.maskoutsiderange:
              if hasattr(ncvar,'valid_min'): mask = addmask(mask,dat<ncvar.valid_min)
              if hasattr(ncvar,'valid_max'): mask = addmask(mask,dat>ncvar.valid_max)
              if hasattr(ncvar,'valid_range'):
                  assert len(ncvar.valid_range)==2,'NetCDF attribute "valid_range" must consist of two values, but contains %i.' % len(ncvar.valid_range)
                  minv,maxv = ncvar.valid_range
                  mask = addmask(mask,numpy.logical_or(dat<minv,dat>maxv))
          if hasattr(ncvar,'_FillValue'):    mask = addmask(mask,dat==numpy.asarray(ncvar._FillValue,dtype=dat.dtype))
          if hasattr(ncvar,'missing_value'): mask = addmask(mask,dat==numpy.asarray(ncvar.missing_value,dtype=dat.dtype))

          # Apply the combined mask (if any)
          if mask!=None: dat = numpy.ma.masked_where(mask,dat,copy=False)

          # If we have to apply a transformation to the data, make sure that the data type can accommodate it.
          # Cast to the most detailed type available (64-bit float)
          if hasattr(ncvar,'scale_factor') or hasattr(ncvar,'add_offset') and dat.dtype!=numpy.float: dat = numpy.asarray(dat,dtype=numpy.float)
          
          # Apply transformation to data based on nc variable attributes.
          if hasattr(ncvar,'scale_factor'): dat *= float(ncvar.scale_factor)
          if hasattr(ncvar,'add_offset'):   dat += float(ncvar.add_offset)
          
          # If the unit is time, convert to internal time unit
          if self.store.isTimeDimension(self.ncvarname):
              timeunit,timeref = self.store.getTimeReference(self.ncvarname)
              timeref = common.date2num(timeref)
              dat = timeref+timeunit*numpy.asarray(dat,numpy.float64)
              
          return dat
              
        def getSlice(self,bounds,dataonly=False,cache=False):
          # Translate the slice specification so only slice objects an integer indices remain.
          bounds = self.translateSliceSpecification(bounds)
          
          # Retrieve the data values
          if cache:
              # Take all data from cache if present, otherwise read all data from NetCDF and store it in cache first.
              if self.ncvarname not in self.store.cachedcoords:
                  self.store.cachedcoords[self.ncvarname] = self.getNcData()
              dat = self.store.cachedcoords[self.ncvarname]
              if bounds: dat = dat[bounds]
          else:
              # Read the data slab directly from the NetCDF file.
              dat = self.getNcData(bounds)

          # If the caller wants the data values only, we are done: return the value array.
          if dataonly: return dat

          # Get dimension names
          dimnames = list(self.getDimensions_raw())

          # Create Variable.Slice object to hold coordinates and data.
          newdimnames = [d for d,b in zip(dimnames,bounds) if isinstance(b,slice)]
          varslice = self.Slice(newdimnames)

          # Retrieve coordinate values
          inewdim = 0
          for idim,dimname in enumerate(dimnames):
            # If we take a single index for this dimension, it will not be included in the output.
            if not isinstance(bounds[idim],slice): continue

            # Get the coordinate variable          
            coordvar = self.store.getVariable_raw(dimname)
            
            if coordvar==None:
                # No coordinate variable available: use indices
                coorddims = [dimname]
                coords = numpy.arange(self.getShape()[idim],dtype=numpy.float)[tuple(bounds[idim])]
            else:
                # Coordinate variable present: use it.
                coorddims = list(coordvar.getDimensions())

                # Debug check: see if all coordinate dimensions are also used by the variable.
                for cd in coorddims:
                    assert cd in dimnames, 'Coordinate dimension %s is not used by this variable (it uses %s).' % (cd,', '.join(dimnames))

                # Get coordinate values
                coordslice = [bounds[dimnames.index(cd)] for cd in coorddims]
                coords = coordvar.getSlice(coordslice, dataonly=True, cache=True)

            # Get staggered coordinates over entire domain
            if coordvar!=None and dimname in self.store.staggeredcoordinates:
                stagcoordvar = self.store.getVariable_raw(self.store.staggeredcoordinates[dimname])
                assert stagcoordvar!=None, 'Staggered coordinate for %s registered in store as %s, but not present as variable.' % (dimname,self.store.staggeredcoordinates[dimname])
                for i in range(len(coordslice)):
                    if isinstance(coordslice[i],slice): coordslice[i] = slice(coordslice[i].start,coordslice[i].stop+1)
                coords_stag = stagcoordvar.getSlice(coordslice, dataonly=True, cache=True)
            else:
                coords_stag = common.stagger(coords)
            
            # Insert data dimensions where they are lacking in coordinate
            newcoorddims = [cd for cd in coorddims if isinstance(bounds[dimnames.index(cd)],slice)]
            coords      = common.broadcastSelective(coords,     newcoorddims,dat.shape,               newdimnames)
            coords_stag = common.broadcastSelective(coords_stag,newcoorddims,[l+1 for l in dat.shape],newdimnames)

            # Assign coordinate values
            varslice.coords     [inewdim] = coords
            varslice.coords_stag[inewdim] = coords_stag
            
            inewdim += 1

          varslice.data = dat
                  
          return varslice

    def __init__(self,path=None,*args,**kwargs):
        xmlstore.util.referencedobject.__init__(self)
        common.VariableStore.__init__(self)
        
        self.datafile = None
        self.nc = None

        self.cachedcoords = {}
        self.reassigneddims = {}
        self.staggeredcoordinates = {}
        
        # Whether to mask values outside the range specified by valid_min,valid_max,valid_range
        # NetCDF variable attributes (as specified by CF convention)
        self.maskoutsiderange = True
        
        if path!=None: self.load(path,*args,**kwargs)
                
    def __str__(self):
        return self.datafile

    def getDimensionInfo_raw(self,dimname):
        res = common.VariableStore.getDimensionInfo_raw(self,dimname)
        if dimname not in self.nc.variables: return res
        varinfo = self.nc.variables[dimname]
        if hasattr(varinfo,'long_name'):
            res['label'] = varinfo.long_name
        else:
            res['label'] = dimname
        if hasattr(varinfo,'units'):
            res['unit']  = common.convertUnitToUnicode(varinfo.units)
        if dimname=='z' or dimname=='z1':
            res['label'] = 'depth'
            res['preferredaxis'] = 'y'
            if res['unit']=='meters': res['unit']='m'
        elif self.isTimeDimension(dimname):
            res['datatype'] = 'datetime'
            res['preferredaxis'] = 'x'
            res['unit'] = ''
        #elif hasattr(varinfo,'units') and varinfo.units=='degrees_north':
        #    res['preferredaxis'] = 'y'
        if hasattr(varinfo,'positive') and varinfo.positive=='down':
            res['reversed'] = True
        return res
        
    def save(self,path):
        shutil.copyfile(self.datafile,path)
        
    def unlink(self):
        if self.nc!=None:
            # Close NetCDF result file.
            self.nc.close()
            self.nc = None
            self.datafile = None
            
    def load(self,path):
        # Store link to result file, and try to open the CDF file
        self.datafile = path
        nc = self.getcdf()
        self.relabelVariables()
        self.autoReassignCoordinates()

    def autoReassignCoordinates(self):
        self.reassigneddims = {}
    
    def getcdf(self):
        """Returns a NetCDFFile file object representing the NetCDF file
        at the path in self.datafile. The returned object should follow
        Scientific.IO.NetCDFFile conventions.
        """
        if self.nc!=None: return self.nc
        assert self.datafile!=None, 'The path to the NetCDF file has not yet been set. This may imply that the object has been unlinked.'
        self.nc = getNetCDFFile(self.datafile)
        return self.nc

    def getVariableNames_raw(self):
        return map(str,self.getcdf().variables.keys())

    def getVariableLongNames_raw(self):
      varnames = self.getVariableNames_raw()
      nc = self.getcdf()
      vardict = {}
      for varname in varnames:
          if varname not in nc.variables:
            vardict[varname] = varname
            continue
          ncvar = nc.variables[varname]
          if hasattr(ncvar,'long_name'):
            vardict[varname] = ncvar.long_name
          else:
            vardict[varname] = varname
      return vardict

    def getVariable_raw(self,varname):
        ncvarname = str(varname)
        nc = self.getcdf()
        if ncvarname not in nc.variables: return None
        return self.NetCDFVariable(self,ncvarname)
                
    def getDimensions(self):
        nc = self.getcdf()
        ncdims = list(nc.dimensions)
        def cmpdims(x,y):
            for vn in nc.variables.keys():
                v = nc.variables[vn]
                if x in v.dimensions and y in v.dimensions:
                    curdims = list(v.dimensions)
                    return cmp(curdims.index(x),curdims.index(y))
            return 0
        ncdims.sort(cmp=cmpdims)
        return ncdims

    def getDefaultCoordinateDelta(self,dimname,coord):
        return 1.
        
    class ReferenceTimeParseError(Exception):
        def __init__(self,error):
            Exception.__init__(self,error)
        
    def isTimeDimension(self,dimname):
        """See if specified dimension is a time dimension according to COARDS convention.
        """
        try:
            timeunit,timeref = self.getTimeReference(dimname)
        except self.ReferenceTimeParseError:
            return False
        return True

    def getTimeReference(self,dimname):
      """Parses the "units" attribute of the NetCDF variable, and returns the time unit
      (in days) and the reference date. Throws an exception if the "units" attribute does
      not match the COARDS/udunits convention for specifying time offsets.
      
      Supposedly the udunits package could do this, but so far I have not found a minimal
      udunits module for Python.
      """
      nc = self.getcdf()
      if dimname not in nc.variables:
          raise self.ReferenceTimeParseError('dimensions "%s" does not have an associated variable.' % (dimname,))

      cdfvar = self.getcdf().variables[dimname]
      if not hasattr(cdfvar,'units'):
          raise self.ReferenceTimeParseError('variable "%s" lacks "units" attribute.' % (dimname,))
        
      # Retrieve time unit (in days) and reference date/time, based on COARDS convention.
      fullunit = cdfvar.units
      if ' since ' not in fullunit:
          raise self.ReferenceTimeParseError('"units" attribute of variable "%s" equals "%s", which does not follow COARDS convention. Problem: string does not contain " since ".' % (dimname,fullunit))
      timeunit,reftime = fullunit.split(' since ')
      
      # Parse the reference date, time and timezone
      datematch = re.match(r'(\d\d\d\d)[-\/](\d{1,2})-(\d{1,2})\s*',reftime)
      if datematch==None:
        raise self.ReferenceTimeParseError('"units" attribute of variable "time" equals "%s", which does not follow COARDS convention. Problem: cannot parse date in "%s".' % (fullunit,reftime))
      year,month,day = map(int,datematch.group(1,2,3))
      year = max(year,1900) # datetime year>=datetime.MINYEAR, but strftime needs year>=1900
      hours,minutes,seconds,mseconds = 0,0,0,0
      reftime = reftime[datematch.end():]
      if len(reftime)>0:
        timematch = re.match(r'(\d{1,2}):(\d{1,2}):(\d{1,2}(?:\.\d*)?)\s*',reftime)
        if timematch==None:
            raise self.ReferenceTimeParseError('"units" attribute of variable "time" equals "%s", which does not follow COARDS convention. Problem: cannot parse time in "%s".' % (fullunit,reftime))
        hours,minutes = map(int,timematch.group(1,2))
        seconds = float(timematch.group(3))
        mseconds = 1e6*(seconds % 1.)
        seconds = int(seconds)
        reftime = reftime[timematch.end():]
      dateref = datetime.datetime(year,month,day,hours,minutes,seconds,tzinfo=xmlstore.util.utc)
      if len(reftime)>0:
        timezonematch = re.match(r'(-?\d{1,2})(?::?(\d\d))?$',reftime)
        if timezonematch==None:
            raise self.ReferenceTimeParseError('"units" attribute of variable "time" equals "%s", which does not follow COARDS convention. Problem: cannot parse time zone in "%s".' % (fullunit,reftime))
        if timezonematch.group(2)==None:
            dhour,dmin = int(timezonematch.group(1)),0
        else:
            dhour,dmin = map(int,timezonematch.group(1,2))
            if dhour<0: dmin = -dmin
        dateref -= datetime.timedelta(hours=dhour,minutes=dmin)
      
      # Get time unit in number of days.
      timeunit = timeunit.lower()
      if timeunit in ('seconds','second','secs','sec','ss','s'):
          timeunit = 1./86400.
      elif timeunit in ('minutes','minute','mins','min'):
          timeunit = 1./1440.
      elif timeunit in ('hours','hour','hrs','hr','hs','h'):
          timeunit = 1./24.
      elif timeunit in ('days','day','ds','d'):
          timeunit = 1.
      elif timeunit in ('years','year','yrs','yr','ys','y'):
          timeunit = 365.   # udunits convention: year=365 days
      else:
          raise self.ReferenceTimeParseError('"units" attribute of variable "time" equals "%s", which does not follow COARDS convention. Problem: unknown time unit "%s".' % (fullunit,timeunit))
      
      return timeunit,dateref

class NetCDFStore_GOTM(NetCDFStore):
    """Class encapsulating a GOTM/GETM-produced NetCDF file.
    
    The file is expected to follow the COARDS/CF convention, and in addition assumes
    
    - the GOTM/GETM convention for storing time-variable depth/leyer heights (h + elev).
    - the GETM convention for curvilinear grids (xic, etac -> lonc, latc)
    """
    
    @staticmethod
    def testFile(path):
        match = False
        nc = getNetCDFFile(path)
        ncvars,ncdims = nc.variables,nc.dimensions
        
        # Test for GETM with curvilinear coordinates
        if ('xic'  in ncdims and 'etac' in ncdims and
            'lonc' in ncvars and 'latc' in ncvars): match = True

        # Test for GETM with cartesian coordinates
        if ('xc'  in ncdims and 'yc' in ncdims and
            'lonc' in ncvars and 'latc' in ncvars): match = True

        # Test for GOTM with variable layer heights and sea surface elevation
        if ('z' in ncdims and 'z1' in ncdims and
            'h' in ncvars and 'zeta' in ncvars): match = True

        # Test for GETM with variable heights and sea surface elevation
        if ('level' in ncdims and
            'h' in ncvars and 'elev' in ncvars): match = True

        nc.close()
        return match

    def __init__(self,path=None,*args,**kwargs):
        self.xname,self.yname,self.hname,self.elevname = 'lon','lat','h','zeta'

        # Link new depth coordinates to an existing NetCDF dimension
        self.depth2coord = {}

        NetCDFStore.__init__(self,path,*args,**kwargs)
        
        # Link centered and staggered coordinates
        self.staggeredcoordinates['z' ] = 'z_stag'
        self.staggeredcoordinates['z1'] = 'z1_stag'
        
    def autoReassignCoordinates(self):
        NetCDFStore.autoReassignCoordinates(self)
        
        # Get reference to NetCDF file and its variables and dimensions.
        nc = self.getcdf()
        ncvars,ncdims = nc.variables,nc.dimensions

        # Re-assign x,y coordinate dimensions if using GETM with curvilinear coordinates
        if ('xic'  in ncdims and 'etac' in ncdims and
            'lonc' in ncvars and 'latc' in ncvars):
            self.reassigneddims['xic' ] = 'lonc'
            self.reassigneddims['etac'] = 'latc'
            self.xname,self.yname = 'xic','etac'

        # Re-assign x,y coordinate dimensions if using GETM with cartesian coordinates
        if ('xc'   in ncdims and 'yc'   in ncdims and
            'lonc' in ncvars and 'latc' in ncvars):
            self.reassigneddims['xc' ] = 'lonc'
            self.reassigneddims['yc'] = 'latc'
            self.xname,self.yname = 'xc','yc'

        # Check for GETM convention for longitude,latitude names (default is GOTM convention)
        # This will be relevant for GETM with spherical coordinates only, since curvilinear/cartesian
        # was handled above.
        if self.xname=='lon' and ('lon' not in ncvars) and 'lonc' in ncvars: self.xname = 'lonc'
        if self.yname=='lat' and ('lat' not in ncvars) and 'latc' in ncvars: self.yname = 'latc'

        # Re-assign depth coordinate dimension if using GETM
        if ('level' in ncdims and 'h' in ncvars and 'elev' in ncvars):
            # GETM: "level" reassigned to "z"
            self.reassigneddims['level' ] = 'z'
            self.hname,self.elevname = 'h','elev'
            self.depth2coord['z'] = 'level'
            
    def getVariableNames_raw(self):
        names = list(NetCDFStore.getVariableNames_raw(self))
        names.append('z')
        
        # Only add alternative depth coordinate if it is actually used in the NetCDF file.
        # (note: GETM does not use it, but GOTM does)
        if 'z1' in self.getcdf().variables: names.append('z1')
        
        return names

    def getVariable_raw(self,varname):
        if varname not in ('z','z1','z_stag','z1_stag'):
            return NetCDFStore.getVariable_raw(self,varname)

        class DepthVariable(NetCDFStore.NetCDFVariable):
            def __init__(self,store,ncvarname,dimname):
                NetCDFStore.NetCDFVariable.__init__(self,store,ncvarname)
                self.dimname = dimname
        
            def getName_raw(self):
                return self.dimname

            def getLongName(self):
                return 'depth'

            def getUnit(self):
                nc = self.store.getcdf()
                ncvar = nc.variables[self.store.hname]
                if not hasattr(ncvar,'units'): return ''
                return common.convertUnitToUnicode(ncvar.units)

            def getProperties(self):
                return {}

            def getDimensions_raw(self,reassign=True):
                dims = ['time',self.store.depth2coord.get(self.dimname,self.dimname),self.store.yname,self.store.xname]
                if reassign: dims = [self.store.reassigneddims.get(d,d) for d in dims]
                return dims
                
            def getShape(self):
                return self.getNcData().shape
                
            def getNcData(self,bounds=None):
                if 'z' not in self.store.cachedcoords:
                    # Get layer heights (dimension 0: time, dimension 1: depth, dimension 2: y coordinate, dimension 3: x coordinate)
                    h = self.store[self.store.hname].getSlice((Ellipsis,),dataonly=True)
                    
                    # Get initial elevation
                    elev = self.store[self.store.elevname].getSlice((0,Ellipsis,),dataonly=True)
                    
                    # Fill masked values (we do not want coordinate arrays with masked values)
                    # This should not have any effect, as the value arrays should also be masked at
                    # these locations.
                    # Check for the "filled" attribute to see if these are masked arrays.
                    if hasattr(h,   'filled'): h = h.filled(0.)
                    if hasattr(elev,'filled'): elev = elev.filled(0.)
                    
                    # Get depths of interfaces
                    z_stag = h.cumsum(axis=1)
                    sliceshape = list(z_stag.shape)
                    sliceshape[1] = 1
                    z_stag = numpy.concatenate((numpy.zeros(sliceshape,z_stag.dtype),z_stag),axis=1)
                    bottomdepth = z_stag[0,-1,...]-elev
                    z_stag -= bottomdepth

                    # Get depths of layer centers
                    z = z_stag[:,1:z_stag.shape[1],...]-0.5*h[:,:,...]
                    
                    # The actual interface coordinate z1 lacks the bottom interface
                    z1 = z_stag[:,1:,...]
                    
                    # Use the actual top and bottom of the column as boundary interfaces for the
                    # grid of the interface coordinate.
                    z1_stag = numpy.concatenate((numpy.take(z_stag,(0,),1),z[:,1:,...],numpy.take(z_stag,(-1,),1)),1)
                    
                    # Use normal staggering for the time, longitude and latitude dimension.
                    z_stag  = common.stagger(z_stag, (0,2,3),defaultdeltafunction=self.store.getDefaultCoordinateDelta,dimnames=self.getDimensions_raw())
                    z1_stag = common.stagger(z1_stag,(0,2,3),defaultdeltafunction=self.store.getDefaultCoordinateDelta,dimnames=self.getDimensions_raw())

                    # Store all coordinates in cache
                    self.store.cachedcoords['z']       = z
                    self.store.cachedcoords['z1']      = z1
                    self.store.cachedcoords['z_stag']  = z_stag
                    self.store.cachedcoords['z1_stag'] = z1_stag

                if bounds==None:
                    return self.store.cachedcoords[self.dimname]
                else:                        
                    return self.store.cachedcoords[self.dimname][bounds]

        return DepthVariable(self,varname,varname)

    def getDefaultCoordinateDelta(self,dimname,coords):
        # Only operate on 1D coordinates
        if coords.ndim>1: return NetCDFStore.getDefaultCoordinateDelta(self,dimname,coords)

        # Only operate on time dimension
        try:
            timeunit,timeref = self.getTimeReference(dimname)
        except self.ReferenceTimeParseError:
            return NetCDFStore.getDefaultCoordinateDelta(self,dimname,coords)
            
        # Take delta as the difference between the reference time and the first time step
        if coords[0]>timeref: return coords[0]-timeref
        
        return 1.

class NetCDFStore_MOM4(NetCDFStore):
    @staticmethod
    def testFile(path):
        match = False
        nc = getNetCDFFile(path)
        ncvars,ncdims = nc.variables,nc.dimensions
        if ('xt_ocean' in ncdims and 'yt_ocean' in ncdims and
            'geolon_t' in ncvars and 'geolat_t' in ncvars): match = True
        nc.close()
        return match

    def __init__(self,path=None,*args,**kwargs):
        NetCDFStore.__init__(self,path,*args,**kwargs)

    def autoReassignCoordinates(self):
        NetCDFStore.autoReassignCoordinates(self)
        
        # Re-assign x,y coordinate dimensions to longitude, latitude
        nc = self.getcdf()
        ncvars,ncdims = nc.variables,nc.dimensions
        if ('xt_ocean'  in ncdims and 'yt_ocean' in ncdims and
            'geolon_t' in ncvars and 'geolat_t' in ncvars):
            self.reassigneddims['xt_ocean' ] = 'geolon_t'
            self.reassigneddims['yt_ocean'] = 'geolat_t'

NetCDFStore.registerConvention(NetCDFStore_GOTM)
NetCDFStore.registerConvention(NetCDFStore_MOM4)