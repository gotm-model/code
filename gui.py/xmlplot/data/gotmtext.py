import os, StringIO

import numpy

import xmlstore.xmlstore
import xmlplot.common

class LinkedFileVariableStore(xmlplot.common.VariableStore,xmlstore.datatypes.DataFileEx):

    # XML store-derived class for storing (cached) metadata of a data file,
    # such as coordinate ranges.
    # This is implemented as XML store (rather than Python object) because it
    # needs to be saved in a descriptive form along with the data files themselves.
    class DataFileCache(xmlstore.xmlstore.TypedStore):
        @classmethod
        def getSchemaInfo(cls):
            return xmlstore.xmlstore.schemainfocache[os.path.join(xmlplot.common.getDataRoot(),'schemas/datafilecache')]

        def __init__(self,valueroot=None,adddefault = True,schema=None):
            if schema is None: schema = os.path.join(xmlplot.common.getDataRoot(),'schemas/datafilecache/0001.schema')
            xmlstore.xmlstore.TypedStore.__init__(self,schema,valueroot,adddefault=adddefault)

    class LinkedFileVariable(xmlplot.common.Variable):

        def __init__(self,store,data,index):
            xmlplot.common.Variable.__init__(self,store)
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
        assert finfo is not None, 'Node "%s" lacks "fileinfo" attribute.' % node
        store = None
        type = finfo.getAttribute('type')
        if type=='pointsintime':
            store = LinkedMatrix(datafile,context,infonode,nodename,type=0,dimensions={'time':{'label':'time','datatype':'datetime','preferredaxis':'x'}},dimensionorder=('time',))
        elif type=='profilesintime':
            store = LinkedProfilesInTime(datafile,context,infonode,nodename,dimensions={'time':{'label':'time','datatype':'datetime','preferredaxis':'x'},'z':{'label':'depth','unit':'m','preferredaxis':'y'}},dimensionorder=('time','z'))
        elif type=='singleprofile':
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
    
        xmlplot.common.VariableStore.__init__(self)
        xmlstore.datatypes.DataFileEx.__init__(self,datafile,context,infonode,nodename)

        # Copy data from supplied dimensions and variables
        self.dimensions = {}
        for dimname,dimdata in dimensions.iteritems():
            self.dimensions[dimname] = xmlplot.common.VariableStore.getDimensionInfo_raw(self,None)
            self.dimensions[dimname].update(dimdata)
        self.vardata = list(variables)
        self.dimensionorder = list(dimensionorder)
        
        # Supplement dimensions and variables with information in
        # supplied XML node (if any)
        self.filename = defaultfilename
        if infonode is not None:
            finfo = xmlstore.util.findDescendantNode(infonode,['fileinfo'])
            self.filename = infonode.getAttribute('name')
            if finfo.hasAttribute('datatype'): datatype = finfo.getAttribute('datatype')

            # Get variables
            fvars = xmlstore.util.findDescendantNode(finfo,['filevariables'])
            if fvars is not None:
                for ch in fvars.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filevariable':
                        assert ch.hasAttribute('name'), '"name" attribute of filevariable is missing, label = %s.' % longname
                        name = ch.getAttribute('name')
                        unit = ch.getAttribute('unit')
                        if ch.hasAttribute('label'):
                            longname = ch.getAttribute('label')
                        else:
                            longname = name
                        self.vardata.append((name,longname,unit))

            # Get dimensions
            fdims = xmlstore.util.findDescendantNode(finfo,['filedimensions'])
            if fdims is not None:
                for ch in fdims.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filedimension':
                        dimdata = xmlplot.common.VariableStore.getDimensionInfo_raw(self,None)
                        assert ch.hasAttribute('name'), '"name" attribute of filedimension is missing, label = "%s".' % ch.getAttribute('label')
                        id = ch.getAttribute('name')
                        if ch.hasAttribute('label'):
                            dimdata['label'] = ch.getAttribute('label')
                        else:
                            dimdata['label'] = id
                        if ch.hasAttribute('unit'):          dimdata['unit']          = ch.getAttribute('unit')
                        if ch.hasAttribute('datatype'):      dimdata['datatype']      = ch.getAttribute('datatype')
                        if ch.hasAttribute('preferredaxis'): dimdata['preferredaxis'] = ch.getAttribute('preferredaxis')
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
        if self.data is None: return
        
        #print '%s - caching validation result and dimension boundaries.' % self.filename
        metadata = self.getMetaData()
        for dimname in self.getDimensionNames():
            dimnode = metadata['Dimensions'].getChildById('Dimension',id=dimname,create=True)
            assert dimnode is not None, 'Failed to create Dimension node for %s.' % dimname
            dimrange = self.calculateDimensionRange(dimname)
            if dimrange is None: continue
            minval,maxval = dimrange
            if self.getDimensionInfo_raw(dimname)['datatype']=='datetime':
                dimnode['IsTimeDimension'].setValue(True)
                dimnode['MinimumTime'].setValue(xmlplot.common.num2date(minval))
                dimnode['MaximumTime'].setValue(xmlplot.common.num2date(maxval))
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
        if self.data is None and (self.datafile is None or not self.datafile.isValid()): return None
        
        metadata = self.getMetaData()
        dimnode = metadata['Dimensions'].getChildById('Dimension',dimname)
        if dimnode is None:
            try:
                self.getData()
            except Exception,e:
                pass
            dimnode = metadata['Dimensions'].getChildById('Dimension',dimname)
            assert dimnode is not None, 'Cannot locate node for dimension %s in data file cache.' % dimname
            
        if metadata['Valid'].getValue()==False: return None

        #print '%s - using cached bounds for %s.' % (self.filename,dimname)
        if dimnode['IsTimeDimension'].getValue():
            minval = dimnode['MinimumTime'].getValue()
            maxval = dimnode['MaximumTime'].getValue()
        else:
            minval = dimnode['Minimum'].getValue()
            maxval = dimnode['Maximum'].getValue()
        if minval is None and maxval is None: return None
        return (minval,maxval)
            
    def hasExpensiveValidate(self):
        return True

    def validate(self,templatenode,callback=None):
        if self.data is None and (self.datafile is None or not self.datafile.isValid()): return False
        metadata = self.getMetaData()
        valid = metadata['Valid'].getValue()
        if valid is None:
            try:
                self.getData(callback=callback)
            except Exception,e:
                pass
            valid = metadata['Valid'].getValue()
            assert valid is not None, 'Information on validity of data file %s not in data file cache.' % self.filename
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
        if self.datafile is not None:
            self.datafile.saveToFile(path)
        else:
            f = open(path,'w')
            self.writeData(f,callback=callback)
            f.close()
            
    def getDataFile(self,callback=None):
        if self.datafile is None:
            assert self.data is not None, 'getDataFile called with both the data file and the data in memory are not set.'
        
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
        if self.data is None and self.datafile is not None:
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
        if self.datafile is None or not self.datafile.isValid(): return None

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
        """Load a data from a DataFile object with the number of lines listed on the first line.
        """
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
                    try:
                        refvals = map(int,(line[:4],line[5:7],line[8:10],line[11:13],line[14:16],line[17:19]))
                    except ValueError:
                        raise Exception('Line %i does not start with date and time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
                    dimvalue = xmlstore.util.dateTimeFromTuple(refvals)
                    if prevdate is not None and dimvalue<prevdate:
                        raise Exception('Line %i: observation time %s lies before previous observation time %s. Times should be increasing.' % (iline,xmlstore.util.formatDateTime(dimvalue),xmlstore.util.formatDateTime(prevdate)))
                    prevdate = dimvalue
                    dimvalue = xmlplot.common.date2num(dimvalue)
                
                    # Read variable values.
                    data = line[19:].split()
                else:
                    # Split line, convert values to floats and store first as coordinate.
                    data = map(float,line.split())
                    dimvalue = data.pop(0)
            else:
                data = map(float,line.split())

            if len(data)<varcount:
                raise Exception('Line %i contains only %i observations, where %i are expected (%s).' % (iline,len(data),varcount,', '.join([d[1] for d in self.vardata])))
            
            # Store time and values.
            if dimcount==1: dimvalues[irow] = dimvalue
            values[irow,:] = data[:varcount]
            
            # Inform caller about progress
            if callback is not None and iline%1000==0:
                progress = None
                if filesize is not None:
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
        """Load a data file with the number of lines not known in advance.
        """
        varcount = len(self.vardata)
        
        # Get the size of the file (in bytes, may be None if the size is not known)
        # This will be used in combination with the position of the file pointer to report progress.
        filesize = float(self.datafile.getSize())
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()
        
        # Get the data type to use for the dimension
        dimdatatype = self.dimensions[self.dimensionorder[0]]['datatype']
        
        # Size of one memory slab (roughly equivalent to 1 MB in memory)
        buffersize = 125000/(varcount+1)

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
            try:
                refvals = map(int,(line[:4],line[5:7],line[8:10],line[11:13],line[14:16],line[17:19]))
            except ValueError:
                raise Exception('Line %i does not start with date and time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
            curdate = xmlstore.util.dateTimeFromTuple(refvals)
            times[-1][ipos] = xmlplot.common.date2num(curdate)
            
            # Read values.
            data = line[19:].split()
            if len(data)<varcount:
                raise Exception('Line %i contains only %i observations, where %i are expected (%s).' % (iline,len(data),varcount,', '.join([d[1] for d in self.vardata])))
            values[-1][ipos,:] = map(float,data[:varcount])
            
            # Inform caller about progress
            if callback is not None and iline%1000==0:
                progress = None
                if filesize is not None:
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

    def writeData(self,target,callback=None,missing=''):
        """Writes the current data to a file-like object."""
        # Get number of dimensions and variables, and get shortcuts to the data.
        dimcount = len(self.dimensions)
        data = self.getData()
        if dimcount==1:
            # One coordinate dimension present; get the data type of that dimension.
            dimdata = data[0]
            dimtype = self.dimensions.values()[0]['datatype']
            dimisdate = (dimtype=='datetime')
            if dimisdate: dimdata = xmlplot.common.num2date(dimdata)
        varcount = len(self.vardata)
        vardata = data[-1]
        
        mask = hasattr(vardata,'_mask')
        
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
                if mask and vardata._mask[iline,ivar]:
                    target.write('\t%s' % missing)
                else:
                    target.write('\t%.12g' % vardata[iline,ivar])
            target.write('\n')
            if callback is not None and iline%1000==0:
                callback(float(iline)/vardata.shape[0],'wrote %i lines.' % iline)

class LinkedProfilesInTime(LinkedFileVariableStore):

    class LinkedProfilesInTimeVariable(LinkedFileVariableStore.LinkedFileVariable):
        def getSlice(self,bounds):
            varslice = self.Slice(self.getDimensions())

            data = self.store.getGriddedData()
            if data[0].shape[0]==0: return varslice

            timebounds = xmlplot.common.findIndices((bounds[0].start,bounds[0].stop),data[0])
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
        self.data = [numpy.empty((0,)),[],[]]
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
            dimmin,dimmax = None,None
            for curdata in dimdata:
                if 0 in curdata.shape: continue
                curmin,curmax = curdata.min(),curdata.max()
                if dimmin is None or curmin<dimmin: dimmin = curmin
                if dimmax is None or curmax>dimmax: dimmax = curmax
            return (dimmin,dimmax)
                
    def writeData(self,target,callback=None):
        """Writes the current data to a file-like object."""
        varcount = len(self.vardata)
        data = self.getData()
        assert data is not None, 'Cannot write data to file, because data is set to None.'
        times,depths,values = data
        for itime in range(times.shape[0]):
            target.write(xmlstore.util.formatDateTime(xmlplot.common.num2date(times[itime]),iso=True))
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
        if self.griddeddata is None:
            # Select only non-empty profiles
            times,depths,values = [],[],[]
            for t,d,v in zip(*data):
                if 0 not in d.shape:
                    times.append(t)
                    depths.append(d)
                    values.append(v)
            times = numpy.array(times,dtype=data[0].dtype)
            
            varcount = len(self.vardata)
            
            # Find unique depth levels.
            uniquedepths = set()
            for ds in depths:
                for d in ds: uniquedepths.add(d)
            
            # Create depth grid to interpolate on to. Use the observation depths if less than 200,
            # otherwise create a equidistant 200-point grid between the minimum and maximum depth.
            uniquedepths = list(uniquedepths)
            uniquedepths.sort()
            if len(uniquedepths)<200:
                depthdatatype = self.dimensions[self.dimensionorder[1]]['datatype']
                depthgrid = numpy.array(uniquedepths,self.mpldatatypes[depthdatatype])
            else:
                depthgrid = numpy.linspace(uniquedepths[0],uniquedepths[-1],200)
                
            # Grid observed profiles to depth grid.
            griddedvalues = numpy.empty((times.shape[0],depthgrid.shape[0],varcount),self.mpldatatypes[self.datatype])
            for it in range(len(times)):
                griddedvalues[it,:,:] = xmlplot.common.interp1(depths[it],values[it],depthgrid)
                if callback is not None and (it+1)%20==0:
                    callback(float(it+1)/len(times),'gridded %i profiles.' % (it+1))
                
            # Store time grid, depth grid and observations.
            self.griddeddata = (times,depthgrid,griddedvalues)
            
        return self.griddeddata

    def parseDataFile(self,callback=None):
        if self.datafile is None or not self.datafile.isValid(): return None
        
        varcount = len(self.vardata)
        
        # Get the size of the file (in bytes, may be None if the size is not known)
        # This will be used in combination with the position of the file pointer to report progress.
        filesize = float(self.datafile.getSize())
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()

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
            try:
                refvals = map(int,(line[:4],line[5:7],line[8:10],line[11:13],line[14:16],line[17:19]))
            except ValueError:
                raise Exception('Line %i does not start with date and time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
            curdate = xmlstore.util.dateTimeFromTuple(refvals)
            curdate = xmlplot.common.date2num(curdate)

            # Get the number of observations and the depth direction.
            (depthcount,updown) = map(int, line[19:].split())

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
                if callback is not None and iline%1000==0:
                    pos = f.tell()
                    callback(pos/filesize,'processed %i lines.' % iline)
                    
                # Read line
                line = f.readline()
                if line=='':
                    raise Exception('Premature end-of-file after line %i; expected %i more observations.' % (iline,depthcount-depthindices.index(idepthline)))
                iline += 1
                
                # Read values (depth followed by data) and check.
                try:
                    linedata = map(float,line.split())
                except ValueError,e:
                    raise Exception('Line %i: %s' % (iline,e))
                    
                if len(linedata)<varcount+1:
                    raise Exception('Line %i contains only %i value(s), where %i (1 depth and %i observations) are expected.' % (iline,len(linedata),varcount+1,varcount))
                if prevdepth is not None:
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
            if callback is not None and iline%1000==0:
                pos = f.tell()
                callback(pos/filesize,'processed %i lines.' % iline)
                
        # Convert sequence with times to numpy array.
        timedatatype = self.dimensions[self.dimensionorder[0]]['datatype']
        times = numpy.array(times,self.mpldatatypes[timedatatype])
        
        # Close data file
        f.close()

        # Succeeded in reading the data: store them internally.
        return [times,depths,values]
