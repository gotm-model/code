import os, re, datetime, xml.dom.minidom, tempfile, shutil, StringIO, math

import common, xmlstore, scenario

# Import NetCDF file format support
from pynetcdf import NetCDFFile
#from Scientific.IO.NetCDF import NetCDFFile
import matplotlib.numerix, numpy, pytz

# Abstract class that contains one or more variables that can be plotted.
# Classes deriving from it must support the virtual methods below.
class PlotVariableStore:

    def __init__(self):
        pass

    def getVariableNames(self):
        return []

    def getVariableLongNames(self):
        return dict([(name,self.getVariable(name).getLongName()) for name in self.getVariableNames()])

    def getVariable(self,varname):
        return None
        
    def getDimensionInfo(self,dimname):
        return {'label':'','unit':'','preferredaxis':None,'datatype':'float','logscale':False}

    def getVariableTree(self,path,otherstores={}):
        xmlschema = xml.dom.minidom.parse(path)
        vardict = self.getVariableLongNames()
        found = set(self.filterNodes(xmlschema.documentElement,vardict))
        remaining = [nodename for nodename in vardict if nodename not in found]
        other = None
        for ch in xmlschema.getElementsByTagName('element'):
            if ch.getAttribute('name')=='other':
                other = ch
                break
        if other!=None:
            if len(remaining)==0:
                other.parentNode.removeChild(other)
            else:
                for varid in sorted(remaining,cmp=lambda x,y: cmp(vardict[x].lower(), vardict[y].lower())):
                    el = xmlschema.createElement('element')
                    el.setAttribute('name',varid)
                    el.setAttribute('label',vardict[varid])
                    el.setAttribute('type','bool')
                    other.appendChild(el)
        return xmlstore.TypedStore(xmlschema,otherstores=otherstores)

    def filterNodes(self,node,vardict):
        nodeid = node.getAttribute('name')
        assert nodeid!='', 'Node lacks "name" attribute.'
        nodeids = []
        if nodeid in vardict:
            if not node.hasAttribute('label'):
                node.setAttribute('label',vardict[nodeid])
            node.setAttribute('type','bool')
            nodeids.append(nodeid)
        for ch in common.findDescendantNodes(node,['element']):
            nodeids += self.filterNodes(ch,vardict)
        if len(nodeids)==0 and nodeid!='other':
            node.parentNode.removeChild(node)
        return nodeids

# Abstract class that represents a variable that can be plotted.
# Classes deriving from it must support the virtual methods below.
class PlotVariable:
    
    class Slice:
        def __init__(self,dimensions=()):
            self.dimensions = dimensions
            self.ndim = len(dimensions)
            self.data = None
            self.coords = self.ndim*[None]
            self.coords_stag = self.ndim*[None]
            
            # Bounds for confidence interval (optional)
            self.lbound = None
            self.ubound = None
        
        def isValid(self):
            return (self.ndim>0) and (self.data!=None) and (None not in self.coords) and (None not in self.coords_stag)

        def generateStaggered(self):
            for idim in range(self.ndim):
                assert self.coords[idim]!=None, 'Cannot generate staggered coordinates because centered coordinates have not been set.'
                assert self.coords[idim].ndim==1, 'Currently a staggered grid can only be generated automatically for 1D coordinate vectors.'
                self.coords_stag[idim] = common.getCenters(self.coords[idim],addends=True)
                
        def squeeze(self):
            # Find non-singleton dimensions (singleton dimension: dimension with length one)
            # Store singleton dimensions as fixed extra coordinates.
            gooddimindices = []
            gooddimnames = []
            self.fixedcoords = []
            for idim,dimname in enumerate(self.dimensions):
                if self.data.shape[idim]>1:
                    # Normal dimension (more than one coordinate)
                    gooddimindices.append(idim)
                    gooddimnames.append(dimname)
                elif self.data.shape[idim]==1:
                    # Singleton dimension
                    self.fixedcoords.append((dimname,self.coords[idim][0]))

            newslice = PlotVariable.Slice(gooddimnames)
            newslice.coords      = [self.coords     [i].squeeze() for i in gooddimindices]
            newslice.coords_stag = [self.coords_stag[i].squeeze() for i in gooddimindices]
            newslice.data = self.data.squeeze()

            # Update confidence interval (if any)
            if self.lbound!=None: newslice.lbound = self.lbound.squeeze()
            if self.ubound!=None: newslice.ubound = self.ubound.squeeze()

            return newslice

    def __init__(self,store):
        self.store = store

    # getName()
    #   Return type: string
    #   Returns the short name (or identifier) of the variable.
    def getName(self):
        return ''

    # getLongName()
    #   Return type: string
    #   Returns the pretty name for the variable.
    def getLongName(self):
        return ''

    # getUnit()
    #   Return type: string
    #   Returns the unit of the variable.
    def getUnit(self):
        return ''

    # getDimensions()
    #   Return type: tuple of strings
    #   Returns the names of the dimensions of the variable; currently supported dimensions: "time", "z".
    def getDimensions(self):
        return ()

    def getSlice(self,bounds):
        return self.Slice()

    def getDimensionInfo(self,dimname):
        return self.store.getDimensionInfo(dimname)

class LinkedFileVariableStore(PlotVariableStore):

    class LinkedFileVariable(PlotVariable):

        def __init__(self,store,data,index):
            self.store = store
            self.data = data
            self.index = index

        def getName(self):
            return self.data[0]

        def getLongName(self):
            return self.data[1]

        def getUnit(self):
            return self.data[2]

        def getDimensions(self):
            return self.store.dimensionorder[:]

        def getSlice(self,bounds):
            assert False, 'This function must be implemented by inheriting class.'
            
    @staticmethod
    def fromNode(node):
        finfo = common.findDescendantNode(node.templatenode,['fileinfo'])
        assert finfo!=None, 'Node "%s" lacks "fileinfo" attribute.' % node
        type = finfo.getAttribute('type')
        if type=='pointsintime':
            return LinkedMatrix(node,type=0,dimensions={'time':{'label':'time','datatype':'datetime','preferredaxis':'x'}},dimensionorder=('time',))
        elif type=='profilesintime':
            return LinkedProfilesInTime(node,dimensions={'time':{'label':'time','datatype':'datetime','preferredaxis':'x'},'z':{'label':'depth','unit':'m','preferredaxis':'y'}},dimensionorder=('time','z'))
        elif type=='singleprofile' or type=='verticalgrid':
            return LinkedMatrix(node,type=1)
        else:
            assert False, 'Linked file has unknown type "%s".' % node.type

    def __init__(self,node,dimensions={},dimensionorder=()):

        finfo = common.findDescendantNode(node.templatenode,['fileinfo'])
        self.nodeid = node.getId()
        
        self.vardata = []
        fvars = common.findDescendantNode(finfo,['filevariables'])
        if fvars!=None:
            for ch in fvars.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filevariable':
                    longname = ch.getAttribute('label')
                    unit = ch.getAttribute('unit')
                    name = longname
                    self.vardata.append([name,longname,unit])
                    
        # Copy data from supplied dimensions
        self.dimensions = {}
        for dimname,dimdata in dimensions.iteritems():
            self.dimensions[dimname] = PlotVariableStore.getDimensionInfo(self,None)
            self.dimensions[dimname].update(dimdata)

        self.dimensionorder = list(dimensionorder)
        
        fdims = common.findDescendantNode(finfo,['filedimensions'])
        if fdims!=None:
            for ch in fdims.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filedimension':
                    dimdata = PlotVariableStore.getDimensionInfo(self,None)
                    if ch.hasAttribute('label'):         dimdata['label']         = ch.getAttribute('label')
                    if ch.hasAttribute('unit'):          dimdata['unit']          = ch.getAttribute('unit')
                    if ch.hasAttribute('datatype'):      dimdata['datatype']      = ch.getAttribute('datatype')
                    if ch.hasAttribute('preferredaxis'): dimdata['preferredaxis'] = ch.getAttribute('preferredaxis')
                    id = ch.getAttribute('name')
                    if id=='': id = dimdata['label']
                    self.dimensions[id] = dimdata
                    self.dimensionorder.append(id)

        self.datafile = None
        self.clear()
        
    def clear(self):
        pass
        
    def getDimensionNames(self):
        return self.dimensionorder[:]
        
    def getDimensionInfo(self,dimname):
        return self.dimensions[dimname]
        
    def getDimensionRange(self,dimname):
        return None
        
    def getVariableNames(self):
        return [data[0] for data in self.vardata]

    def getVariableLongNames(self):
        return dict([(data[0],data[1]) for data in self.vardata])

    def getVariable(self,varname):
        for (index,data) in enumerate(self.vardata):
            if data[0]==varname:
                return self.variableclass(self,data,index)
        assert False, 'Variable with name "%s" not found in store.' % varname
        
    def dataChanged(self):
        """Event handler, must be called by external actors when they change the data."""
        self.datafile = None
        
    def saveToFile(self,path):
        """Saves the current data to file."""
        if self.datafile!=None:
            self.datafile.saveToFile(path)
        else:
            f = open(path,'w')
            self.writeData(f)
            f.close()
            
    def getAsDataFile(self):
        if self.datafile!=None:
            return self.datafile.addref()
        else:
            target = StringIO.StringIO()
            self.writeData(target)
            df = xmlstore.DataFileMemory(target.getvalue(),self.nodeid+'.dat')
            target.close()
            return df
        
    def writeData(self,target):
        """Writes the current data to a file-like object."""
        assert False, 'This function must be implemented by inheriting class.'
        
    def getGriddedData(self,callback=None):
        assert self.data!=None, 'Data not set.'
        return self.data

    def loadDataFile(self,datafile,callback=None):
        assert False, 'This function must be implemented by inheriting class.'



class LinkedMatrix(LinkedFileVariableStore):

    class LinkedMatrixVariable(LinkedFileVariableStore.LinkedFileVariable):
        def getSlice(self,bounds):
            slice = self.Slice(self.getDimensions())
            
            # Get a reference to all data, and stop if the coordinate dimension is empty.
            data = self.store.getGriddedData()
            if data[0].shape[0]==0: return slice

            if slice.ndim==1:
                slice.coords[0] = data[0][:]
            slice.data = data[-1][:,self.index]
            slice.generateStaggered()
            return slice

    def __init__(self,node,type=0,dimensions={},dimensionorder=()):
        LinkedFileVariableStore.__init__(self,node,dimensions,dimensionorder)
        self.variableclass = self.LinkedMatrixVariable
        assert len(self.dimensions)<=1, 'Linkedmatrix objects can only be used with 0 or 1 coordinate dimensions, but %i are present.' % len(self.dimensions)
        self.type = type
        
    def clear(self):
        """Clears all contained data."""
        self.data = []
        if len(self.dimensions)==1:
            self.data.append(matplotlib.numerix.empty((0,)))
        self.data.append(matplotlib.numerix.empty((0,len(self.vardata))))
        
    def getDimensionRange(self,dimname):
        ind = self.dimensionorder.index(dimname)
        dimdata = self.data[ind]
        if 0 in dimdata.shape: return None
        return (dimdata.min(),dimdata.max())

    def loadDataFile(self,datafile,callback=None):
        self.datafile = datafile
        
        if not self.datafile.isValid():
            # No data: empty the store and return
            self.clear()
            return

        if self.type==0:
            # Unknown number of rows
            res = self.loadDataFile_UnknownCount(datafile,callback)
        elif self.type==1:
            # Known number of rows
            res = self.loadDataFile_KnownCount(datafile,callback)
        else:
            assert False, 'unknown LinkedMatrix type %i.' % self.type
            
        return res
        
    def loadDataFile_KnownCount(self,datafile,callback):
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
        values = matplotlib.numerix.empty((obscount,varcount),matplotlib.numerix.Float32)
        if dimcount==1:
            # One coordinate dimension present; allocate an array for its values.
            dimtype = self.dimensions.values()[0]['datatype']
            dimisdate = (dimtype=='datetime')
            if dimisdate:
                datetimere = re.compile('(\d\d\d\d).(\d\d).(\d\d) (\d\d).(\d\d).(\d\d)')
                dimvalues = matplotlib.numerix.empty((obscount,),matplotlib.numerix.Float64)
            else:
                dimvalues = matplotlib.numerix.empty((obscount,),matplotlib.numerix.Float32)
            self.data = [dimvalues,values]
        else:
            self.data = [values]

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
                    dimvalue = common.dateTimeFromTuple(refvals)
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
                callback('processed %i lines.' % iline,progress=progress)
            
        # Close data file
        f.close()

    def loadDataFile_UnknownCount(self,datafile,callback):
        varcount = len(self.vardata)
        
        # Get the size of the file (in bytes, may be None if the size is not known)
        # This will be used in combination with the position of the file pointer to report progress.
        filesize = float(self.datafile.getSize())
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()

        # Compile regular expression for reading dates.
        datetimere = re.compile('(\d\d\d\d).(\d\d).(\d\d) (\d\d).(\d\d).(\d\d)')
        
        buffersize = 1000

        times = []
        values = []
        iline = 0
        ipos = 0
        while True:
            # Read a line (stop if end-of-file was reached)
            line = f.readline()
            if line=='': break

            # Calculate position in current slice, create new data slice if needed.
            ipos = iline%buffersize
            if ipos==0:
                times.append(matplotlib.numerix.empty((buffersize,),matplotlib.numerix.Float64))
                values.append(matplotlib.numerix.empty((buffersize,varcount),matplotlib.numerix.Float32))

            # Increment current line number
            iline += 1
            
            # Read the date + time
            datematch = datetimere.match(line)
            if datematch==None:
                raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
            refvals = map(int,datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
            curdate = common.dateTimeFromTuple(refvals)
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
                callback('processed %i lines.' % iline,progress=progress)

        # Delete unused rows in last data slice.
        times [-1] = times [-1][0:iline%buffersize]
        values[-1] = values[-1][0:iline%buffersize,:]
        
        # Concatenate data slices.
        times = matplotlib.numerix.concatenate(times,axis=0)
        values = matplotlib.numerix.concatenate(values,axis=0)
        self.data = [times,values]
            
        # Close data file
        f.close()

    def writeData(self,target):
        """Writes the current data to a file-like object."""
        # Get number of dimensions and variables, and get shortcuts to the data.
        dimcount = len(self.dimensions)
        if dimcount==1:
            # One coordinate dimension present; get the data type of that dimension.
            dimdata = self.data[0]
            dimtype = self.dimensions.values()[0]['datatype']
            dimisdate = (dimtype=='datetime')
        varcount = len(self.vardata)
        vardata = self.data[-1]
        
        if self.type==1:
            # Write first line with number of observations.
            target.write('%i\n' % vardata.shape[0])
        
        # Write lines with observations.
        for iline in range(vardata.shape[0]):
            if dimcount==1:
                if dimisdate:
                    target.write(common.num2date(dimdata[iline]).strftime('%Y-%m-%d %H:%M:%S'))
                else:
                    target.write('%.9g' % dimdata[iline])
            for ivar in range(varcount):
                target.write('\t%.9g' % vardata[iline,ivar])
            target.write('\n')

class LinkedProfilesInTime(LinkedFileVariableStore):

    class LinkedProfilesInTimeVariable(LinkedFileVariableStore.LinkedFileVariable):
        def getSlice(self,bounds):
            varslice = self.Slice(self.getDimensions())

            data = self.store.getGriddedData()
            if data[0].shape[0]==0: return varslice

            timebounds = common.findIndices(bounds[0],data[0])
            varslice.coords[0] = data[0][timebounds[0]:timebounds[1]+1]
            varslice.coords[1] = data[1]
            varslice.data = data[2][timebounds[0]:timebounds[1]+1,:,self.index]
            varslice.generateStaggered()
                    
            return varslice

    def __init__(self,node,dimensions=[],dimensionorder=()):
        LinkedFileVariableStore.__init__(self,node,dimensions,dimensionorder)
        self.variableclass = self.LinkedProfilesInTimeVariable
        
    def clear(self):
        self.data = (matplotlib.numerix.empty((0,)),[],[])
        self.griddeddata = None

    def getDimensionRange(self,dimname):
        ind = self.dimensionorder.index(dimname)
        dimdata = self.data[ind]
        if len(dimdata)==0: return None
        if ind==0:
            return (dimdata.min(),dimdata.max())
        else:
            dimmin,dimmax = dimdata[0].min(),dimdata[0].max()
            for iobs in range(1,len(dimdata)):
                dimmin = min(dimmin,dimdata[iobs].min())
                dimmax = max(dimmin,dimdata[iobs].max())
            return (dimmin,dimmax)
        
    def dataChanged(self):
        """Event handler, must be called by external actors when they change the data."""
        LinkedFileVariableStore.dataChanged(self)
        self.griddeddata = None
        
    def writeData(self,target):
        """Writes the current data to a file-like object."""
        varcount = len(self.vardata)
        times = self.data[0]
        depths = self.data[1]
        data = self.data[2]
        for itime in range(times.shape[0]):
            target.write(common.num2date(times[itime]).strftime('%Y-%m-%d %H:%M:%S'))
            curdepths = depths[itime]
            curdata = data[itime]
            depthcount = len(curdepths)
            target.write('\t%i\t1\n' % depthcount)
            for idepth in range(depthcount):
                target.write('%.9g' % curdepths[idepth])
                for ivar in range(varcount):
                    target.write('\t%.9g' % curdata[idepth,ivar])
                target.write('\n')
        
    def getGriddedData(self,callback=None):
        assert self.data!=None, 'Data not set.'
        if self.griddeddata==None:
            (times,depths,values) = self.data
            
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
                depthgrid = matplotlib.numerix.array(uniquedepths,matplotlib.numerix.Float32)
            else:
                depthgrid = numpy.linspace(uniquedepths[0],uniquedepths[-1],200)
                
            # Grid observed profiles to depth grid.
            griddedvalues = matplotlib.numerix.empty((times.shape[0],depthgrid.shape[0],varcount),matplotlib.numerix.Float32)
            for it in range(len(times)):
                griddedvalues[it,:,:] = common.interp1(depths[it],values[it],depthgrid)
                if callback!=None and (it+1)%20==0:
                    callback('gridded %i profiles.' % (it+1),progress=float(it+1)/len(times))
                
            # Store time grid, depth grid and observations.
            self.griddeddata = (times,depthgrid,griddedvalues)
            
        return self.griddeddata

    def loadDataFile(self,datafile,callback=None):
        self.datafile = datafile
        
        if not self.datafile.isValid():
            self.clear()
            return
        
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
            curdate = common.dateTimeFromTuple(refvals)
            curdate = common.date2num(curdate)

            # Get the number of observations and the depth direction.
            (depthcount,updown) = map(int, line[datematch.end()+1:].split())

            # Create arrays that will contains depths and observed values.
            curdepths = matplotlib.numerix.empty((depthcount,),matplotlib.numerix.Float32)
            curvalues = matplotlib.numerix.empty((depthcount,varcount),matplotlib.numerix.Float32)
            
            # Depths can be increasing (updown==1) or decreasing (updown!=1)
            if updown==1:
                depthindices = range(0,depthcount,1)
            else:
                depthindices = range(depthcount-1,-1,-1)
            
            # Now parse the specified number of observations to cretae the profiles.
            prevdepth = None
            for idepthline in depthindices:
                if callback!=None and iline%1000==0:
                    pos = f.tell()
                    callback('processed %i lines.' % iline,progress=pos/filesize)
                    
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
                callback('processed %i lines.' % iline,progress=pos/filesize)
                
        # Convert sequence with times to numpy array.
        times = matplotlib.numerix.array(times,matplotlib.numerix.Float64)
        
        self.data = [times,depths,values]
        self.griddeddata = None
            
        # Close data file
        f.close()

class NetCDFStore(PlotVariableStore,common.referencedobject):
    
    class NetCDFVariable(PlotVariable):
        def __init__(self,store,varname):
            PlotVariable.__init__(self,store)
            self.varname = str(varname)

        def getName(self):
            return self.varname

        def getLongName(self):
            nc = self.store.getcdf()
            var = nc.variables[self.varname]
            if hasattr(var,'long_name'):
                return var.long_name
            else:
                return self.getName()

        def getUnit(self):
            nc = self.store.getcdf()
            cdfvar = nc.variables[self.varname]
            if not hasattr(cdfvar,'units'): return None
            return common.convertUnitToUnicode(cdfvar.units)
            
        def getDimensions(self):
          nc = self.store.getcdf()
          return nc.variables[self.varname].dimensions

        def getSlice(self,bounds):
          nc = self.store.getcdf()
            
          v = nc.variables[self.varname]
          dimnames = v.dimensions
          assert len(bounds)==len(dimnames), 'Number of specified bounds (%i) does not match number of dimensions (%i).' % (len(bounds),len(dimnames))
          
          varslice = self.Slice(dimnames)
          
          boundindices = []
          
          for idim,dimname in enumerate(dimnames):
            if dimname=='z' or dimname=='z1':
                # Get depth coordinates and bounds.
                (z,z1,z_stag,z1_stag) = self.store.getDepth()
                depthbounds = (0,z.shape[1])
                timebounds = boundindices[list(dimnames).index('time')]
                if dimname=='z':
                    varslice.coords     [idim] = z     [timebounds[0]:timebounds[1]+1,depthbounds[0]:depthbounds[1]+1]
                    varslice.coords_stag[idim] = z_stag[timebounds[0]:timebounds[1]+2,depthbounds[0]:depthbounds[1]+2]
                elif dimname=='z1':
                    varslice.coords     [idim] = z1     [timebounds[0]:timebounds[1]+1,depthbounds[0]+1:depthbounds[1]+2]
                    varslice.coords_stag[idim] = z1_stag[timebounds[0]:timebounds[1]+2,depthbounds[0]  :depthbounds[1]+2]
                boundindices.append(depthbounds)
            else:
                # Get coordinates and bounds of this dimension.
                (coords,coords_stag) = self.store.getCoordinates(dimname)
                if coords==None: return None
                curbounds = common.findIndices(bounds[idim],coords)
                varslice.coords     [idim] = coords     [curbounds[0]:curbounds[1]+1]
                varslice.coords_stag[idim] = coords_stag[curbounds[0]:curbounds[1]+2]
                boundindices.append(curbounds)

          try:
            dat = v[tuple([slice(b[0],b[1]+1) for b in boundindices])]
          except Exception, e:
            raise Exception('Unable to read values for NetCDF variable "%s". Error: %s' % (self.varname,str(e)))

        # Process COARDS variable attributes.
          if hasattr(v,'missing_value'):
            dat = matplotlib.numerix.ma.masked_array(dat,dat==v.missing_value)
          if hasattr(v,'scale_factor'):
            dat *= v.scale_factor
          if hasattr(v,'add_offset'):
            dat += v.add_offset

          varslice.data = dat
        
          return varslice

    def __init__(self,path=None):
        common.referencedobject.__init__(self)
        PlotVariableStore.__init__(self)
        
        self.datafile = None
        self.nc = None

        self.cachedcoords = {}
        
        if path!=None: self.load(path)
        
    def __str__(self):
        return self.datafile
        
    def getDimensionInfo(self,dimname):
        res = PlotVariableStore.getDimensionInfo(self,dimname)
        varinfo = self.nc.variables[dimname]
        if hasattr(varinfo,'long_name'):
            res['label'] = varinfo.long_name
        else:
            res['label'] = dimname
        if hasattr(varinfo,'units'):     res['unit']  = varinfo.units
        if dimname=='z' or dimname=='z1':
            res['label'] = 'depth'
            res['preferredaxis'] = 'y'
            if res['unit']=='meters': res['unit']='m'
        elif self.isTimeDimension(dimname):
            res['datatype'] = 'datetime'
            res['preferredaxis'] = 'x'
            res['unit'] = ''
        return res
        
    def save(self,path):
        shutil.copyfile(self.datafile,path)

    def unlink(self):
        if self.nc!=None:
            # Close NetCDF result file.
            self.nc.close()
            self.nc = None
            
    def load(self,path):
        # Store link to result file, and try to open the CDF file
        self.datafile = path
        self.getcdf()

    def getcdf(self):
        if self.nc!=None: return self.nc
        assert self.datafile!=None, 'The result object has not yet been attached to an actual result.'
        try:
            self.nc = NetCDFFile(self.datafile)
        except Exception, e:
            raise Exception('An error occured while opening the NetCDF file "%s": %s' % (self.datafile,str(e)))
        return self.nc

    def getVariableNames(self):
        nc = self.getcdf()

        # Get names of NetCDF variables
        try:
          varNames = nc.variables.keys()
        except Exception, e:
            raise Exception('Unable to obtain NetCDF variable names, error: '+str(e))

        return varNames

    def getVariableLongNames(self):
      varnames = self.getVariableNames()
      nc = self.getcdf()
      vardict = {}
      for varname in varnames:
          vardict[varname] = nc.variables[varname].long_name
      return vardict

    def getVariable(self,varname,check=True):
        varname = str(varname)
        if check:
            nc = self.getcdf()
            vars = nc.variables
            if not (varname in vars): return None
        return self.NetCDFVariable(self,varname)
        
    def getCoordinates(self,dimname):
        if dimname not in self.cachedcoords:
            nc = self.getcdf()
            coords = nc.variables[dimname][:]
            
            if 0 in coords.shape:
                coords = None
                coords_stag = None
            else:
                assert len(coords.shape)==1, 'Currently only independent dimensions (coordinates of which do no vary along other dimensions) can be used.'
                
                istimedim = self.isTimeDimension(dimname)
                if istimedim:
                    timeunit,timeref = self.getTimeReference(dimname)
                    timeref = common.date2num(timeref)
                    coords = timeref+timeunit*matplotlib.numerix.asarray(coords,matplotlib.numerix.Float64)
                
                coords_stag = matplotlib.numerix.zeros((coords.shape[0]+1,),matplotlib.numerix.typecode(coords))
                if coords.shape[0]==1:
                    # Only one coordinate provided; use default step of one.
                    delta = 1.
                    if istimedim:
                        if self.scenario!=None:
                            delta = self.scenario['output/dtsave'].getValue(usedefault=True)
                            if delta!=None: delta = delta.getAsSeconds()/86400.
                        if delta==None:
                            if coords[0]>timeref:
                                delta=coords[0]-timeref
                            else:
                                delta=1.
                    coords_stag[0] = coords[0]-delta/2
                    coords_stag[1] = coords[0]+delta/2
                else:
                    coords_stag[1:-1] = coords[0:-1] + (coords[1:]-coords[0:-1])/2
                    coords_stag[0 ] = coords[0]  - (coords[ 1]-coords[ 0])/2
                    coords_stag[-1] = coords[-1] + (coords[-1]-coords[-2])/2

            self.cachedcoords[dimname]         = coords
            self.cachedcoords[dimname+'_stag'] = coords_stag

        return (self.cachedcoords[dimname],self.cachedcoords[dimname+'_stag'])

    def getDepth(self):
        if 'z' not in self.cachedcoords:
            nc = self.getcdf()

            # Get layer heights
            h = nc.variables['h'][:,:,0,0]
            
            # Get depths of interfaces
            z1 = h.cumsum(1)
            z1 = matplotlib.numerix.concatenate((matplotlib.numerix.zeros((z1.shape[0],1),matplotlib.numerix.typecode(z1)),z1),1)
            bottomdepth = z1[0,-1]-nc.variables['zeta'][0,0,0]
            z1 -= bottomdepth

            # Get depth of layer centers
            z = z1[:,1:z1.shape[1]]-0.5*h

            # Interpolate in depth to create staggered grid
            z1_med = matplotlib.numerix.concatenate((matplotlib.numerix.take(z1,(0,),0),z1,matplotlib.numerix.take(z1,(-1,),0)),0)
            z_stag = 0.5 * (z1_med[0:z1_med.shape[0]-1,:] + z1_med[1:z1_med.shape[0],:])
            
            z_med = matplotlib.numerix.concatenate((z,matplotlib.numerix.take(z1,(-1,),1)),1)
            z_med = matplotlib.numerix.concatenate((matplotlib.numerix.take(z_med,(0,),0),z_med,matplotlib.numerix.take(z_med,(-1,),0)),0)
            z1_stag = 0.5 * (z_med[0:z_med.shape[0]-1,:] + z_med[1:z_med.shape[0],:])
            
            z.shape = list(z.shape)+[1,1]
            z1.shape = list(z1.shape)+[1,1]
            z_stag.shape = list(z_stag.shape)+[1,1]
            z1_stag.shape = list(z1_stag.shape)+[1,1]

            self.cachedcoords['z']       = z
            self.cachedcoords['z1']      = z1
            self.cachedcoords['z_stag']  = z_stag
            self.cachedcoords['z1_stag'] = z1_stag

        return (self.cachedcoords['z'],self.cachedcoords['z1'],self.cachedcoords['z_stag'],self.cachedcoords['z1_stag'])
        
    class ReferenceTimeParseError(Exception):
        def __init__(self,error):
            Exception.__init__(self,error)
        
    def isTimeDimension(self,dimname):
        # See if specified dimension is a time dimension according to COARDS convention.
        try:
            timeunit,timeref = self.getTimeReference(dimname)
        except self.ReferenceTimeParseError:
            return False
        return True

    def getTimeReference(self,dimname):
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
      hour,min,sec = 0,0,0
      reftime = reftime[datematch.end():]
      if len(reftime)>0:
        timematch = re.match(r'(\d{1,2}):(\d{1,2}):(\d{1,2}(?:\.\d*)?)\s*',reftime)
        if timematch==None:
            raise self.ReferenceTimeParseError('"units" attribute of variable "time" equals "%s", which does not follow COARDS convention. Problem: cannot parse time in "%s".' % (fullunit,reftime))
        hour,min,sec = map(int,timematch.group(1,2,3))
        reftime = reftime[timematch.end():]
      dateref = datetime.datetime(year,month,day,hour,min,sec,tzinfo=common.utc)
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

# Class that represents a GOTM result.
#   Inherits from PlotVariableStore, as it contains variables that can be plotted.
#   Contains a link to the scenario from which the result was created (if available)
class Result(NetCDFStore):

    schemadirname = 'schemas/result'
    @staticmethod
    def setRoot(rootpath):
        Result.schemadirname = os.path.join(rootpath,'schemas/result')

    def __init__(self):
        NetCDFStore.__init__(self)
        
        self.scenario = None
        self.tempdir = None
        self.changed = False
        
        self.stdout = None
        self.stderr = None
        self.returncode = 0
        self.errormessage = None
        
        self.store = xmlstore.TypedStore(os.path.join(Result.schemadirname,'gotmgui.xml'))
        self.wantedscenarioversion = scenario.guiscenarioversion
        
        self.path = None

    def hasChanged(self):
        return self.changed or self.store.changed

    def getTempDir(self,empty=False):
        if self.tempdir!=None:
            if empty:
                for f in os.listdir(self.tempdir): 
                    os.remove(os.path.join(self.tempdir,f))
        else:
            self.tempdir = tempfile.mkdtemp('','gotm-')
            print 'Created temporary result directory "%s".' % self.tempdir
        return self.tempdir
        
    def saveNetCDF(self,path):
        NetCDFStore.save(self,path)

    def save(self,path,addfiguresettings=True):
        assert self.datafile!=None, 'The result object was not yet attached to a result file (NetCDF).'

        # Create a ZIP container to hold the result.
        container = xmlstore.DataContainerZip(path,'w')

        if not addfiguresettings:
            # First clear all figure settings.
            self.store['FigureSettings'].clearValue(recursive=True)

        # Add the XML file describing result properties.            
        df = xmlstore.DataFileXmlNode(self.store.root.valuenode)
        df_added = container.addItem(df,'result.xml')
        df_added.release()
        df.release()
        self.store.resetChanged()

        # If we have a link to the scenario, add it to the result file.
        if self.scenario!=None:
            fscen = StringIO.StringIO()
            self.scenario.saveAll(fscen,claim=False)
            df = xmlstore.DataFileMemory(fscen.getvalue(),'scenario.gotmscenario')
            fscen.close()
            container.addItem(df)
            df.release()
        
        # Add the result data (NetCDF)
        container.addFile(self.datafile,'result.nc')
        
        # Make changes to container persistent (this closes the ZIP file), and release it.
        container.persistChanges()
        container.release()

        # Saved all changes; reset "changed" state
        self.changed = False
        
        # Store the path we saved to.
        self.path = path

    @classmethod
    def canBeOpened(cls, container):
        assert isinstance(container,xmlstore.DataContainer), 'Argument must be data container object.'
        filelist = container.listFiles()
        return ('result.nc' in filelist and 'scenario.gotmscenario' in filelist)

    def load(self,path):
        if isinstance(path,basestring):
            container = xmlstore.DataContainer.fromPath(path)
        elif isinstance(path,xmlstore.DataContainer):
            container = path.addref()

        files = container.listFiles()
        if 'scenario.gotmscenario' not in files:
            raise Exception('The archive "%s" does not contain "scenario.gotmscenario"; it cannot be a GOTM result.' % path)
        if 'result.nc' not in files:
            raise Exception('The archive "%s" does not contain "result.nc"; it cannot be a GOTM result.' % path)

        # Create a temporary directory to which we can unpack the archive.
        tempdir = self.getTempDir()

        df = container.getItem('scenario.gotmscenario')
        self.scenario = scenario.Scenario.fromSchemaName(self.wantedscenarioversion)
        self.scenario.loadAll(df)
        df.release()

        df = container.getItem('result.nc')
        resultpath = os.path.join(tempdir,'result.nc')
        df.saveToFile(resultpath)
        df.release()

        df = container.getItem('result.xml')
        if df!=None:
            f = df.getAsReadOnlyFile()
            valuedom = xml.dom.minidom.parse(f)
            self.store.setStore(valuedom)
            f.close()
            df.release()

        # Close the archive
        container.release()

        # Attach the result, try to open the CDF file
        self.datafile = resultpath
        self.getcdf()

        # Reset "changed" status.
        self.changed = False
        
        # Store path
        if isinstance(path,basestring):
            self.path = path
        else:
            self.path = None

    def setFigure(self,name,source):
        setroot = self.store['FigureSettings']
        fignodename = source.root.templatenode.getAttribute('name')
        fig = setroot.getChildById(fignodename,name,create=True)
        fig.copyFrom(source.root,replace=True)

    def getFigure(self,name,target):
        setroot = self.store['FigureSettings']
        fignodename = target.root.templatenode.getAttribute('name')
        fig = setroot.getChildById(fignodename,name,create=False)
        if fig!=None:
            target.root.copyFrom(fig,replace=True)
            return True
        return False

    def unlink(self):
        NetCDFStore.unlink(self)
        if self.tempdir!=None:
            # Delete temporary directory.
            print 'Deleting temporary result directory "%s".' % self.tempdir
            shutil.rmtree(self.tempdir)
            self.tempdir = None
        if self.scenario!=None:
            self.scenario.release()
        self.store.release()
            
    def attach(self,srcpath,scenario=None,copy=True):
        if scenario!=None:
            self.scenario = scenario.convert(self.wantedscenarioversion)
        else:
            self.scenario = None
            
        if copy:
            # Create a copy of the result file.
            tempdir = self.getTempDir(empty=True)
            datafile = os.path.join(tempdir,'result.nc')
            shutil.copyfile(srcpath,datafile)
        else:
            datafile = srcpath

        NetCDFStore.load(self,datafile)

        # Attached to an existing result: we consider it unchanged.
        self.changed = False
        
        if self.scenario!=None and self.scenario.path!=None and self.scenario.path.endswith('.gotmscenario'):
            self.path = self.scenario.path[:-12]+'gotmresult'
        else:
            self.path = None

    def getVariableNames(self,plotableonly=True):
        names = NetCDFStore.getVariableNames(self)
        if plotableonly:
            for i in range(len(names)-1,-1,-1):
                dimnames = self.nc.variables[names[i]].dimensions
                dimcount = len(dimnames)
                good = False
                if   dimcount==3:
                    if dimnames==('time','lat','lon'):
                        good = True
                elif dimcount==4:
                    if (dimnames==('time','z','lat','lon')) | (dimnames==('time','z1','lat','lon')):
                        good = True
                if not good: del names[i]
        return names
            
        # Get names of NetCDF variables
        try:
          vars = nc.variables
          if plotableonly:
              # Only take variables with 3 or 4 dimensions
              varNames = []
              for v in vars.keys():
                  dimnames = vars[v].dimensions
                  dimcount = len(dimnames)
                  if   dimcount==3:
                      if dimnames==('time','lat','lon'):
                          varNames += [v]
                  elif dimcount==4:
                      if (dimnames==('time','z','lat','lon')) | (dimnames==('time','z1','lat','lon')):
                          varNames += [v]
          else:
              # Take all variables
              varNames = vars.keys()

        #pycdf except pycdf.CDFError, msg:
        except Exception, e:
            raise Exception('CDFError: '+str(e))

        return varNames

    def getVariableTree(self,path):
        otherstores = {}
        if self.scenario!=None: otherstores['scenario'] = self.scenario
        return PlotVariableStore.getVariableTree(self,path,otherstores=otherstores)
        
class MergedPlotVariableStore(PlotVariableStore):
    
    class MergedPlotVariable(PlotVariable):
        def __init__(self,store,variables,mergedimid):
            PlotVariable.__init__(self,store)
            self.vars = variables
            self.mergedimid = mergedimid

        def getName(self):
            return self.vars[0].getName()

        def getLongName(self):
            return self.vars[0].getLongName()

        def getUnit(self):
            return self.vars[0].getUnit()

        def getDimensions(self):
            return tuple([self.mergedimid]+list(self.vars[0].getDimensions()))

        def getSlice(self,bounds):
            slice = self.Slice(self.getDimensions())
            assert len(bounds)==slice.ndim, 'Number of specified dimensions (%i) does not equal number of data dimensions (%i).' % (len(bounds),slice.ndim)
            
            # Get bound indices for the merged dimension
            ifirst,ilast = 0,len(self.vars)-1
            if bounds[0][0]!=None and bounds[0][0]>ifirst: ifirst = int(math.floor(bounds[0][0]))
            if bounds[0][1]!=None and bounds[0][1]<ilast : ilast  = int(math.ceil (bounds[0][1]))
            slice.coords[0] = numpy.linspace(float(ifirst),float(ilast),ilast-ifirst+1)
            slice.coords_stag[0] = common.getCenters(slice.coords[0],addends=True)

            first = True
            for ivar,var in enumerate(self.vars[ifirst:ilast+1]):
                curslice = var.getSlice(bounds[1:])
                if first:
                    slice.coords[1:] = curslice.coords
                    slice.coords_stag[1:] = curslice.coords_stag
                    slice.data = matplotlib.numerix.empty(tuple([ilast-ifirst+1]+list(curslice.data.shape)),matplotlib.numerix.typecode(curslice.data))
                    first = False
                slice.data[ivar,...] = curslice.data
            return slice

    def __init__(self,stores,mergedimid='obs',mergedimname='observation'):
        PlotVariableStore.__init__(self)
        self.stores = stores
        self.mergedimid = mergedimid
        self.mergedimname = mergedimname

    def getVariableNames(self):
        return self.stores[0].getVariableNames()

    def getVariableLongNames(self):
        return self.stores[0].getVariableLongNames()

    def getDimensionInfo(self,dimname):
        if dimname==self.mergedimid: 
            info = PlotVariableStore.getDimensionInfo(self,dimname)
            info['label'] = self.mergedimname
            return info
        return self.stores[0].getDimensionInfo(dimname)

    def getVariable(self,varname):
        vars = []
        for store in self.stores:
            var = store.getVariable(varname)
            if var==None:
                print 'Store "%s" does not contain variable "%s".' % (store,varname)
                return None
            vars.append(var)
        return MergedPlotVariableStore.MergedPlotVariable(self,vars,self.mergedimid)

class CustomPlotVariableStore(PlotVariableStore):

    def __init__(self):
        PlotVariableStore.__init__(self)
        self.vars = []
        
    def addVariable(self,var):
        self.vars.append(var)

    def getVariableNames(self):
        return [v.getName() for v in self.vars]

    def getVariableLongNames(self):
        return [v.getLongName() for v in self.vars]

    def getDimensionInfo(self,dimname):
        return PlotVariableStore.getDimensionInfo(self,dimname)

    def getVariable(self,varname):
        for v in self.vars:
            if v.getName()==varname: return v
        return None

