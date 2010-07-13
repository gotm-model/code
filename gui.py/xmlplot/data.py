# Import modules from standard Python library
import os, sys, re, datetime, shutil, StringIO, types, UserDict, glob

# Import additional third party modules
import numpy

# Import our custom modules
import common, xmlstore.util, xmlstore.xmlstore

def openNetCDF(path,mode='r'):
    # Test if the path contains wildcard, and resolves to multiple files.
    # If so, we will try to combine these files.
    if isinstance(path,basestring):
        paths = glob.glob(path)
        if len(paths)==1:
            path = paths[0]
        elif len(paths)>1:
            path = paths
    
    if isinstance(path,basestring):
        return getNetCDFFile(path,mode)
    else:
        assert mode=='r','A multi-file NetCDF dataset can only be opened for reading.'
        return MultiNetCDFFile(*path)

netcdfmodules,selectednetcdfmodule = None,None
def chooseNetCDFModule():
    global netcdfmodules,selectednetcdfmodule
    global pupynere,Scientific,netCDF4,pynetcdf
    
    netcdfmodules = []
    selectednetcdfmodule = -1
    error = ''
    
    # We prefer ScientificPython. Try that first.
    ready = True
    try:
        import Scientific.IO.NetCDF
    except ImportError,e:
        error += 'Cannot load Scientific.IO.NetCDF. Reason: %s.\n' % str(e)
        ready = False
    if ready:
        oldscientific = False
        try:
            version = map(int,Scientific.__version__.split('.')[:2])
            oldscientific = version[0]<2 or (version[0]==2 and version[1]<7)
        except: pass
        if not oldscientific and selectednetcdfmodule==-1: selectednetcdfmodule = len(netcdfmodules)
        netcdfmodules.append(('Scientific.IO.NetCDF',Scientific.__version__))

    # Try to locate netCDF4.
    ready = True
    try:
        import netCDF4
    except ImportError,e:
        error += 'Cannot load netCDF4. Reason: %s.\n' % str(e)
        ready = False
    if ready:
        if selectednetcdfmodule==-1: selectednetcdfmodule = len(netcdfmodules)
        netcdfmodules.append(('netCDF4',netCDF4.__version__))
    
    # Try to locate pynetcdf.
    ready = True
    try:
        import pynetcdf
    except ImportError,e:
        error += 'Cannot load pynetcdf. Reason: %s.\n' % str(e)
        ready = False
    if ready:
        if selectednetcdfmodule==-1:
            pyver = sys.version_info
            if (pyver[0]==2 and pyver[1]>=5) or pyver[0]>2:
                print 'pynetcdf will be used for NetCDF support. Note though that pynetcdf has known incompatibilities with Python 2.5 and higher, and you are using Python %i.%i.%i.' % (pyver[0],pyver[1],pyver[2])
            selectednetcdfmodule = len(netcdfmodules)
        netcdfmodules.append(('pynetcdf',''))

    # Try to locate PuPyNeRe, though that does not work for all NetCDF files (e.g., GOTM!).
    ready = True
    try:
        import pupynere
    except ImportError,e:
        error += 'Cannot load pupynere. Reason: %s.\n' % str(e)
        ready = False
    if ready:
        if selectednetcdfmodule==-1: selectednetcdfmodule = len(netcdfmodules)
        netcdfmodules.append(('pupynere',''))
        
    if selectednetcdfmodule==-1 and netcdfmodules: selectednetcdfmodule = 0

class NetCDFError(Exception): pass
def getNetCDFFile(path,mode='r'):
    """Returns a NetCDFFile file object representing the NetCDF file
    at the specified path. The returned object follows
    Scientific.IO.NetCDFFile conventions.
    
    Note: this is the *only* function that needs to know which NetCDF
    module to use. All other functions just operate on an object
    returned by this function, and expect this object to follow
    Scientific.IO.NetCDFFile conventions. Thus adding/replacing a module
    for NetCDF support should only require a chnage in this function.
    """
    
    if selectednetcdfmodule is None: chooseNetCDFModule()

    # First import NetCDF file format support (we do this here rather
    # than on import, because this module can be useful without NetCDF
    # support as well).

    # First check if the file exists in the first place.
    if mode=='r' and not os.path.isfile(path):
        raise NetCDFError('"%s" is not an existing file.' % path)

    netcdfmodule = None
    if netcdfmodules: netcdfmodule = netcdfmodules[selectednetcdfmodule][0]
    if netcdfmodule=='Scientific.IO.NetCDF':
        try:
            nc = Scientific.IO.NetCDF.NetCDFFile(path,mode=mode)
        except Exception, e:
            raise NetCDFError('An error occured while opening the NetCDF file "%s": %s' % (path,str(e)))
    elif netcdfmodule=='netCDF4':
        try:
            nc = netCDF4.Dataset(path,mode=mode,format='NETCDF3_CLASSIC')
        except Exception, e:
            raise NetCDFError('An error occured while opening the NetCDF file "%s": %s' % (path,str(e)))
    elif netcdfmodule=='pupynere':
        try:
            nc = pupynere.NetCDFFile(path,mode=mode,mmap=False)
        except Exception, e:
            raise NetCDFError('An error occured while opening the NetCDF file "%s": %s' % (path,str(e)))
    elif netcdfmodule=='pynetcdf':
        try:
            nc = pynetcdf.NetCDFFile(path,mode=mode)
        except Exception, e:
            raise NetCDFError('An error occured while opening the NetCDF file "%s": %s' % (path,str(e)))
    else:
        # No NetCDF module found - raise exception.
        raise NetCDFError('Cannot load a module for NetCDF reading. Please install either ScientificPython, python-netcdf4 or pynetcdf.')
    return nc

class ReferenceTimeParseError(Exception): pass

def parseNcTimeUnit(fullunit):
  """Parses a udunits/COARDS units string to extract the reference time and time unit.
  Raises an exception if the string does not match udunits/COARDS convention.
  Returns the time unit (in days), and the reference date+time used.

  Supposedly the udunits package could do this, but so far I have not found a minimal
  udunits module for Python.
  """

  # Retrieve time unit (in days) and reference date/time, based on COARDS convention.
  if ' since ' not in fullunit:
      raise ReferenceTimeParseError('"units" attribute equals "%s", which does not follow COARDS convention. Problem: string does not contain " since ".' % fullunit)
  timeunit,reftime = fullunit.split(' since ')
  
  # Parse the reference date, time and timezone
  datematch = re.match(r'(\d\d\d\d)[-\/](\d{1,2})-(\d{1,2})\s*',reftime)
  if datematch is None:
    raise ReferenceTimeParseError('"units" attribute equals "%s", which does not follow COARDS convention. Problem: cannot parse date in "%s".' % (fullunit,reftime))
  year,month,day = map(int,datematch.group(1,2,3))
  year = max(year,1900) # datetime year>=datetime.MINYEAR, but strftime needs year>=1900
  hours,minutes,seconds,mseconds = 0,0,0,0
  reftime = reftime[datematch.end():]
  if len(reftime)>0:
    timematch = re.match(r'(\d{1,2}):(\d{1,2}):(\d{1,2}(?:\.\d*)?)\s*',reftime)
    if timematch is None:
        raise ReferenceTimeParseError('"units" attribute equals "%s", which does not follow COARDS convention. Problem: cannot parse time in "%s".' % (fullunit,reftime))
    hours,minutes = map(int,timematch.group(1,2))
    seconds = float(timematch.group(3))
    mseconds = 1e6*(seconds % 1.)
    seconds = int(seconds)
    reftime = reftime[timematch.end():]
  dateref = datetime.datetime(year,month,day,hours,minutes,seconds,tzinfo=xmlstore.util.utc)
  if len(reftime)>0:
    timezonematch = re.match(r'(-?\d{1,2})(?::?(\d\d))?$',reftime)
    if timezonematch is None:
        raise ReferenceTimeParseError('"units" attribute equals "%s", which does not follow COARDS convention. Problem: cannot parse time zone in "%s".' % (fullunit,reftime))
    if timezonematch.group(2) is None:
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
      raise ReferenceTimeParseError('"units" attribute equals "%s", which does not follow COARDS convention. Problem: unknown time unit "%s".' % (fullunit,timeunit))
  
  return timeunit,dateref

def getNcAttributes(obj):
    """Transparent access to the attributes of a NetCDF file or variable,
    using the clean ncattrs method of NetCDF4 if available.
    """
    if hasattr(obj,'ncattrs'): return obj.ncattrs()
    names = dir(obj)
    if 'close' in names:
        # NetCDF file
        return [name for name in names if name not in ('close','createDimension','createVariable','flush','sync')]
    else:
        # NetCDF variable
        return [name for name in names if name not in ('assignValue','getValue','typecode')]
      
def getNcData(ncvar,bounds=None,maskoutsiderange=True):
    """Returns a slab of values from a NetCDF variable, respecting several NetCDF attributes
    such as missing value specifications, valid value ranges, time unit, etc.
    """
    if bounds:
        # Bounds provided - read a slice.
        if len(ncvar.shape)!=len(bounds): raise Exception('Number of provided slices (%i) does not match number of dimensions (%i).' % (len(bounds),len(ncvar.shape)))
        dat = numpy.asarray(ncvar[bounds])
    elif len(ncvar.shape)>0:
        # Variable is non-scalar - read all data.
        dat = numpy.asarray(ncvar[(slice(None),)*len(ncvar.shape)])
    else:
        # Variable is a scalar - read all data.
        dat = numpy.asarray(ncvar.getValue())

    # Start without mask, and define function for creating/updating mask
    mask = None
    def addmask(mask,newmask):
        if mask is None:
            mask = numpy.zeros(dat.shape,dtype=numpy.bool)
        return numpy.logical_or(mask,newmask)

    # Process the various COARDS/CF variable attributes for missing data.
    if maskoutsiderange:
        if hasattr(ncvar,'valid_min'): mask = addmask(mask,dat<ncvar.valid_min)
        if hasattr(ncvar,'valid_max'): mask = addmask(mask,dat>ncvar.valid_max)
        if hasattr(ncvar,'valid_range'):
            assert len(ncvar.valid_range)==2,'NetCDF attribute "valid_range" must consist of two values, but contains %i.' % len(ncvar.valid_range)
            minv,maxv = ncvar.valid_range
            mask = addmask(mask,numpy.logical_or(dat<minv,dat>maxv))
    if hasattr(ncvar,'_FillValue'):    mask = addmask(mask,dat==numpy.asarray(ncvar._FillValue,   dtype=dat.dtype))
    if hasattr(ncvar,'missing_value'): mask = addmask(mask,dat==numpy.asarray(ncvar.missing_value,dtype=dat.dtype))

    # Apply the combined mask (if any)
    if mask is not None and mask.any(): dat = numpy.ma.masked_where(mask,dat,copy=False)

    # If we have to apply a transformation to the data, make sure that the data type can accommodate it.
    # Cast to the most detailed type available (64-bit float)
    if hasattr(ncvar,'scale_factor') or hasattr(ncvar,'add_offset') and dat.dtype!=numpy.float: dat = numpy.asarray(dat,dtype=numpy.float)
  
    # Apply transformation to data based on nc variable attributes.
    if hasattr(ncvar,'scale_factor'): dat *= float(ncvar.scale_factor)
    if hasattr(ncvar,'add_offset'):   dat += float(ncvar.add_offset)
  
    # If the unit is time, convert to internal time unit
    if hasattr(ncvar,'units'):
        timeref = None
        try:
            timeunit,timeref = parseNcTimeUnit(ncvar.units)
        except ReferenceTimeParseError:
            pass
        if timeref is not None:
            timeref = common.date2num(timeref)
            dat = timeref+timeunit*numpy.asarray(dat,numpy.float64)
      
    return dat
          
class MultiNetCDFFile(object):
    class Variable(object):
        def __init__(self,store,name):
            self.store = store
            self.name = name
            self.ncvars = [nc.variables[name] for nc in self.store.ncs]
            
        def __array__(self,*args,**kwargs):
            return numpy.asarray(self[(Ellipsis,)],*args,**kwargs)
            
        def __getitem__(self,indices):
            if not isinstance(indices,(tuple,list)): indices = (indices,)
            
            dims = list(self.dimensions)
            idim = dims.index(self.store.variabledim)
            shape = self.shape
            indices = common.processEllipsis(indices,len(shape))

            indices = list(indices)
            if isinstance(indices[idim],slice):
                istart,istop,istep = indices[idim].indices(shape[idim])
            else:
                istart = indices[idim]
                istop = istart+1
            
            data = []
            for ivar,ncvar in enumerate(self.ncvars):
                if istart>=ncvar.shape[idim]:
                    # Start position beyond current file.
                    istart -= ncvar.shape[idim]
                    istop  -= ncvar.shape[idim]
                else:
                    # Start position within current file.
                    if isinstance(indices[idim],int):
                        indices[idim] = istart
                        return ncvar[tuple(indices)]
                        #return getNcData(ncvar,tuple(indices))
                    if istop<=ncvar.shape[idim]:
                        # Stop position within current file
                        indices[idim] = slice(istart,istop,istep)
                    else:
                        # Stop position beyond current file
                        indices[idim] = slice(istart,None,istep)
                        left = (ncvar.shape[idim]-istart-1) % istep
                        istart = istep-left-1
                        istop -= ncvar.shape[idim]
                    data.append(ncvar[tuple(indices)])
                    #data.append(getNcData(ncvar,tuple(indices)))
                    if indices[idim].stop is not None: break
                    
                # Process overlap between current and next file.
                if ivar<len(self.ncvars)-1:
                    istart += self.store.overlaps[ivar]
                    istop += self.store.overlaps[ivar]
                    
            return numpy.concatenate(data,axis=idim)
            
        def ncattrs(self):
            return getNcAttributes(self.ncvars[0])
            
        def __getattr__(self,name):
            if name=='shape':
                return [self.store.dim2length[d] for d in self.ncvars[0].dimensions]
            for ncvar in self.ncvars:
                if hasattr(ncvar,name): return getattr(ncvar,name)
            raise AttributeError(name)

    class Variables(object,UserDict.DictMixin):
        def __init__(self,store):
            self.store = store
    
        def __getitem__(self,name):
            ncvar = self.store.ncs[0].variables[name]
            if self.store.variabledim not in ncvar.dimensions: return ncvar
            return MultiNetCDFFile.Variable(self.store,name)
        
        def keys(self):
            return self.store.ncs[0].variables.keys()

    def __init__(self,*args,**kwargs):
        paths = []
        for arg in args:
            paths += glob.glob(arg)
            
        # Functions for comparing two dictionaries, capable of
        # dealing with elements that are numpy arrays.
        def cmpattributes(atts1,atts2):
            match = set(atts1.iterkeys())==set(atts2.iterkeys())
            if not match: return False
            for k in atts1.iterkeys():
                match = atts1[k]==atts2[k]
                if hasattr(match,'all'): match = match.all() 
                if not match: return False
            return True
                    
        # Open NetCDF files.
        self.ncs = [getNetCDFFile(path) for path in paths]
                
        # Get list of all dimensions and variables (unions over all files).
        dims,vars = set(),set()
        for nc in self.ncs:
            dims.update(nc.dimensions.keys())
            vars.update(nc.variables.keys())
        
        # Check if all files use all dimensions and variables.
        # For variables, also check if the variable attributes are identical everywhere.
        dim2coords,var2attr = {},{}
        self.variabledim = kwargs.get('dimension',None)
        for nc,path in zip(self.ncs,paths):
            # Check variables
            for var in vars:
                # Check for presence of variable.
                assert var in nc.variables,'Variable %s does not appear in in "%s". For multiple NetCDF files to be loaded as one single file, they must all contain the same variables.' % (var,path)
                
                # Compare attributes
                ncvar = nc.variables[var]
                atts = dict([(k,getattr(ncvar,k)) for k in getNcAttributes(ncvar)])
                if var not in var2attr:
                    var2attr[var] = atts
                else:
                    assert cmpattributes(atts,var2attr[var]),'Current attributes of variable "%s" (%s) do not match its attributes in one of the other NetCDF files (%s).' % (var,atts,var2attr[var])
                    
            # Check dimensions
            for dim in dims:
                # Check for presence of dimension in dimensions and coordinate variables.
                assert dim in nc.dimensions,'Dimension %s is missing in "%s". For multiple NetCDF files to be loaded as one single file, all must use the same dimensions.' % (dim,path)

                # If no coordinate values are available, just continue with the next dimension.
                # (we will not be able to determine the file order, so we xcept the given order)
                if dim not in nc.variables: continue
                
                # Compare coordinate values.
                coord = getNcData(nc.variables[dim])
                if dim not in dim2coords:
                    dim2coords[dim] = coord
                else:
                    if self.variabledim!=dim and (dim2coords[dim].shape!=coord.shape or numpy.any(dim2coords[dim]!=coord)):
                        # These coordinates vary between files - make sure this is the only dimension that differs.
                        assert self.variabledim is None,'More than one dimension (%s, %s) varies between files.' % (self.variabledim,dim)
                        self.variabledim = dim
                        
        # Make sure that the values of one dimension vary between files.
        assert self.variabledim is not None, 'All dimensions have the same coordinates in the supplied files. One dimension should differ between files in order for them to be loaded as a single file.'
                        
        # Sort NetCDF files based on their values for the varying dimension.
        # Only works if we have the cooridnate values for all files.
        nc2coords = {}
        for nc in self.ncs:
            if self.variabledim in nc.variables: nc2coords[nc] = nc.variables[self.variabledim][0]
        if len(nc2coords)==len(self.ncs):
            self.ncs.sort(cmp=lambda x,y: cmp(nc2coords[x],nc2coords[y]))
        
        # Determine the length of all dimensions in the merged file, and
        # determine the overlap (if any) between the different files.
        self.dim2length = dict([(k,len(v)) for k,v in dim2coords.iteritems()])
        self.dim2length[self.variabledim] = 0
        self.overlaps = []
        lastcoord = None
        for nc in self.ncs:
            curcoord = getNcData(nc.variables[self.variabledim])
            if lastcoord is not None:
                overlap = curcoord.searchsorted(lastcoord[-1],side='right')
                self.dim2length[self.variabledim] -= overlap
                self.overlaps.append(overlap)
            self.dim2length[self.variabledim] += len(curcoord)
            lastcoord = curcoord
        
    def ncattrs(self):
        # Just return the NetCDF attributes of the first file.
        return getNcAttributes(self.ncs[0])

    def __getattr__(self,name):
        if name=='dimensions':
            return self.dim2length
        elif name=='variables':
            return MultiNetCDFFile.Variables(self)
            
        # Request for a custom attribute - loop over all NetCDF files until it is found.
        for nc in self.ncs:
            if hasattr(nc,name): return getattr(nc,name)
            
        raise AttributeError(name)
        
    def close(self):
        # Close all NetCDf files.
        for nc in self.ncs: nc.close()
        self.ncs = []

class LinkedFileVariableStore(common.VariableStore,xmlstore.datatypes.DataFileEx):

    # XML store-derived class for storing (cached) metadata of a data file,
    # such as coordinate ranges.
    # This is implemented as XML store (rather than Python object) because it
    # needs to be saved in a descriptive form along with the data files themselves.
    class DataFileCache(xmlstore.xmlstore.TypedStore):
        @classmethod
        def getSchemaInfo(cls):
            return xmlstore.xmlstore.schemainfocache[os.path.join(common.getDataRoot(),'schemas/datafilecache')]

        def __init__(self,valueroot=None,adddefault = True,schema=None):
            if schema is None: schema = os.path.join(common.getDataRoot(),'schemas/datafilecache/0001.schema')
            xmlstore.xmlstore.TypedStore.__init__(self,schema,valueroot,adddefault=adddefault)

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
                        dimdata = common.VariableStore.getDimensionInfo_raw(self,None)
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
                    if datematch is None:
                        raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
                    refvals = map(int,datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                    dimvalue = xmlstore.util.dateTimeFromTuple(refvals)
                    if prevdate is not None and dimvalue<prevdate:
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
            if datematch is None:
                raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
            refvals = map(int,datematch.groups()) # Convert matched strings into integers
            curdate = xmlstore.util.dateTimeFromTuple(refvals)
            times[-1][ipos] = common.date2num(curdate)
            
            # Read values.
            data = line[datematch.end()+1:].split()
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
            if dimisdate: dimdata = common.num2date(dimdata)
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
                griddedvalues[it,:,:] = common.interp1(depths[it],values[it],depthgrid)
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
            if datematch is None:
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
                if callback is not None and iline%1000==0:
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
        nc = openNetCDF(path)
        for convention in NetCDFStore.conventions:
            if convention.testFile(nc): return convention(nc)
        return NetCDFStore(nc)
    
    class NetCDFVariable(common.Variable):
        def __init__(self,store,ncvarname):
            common.Variable.__init__(self,store)
            self.ncvarname = str(ncvarname)
            
        def __str__(self):
            return str(self.store)+'/'+self.ncvarname

        def getName_raw(self):
            return self.ncvarname

        def setData(self,data,slic=(Ellipsis,),converttime=True):
            assert self.store.mode=='w','NetCDF file has not been opened for writing.'
            nc = self.store.getcdf()
            ncvar = nc.variables[self.ncvarname]
            if converttime and hasattr(ncvar,'units'):
                timeref = None
                try:
                    timeunit,timeref = parseNcTimeUnit(ncvar.units)
                except ReferenceTimeParseError:
                    pass
                if timeref is not None:
                    timeref = common.date2num(timeref)
                    data = numpy.asarray((data-timeref)/timeunit,dtype=self.getDataType())
            if hasattr(data,'filled') and hasattr(ncvar,'_FillValue'):
                ncvar[slic] = data.filled(ncvar._FillValue)
            else:
                ncvar[slic] = data

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
            propnames = getNcAttributes(ncvar)
            return dict([(key,getattr(ncvar,key)) for key in propnames])

        def setProperty(self,name,value):
            nc = self.store.getcdf()
            ncvar = nc.variables[self.ncvarname]
            setattr(ncvar,name,value)
            
        def getDataType(self):
            nc = self.store.getcdf()
            ncvar = nc.variables[self.ncvarname]
            if hasattr(ncvar,'dtype'): return ncvar.dtype
            return ncvar.typecode()
            
        def getDimensions_raw(self,reassign=True):
          nc = self.store.getcdf()
          ncvar = nc.variables[self.ncvarname]
          rawdims = list(ncvar.dimensions)
          
          if reassign:
              # Re-assign dimensions based on the "coordinates" attribute of the variable.
              if hasattr(ncvar,'coordinates'):
                coords = tuple(reversed(ncvar.coordinates.split()))
                if coords[0] in nc.variables:
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
                        #print 'undoing reassignment to %s because %s is not in variable dimensions %s' % (rawdims[idim],cdim,','.join(ncvar.dimensions))
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
          if not isinstance(bounds,(list,tuple)): bounds = (bounds,)
        
          dimnames = list(self.getDimensions_raw())
          shape = self.getShape()

          # Process Ellipsis (if present) and check whether the number of boundaries matches the number of dimensions.
          bounds = common.processEllipsis(bounds,len(dimnames))
          assert len(bounds)==len(dimnames), 'Number of boundaries (%i) does not match number of dimensions (%i).' % (len(bounds),len(dimnames))
                    
          # Convert bounds to list of slice objects.
          # Non-integer bounds are initially ignored; after retrieving the coordinate arrays, these are filled in.
          boundindices,floatslices,floatindices = [],[],[]
          for idim,bound in enumerate(bounds):
            if isinstance(bound,int):
                # Integer value provided as index.
                assert bound>=-shape[idim], 'Slice index %i lies below the lowest possible index for dimension %s (%i).' % (bound,dimnames[idim],-shape[idim]  )
                assert bound<  shape[idim], 'Slice index %i exceeds the highest possible index for dimension %s (%i).'   % (bound,dimnames[idim], shape[idim]-1)
                if bound<0: bound += shape[idim]
                boundindices.append(bound)
            elif not isinstance(bound,slice):
                # Floating point value or other non-integer object provided as index.
                boundindices.append(slice(0,shape[idim]))
                floatindices.append(idim)
            elif not (isinstance(bound.start,(int,types.NoneType)) and isinstance(bound.stop,(int,types.NoneType))):
                # Non-integer slice specification (e.g., using floating point numbers or datetime objects).
                assert bound.step is None,'Non-integer slices with explicitly specified step are not supported.'
                boundindices.append(slice(0,shape[idim]))
                floatslices.append(idim)
            else:
                # Normal (integer-based) slice specification
                start,stop,step = bound.indices(shape[idim])
                boundindices.append(slice(start,stop,step))

          # Translate slices based on non-integer values (e.g. floating point values, dates)
          # to slices based on integers.
          for idim in floatslices:
            dimname = dimnames[idim]
            
            # Get the entire coordinate array
            coordvar = self.store.getVariable_raw(dimname)
            coorddims = list(coordvar.getDimensions())
            coords = coordvar.getSlice([boundindices[dimnames.index(cd)] for cd in coorddims], dataonly=True, cache=True)
            istart,istop = common.getboundindices(coords,coorddims.index(dimname),bounds[idim].start,bounds[idim].stop)
            boundindices[idim] = slice(istart,istop,1)

          # Translate indices based on non-integer values (e.g. floating point values, dates)
          # to integer indices.
          floatdimnames = [dimnames[idim] for idim in floatindices]
          newshape = [shape[idim] for idim in floatindices]
          summeddistance = numpy.zeros(newshape,dtype=numpy.float)
          for idim in floatindices:
            bound = bounds[idim]
            if isinstance(bound,datetime.datetime): bound = common.date2num(bound)
            dimname = dimnames[idim]
            coordvar = self.store.getVariable_raw(dimname)
            coorddims = list(coordvar.getDimensions())
            for cd in coorddims:
                assert cd in dimnames,'Coordinate %s depends on %s, but the variable %s itself does not depend on %s.' % (dimname,cd,self.getName(),cd)
                assert cd in floatdimnames,'A float index is provided for dimension %s, but not for dimension %s on which %s depends.' % (dimname,cd,dimname)
            coords = coordvar.getSlice([boundindices[dimnames.index(cd)] for cd in coorddims], dataonly=True, cache=True)
            coords = common.broadcastSelective(coords,coorddims,newshape,floatdimnames)
            summeddistance += numpy.abs(coords-bound)
          indices = numpy.unravel_index(summeddistance.argmin(), newshape)
          for idim,index in zip(floatindices,indices): boundindices[idim] = index
          
          return tuple(boundindices)
          
        def getData(self,bounds=None,stagger=False):
        
            # Discover effective boundaries
            effbounds,newshape = [],[]
            for b in self.translateSliceSpecification(bounds):
                if isinstance(b,slice):
                    # Set the upper bound to 1 + the index of the last element that will be taken
                    b = slice(b.start,b.stop-(b.stop-b.start-1)%b.step,b.step)
                effbounds.append(b)

            # Convert stagger argument to list with dimension indices to stagger.
            if not stagger:
                stagger = ()
            elif not isinstance(stagger,(list,tuple,set)):
                stagger = range(len(effbounds))
            stagger = [s for s in stagger if not isinstance(effbounds[s],int)]

            shape = self.getShape()
                                
            newshape = []
            for i,b in enumerate(effbounds):
                if isinstance(b,slice):
                    l = 1+(b.stop-b.start-1)/b.step
                    if i in stagger: l+=1
                else:
                    l = 1
                newshape.append(l)
            data = numpy.empty(newshape,dtype=numpy.float)
            data = numpy.ma.array(data,mask=True,copy=False)
                
            addborders = []
            for i in range(len(effbounds)):
                b = effbounds[i]
                addleft,addright,addcenter = False,False,True
                if i in stagger:
                    centers = b.step%2==0   # Use centers for interface coordinates if the stride is an even number.
                    start = b.start - b.step/2
                    stop = b.stop + b.step/2 + 1
                    if start<0:
                        start += b.step
                        addleft = True
                    if stop>shape[i]+1:
                        stop -= b.step
                        addright = True
                    if centers: stagger.remove(i)
                    addcenter = stop>start
                    effbounds[i] = slice(start,stop,b.step)
                addborders.append((addleft,addcenter,addright))
                
            def getdata(bounds,stag):
                print 'Request for:'
                for i,b in enumerate(bounds):
                    print '   ',b,(i in stag)
                return 0.

            def processdim(bounds,addborders,curslice,curstagger,curtarget):
                if not bounds:
                    data[curtarget] = getdata(curslice,curstagger)
                    return
                    
                curbound = bounds[0]
                if isinstance(curbound,int):
                    return processdim(bounds[1:],addborders[1:],curslice+[curbound],curstagger,curtarget)
                
                addleft,addcenter,addright = addborders[0]
                idim = len(curslice)
                start,stop = None,None
                if addleft:
                    start = 1
                    processdim(bounds[1:],addborders[1:],curslice+[0],curstagger+[idim],curtarget+[0])
                if addright:
                    stop = -1
                    processdim(bounds[1:],addborders[1:],curslice+[-1],curstagger+[idim],curtarget+[-1])
                if addcenter:
                    if idim in stagger: curstagger += [idim]
                    processdim(bounds[1:],addborders[1:],curslice+[curbound],curstagger,curtarget+[slice(start,stop)])

            processdim(effbounds,addborders,[],[],[])
            
            assert data._mask.sum()==0,'%i entries are still masked.' % data._mask.sum()
          
        def getNcData(self,bounds=None):
          # Get NetCDF file and variable objects.
          nc = self.store.getcdf()
          ncvar = nc.variables[self.ncvarname]
          try:
            dat = getNcData(ncvar,bounds,maskoutsiderange=self.store.maskoutsiderange)
          except Exception,e:
            raise Exception('Unable to read data from netCDF variable "%s": %s' % (self.ncvarname,str(e)))

          return dat
              
        def getSlice(self,bounds=None,dataonly=False,cache=False,transfercoordinatemask=True):
          if bounds is None: bounds = (Ellipsis,)
        
          # Translate the slice specification so only slice objects and integer indices remain.
          bounds = self.translateSliceSpecification(bounds)
          
          # Retrieve the data values
          n = 1L
          for l in self.getShape(): n *= l
          if cache and n<1000000:
              # Take all data from cache if present, otherwise read all data from NetCDF and store it in cache first.
              if self.ncvarname not in self.store.cachedcoords:
                  self.store.cachedcoords[self.ncvarname] = self.getNcData()
              dat = self.store.cachedcoords[self.ncvarname]
              if bounds:
                assert len(bounds)==dat.ndim,'%s: number of data dimensions (%i) does not match number of provided slices (%i).' % (str(self),dat.ndim,len(bounds))
                dat = dat[bounds]
          else:
              # Read the data slab directly from the NetCDF file.
              dat = self.getNcData(bounds)
              
              # Determine the expected shape of the returned data.
              expectedshape = []
              for b in bounds:
                if isinstance(b,slice):
                    expectedshape.append((b.stop-b.start-1)/b.step+1)
              expectedshape = tuple(expectedshape)
              
              # netCDF4 pre 2010-07-12 incorrectly neglects to squeeze out singleton dimension of scalars.
              # Therefore, ignore differences between expected and returned data shape if they are due to singleton dimensions.
              if dat.shape!=expectedshape and [l for l in expectedshape if l>1]==[l for l in dat.shape if l>1]: dat.shape = expectedshape
              
              # Check whether expected and returned data shapes match.
              assert dat.shape==expectedshape,'%s: getNcData returned data with shape %s, while shape %s was requested.' % (self.getName(),dat.shape,expectedshape)

          # If the caller wants the data values only, we are done: return the value array.
          if dataonly: return dat

          # Get dimension names
          dimnames = list(self.getDimensions_raw())

          # Create Variable.Slice object to hold coordinates and data.
          newdimnames = [d for d,b in zip(dimnames,bounds) if isinstance(b,slice)]
          varslice = self.Slice(newdimnames)

          # Retrieve coordinate values
          inewdim = 0
          datamask = None
          if hasattr(dat,'_mask'): datamask = dat._mask
          for idim,dimname in enumerate(dimnames):
            # If we take a single index for this dimension, it will not be included in the output.
            if (not transfercoordinatemask) and not isinstance(bounds[idim],slice): continue

            # Get the coordinate variable          
            coordvar = self.store.getVariable_raw(dimname)
            
            if coordvar is None:
                # No coordinate variable available: use indices
                if not isinstance(bounds[idim],slice): continue
                coorddims = [dimname]
                coords = numpy.arange(bounds[idim].start,bounds[idim].stop,bounds[idim].step,dtype=numpy.float)
            else:
                # Coordinate variable present: use it.
                coorddims = list(coordvar.getDimensions())

                # Debug check: see if all coordinate dimensions are also used by the variable.
                for cd in coorddims:
                    assert cd in dimnames, 'Coordinate dimension %s is not used by this variable (it uses %s).' % (cd,', '.join(dimnames))

                # Get coordinate values
                coordslice = [bounds[dimnames.index(cd)] for cd in coorddims]
                coords = coordvar.getSlice(coordslice, dataonly=True, cache=True)
                
            # Get the list of coordinate dimensions after the ones with single index have been sliced out.
            newcoorddims = [cd for cd in coorddims if isinstance(bounds[dimnames.index(cd)],slice)]

            # Transfer the coordinate mask to the data if desired.
            if transfercoordinatemask and hasattr(coords,'_mask') and isinstance(coords._mask,numpy.ndarray):
                newmask = common.broadcastSelective(coords._mask,newcoorddims,dat.shape,newdimnames)
                if datamask is None:
                    datamask = newmask
                else:
                    datamask = numpy.logical_or(datamask,newmask)

            # If we take a single index for this dimension, it will not be included in the output.
            if not isinstance(bounds[idim],slice): continue
            
            # Coordinates should not have a mask - undo the masking.
            if hasattr(coords,'_mask'): coords = numpy.array(coords,copy=False)

            # Locate variable that contains staggered [boundary] coordinates.
            stagcoordvar = None
            if coordvar is not None:
                if dimname in self.store.staggeredcoordinates:
                    # The store has assigned a variable with staggered coordinates.
                    stagcoordvar = self.store.getVariable_raw(self.store.staggeredcoordinates[dimname])
                    assert stagcoordvar is not None, 'Staggered coordinate for dimension %s registered in store as variable %s, but not present as variable.' % (dimname,self.store.staggeredcoordinates[dimname])
                elif 'bounds' in coordvar.getProperties():
                    # The variable itself points to a variable with staggered coordinates (CF convention: bounds attribute).
                    boundvar = coordvar.getProperties()['bounds']
                    stagcoordvar = self.store.getVariable_raw(boundvar)
                    assert stagcoordvar is not None, 'Boundary values for coordinate variable %s are set to variable %s, but this variable is not present in the NetCDF file.' % (coordvar.getName(),boundvar)
            
            # Get staggered coordinates over entire domain
            if stagcoordvar is not None:
                centshape = coordvar.getShape()
                stagshape = stagcoordvar.getShape()
                if len(stagshape)==len(centshape)+1:    # CF convention: one extra dimension for the corner index
                    stagdata = stagcoordvar.getSlice(dataonly=True, cache=True)
                    newshape = [l+1 for l in centshape]
                    stagcoordvar = numpy.zeros(newshape)
                    if len(centshape)==1:
                        assert stagshape[-1]==2, 'A 1D coordinate variable must have 2 boundaries per cell (not %i).' % (stagshape[-1],)
                        stagcoordvar[:-1] =  stagdata[:,0]
                        stagcoordvar[1: ] += stagdata[:,1]
                        stagcoordvar[1:-1] /= 2
                    elif len(centshape)==2:
                        assert stagshape[-1]==4, 'A 2D coordinate variable must have 4 boundaries per cell (not %i).' % (stagshape[-1],)
                        stagcoordvar[ :-1, :-1]  = stagdata[:,:,0]
                        stagcoordvar[ :-1,1:  ] += stagdata[:,:,1]
                        stagcoordvar[1:,  1:  ] += stagdata[:,:,2]
                        stagcoordvar[1:  , :-1] += stagdata[:,:,3]
                        stagcoordvar[1:-1,:] /= 2
                        stagcoordvar[:,1:-1] /= 2
            
                coordslice_stag = []
                for slc in coordslice:
                    if isinstance(slc,slice):
                        # We take a subset of this dimension: extent the slice with 1.
                        coordslice_stag.append(slice(slc.start,slc.stop+slc.step,slc.step))
                    else:
                        # We take a single [centered] index from this dimension:
                        # Get the left and right bounds, so we can average them later.
                        coordslice_stag.append(slice(slc,slc+2))
                if isinstance(stagcoordvar,numpy.ndarray):
                    coords_stag = stagcoordvar[coordslice_stag]
                else:
                    coords_stag = stagcoordvar.getSlice(coordslice_stag, dataonly=True, cache=True)

                # Undo the staggering of the dimensions that we take a single slice through
                # by averaging the left- and right bounds.
                for i in range(len(coordslice)-1,-1,-1):
                    if isinstance(coordslice[i],int): coords_stag = coords_stag.mean(axis=i)

                # Coordinates should not have a mask - undo the masking.
                if hasattr(coords_stag,'_mask'): coords_stag = numpy.array(coords_stag,copy=False)
            else:
                # Auto-generate the staggered coordinates.
                coords_stag = common.stagger(coords)
            
            # Insert data dimensions where they are lacking in coordinate
            coords      = common.broadcastSelective(coords,     newcoorddims,dat.shape,               newdimnames)
            coords_stag = common.broadcastSelective(coords_stag,newcoorddims,[l+1 for l in dat.shape],newdimnames)

            # Assign coordinate values
            varslice.coords     [inewdim] = coords
            varslice.coords_stag[inewdim] = coords_stag
            
            inewdim += 1

          # If center coordinates came with a mask, apply that same mask to the data.
          if datamask is not None:
            dat = numpy.ma.masked_where(datamask,dat,copy=False)

          varslice.data = dat
                  
          return varslice

    def __init__(self,path=None,*args,**kwargs):
        xmlstore.util.referencedobject.__init__(self)
        common.VariableStore.__init__(self)
        
        self.datafile = None
        self.nc = None
        self.mode = 'r'

        self.cachedcoords = {}
        self.reassigneddims = {}
        self.staggeredcoordinates = {}
        
        # Whether to mask values outside the range specified by valid_min,valid_max,valid_range
        # NetCDF variable attributes (as specified by CF convention)
        self.maskoutsiderange = True
        
        if path is not None:
            if isinstance(path,(tuple,list,basestring)):
                # Path to a NetCDF file is provided, or a list/tuple of paths.
                self.load(path,*args,**kwargs)
            else:
                # Open NetCDF file is provided.
                self.nc = path
                self.autoReassignCoordinates()
                self.relabelVariables()
                
    def __str__(self):
        if self.datafile is None: return ''
        if isinstance(self.datafile,(list,tuple)): return ', '.join(self.datafile)
        return self.datafile

    def getDimensionInfo_raw(self,dimname):
        res = common.VariableStore.getDimensionInfo_raw(self,dimname)
        var = self.getVariable_raw(dimname)
        if var is None: return res
        res['label'] = var.getLongName()
        res['unit']  = var.getUnit()
        if dimname in ('z','z1'):
            res['preferredaxis'] = 'y'
        elif self.isTimeDimension(dimname):
            res['datatype'] = 'datetime'
            res['preferredaxis'] = 'x'
            res['unit'] = ''
        props = var.getProperties()
        if props.get('positive','up')=='down':
            res['reversed'] = True
        return res
                
    def save(self,path):
        assert isinstance(self.datafile,basestring),'Only single NetCDF files can be saved.'
        shutil.copyfile(self.datafile,path)
        
    def unlink(self):
        if self.nc is not None:
            # Close NetCDF result file.
            self.nc.close()
            self.nc = None
            self.datafile = None
            
    def load(self,path,mode='r'):
        # Store link to result file, and try to open the CDF file
        self.datafile = path
        self.mode = mode
        nc = self.getcdf()
        
        # Auto-reassign coordinates
        self.autoReassignCoordinates()
        
        # Re-label variables - this must be done after reassignments because relabel requests
        # the variable names, which are then cached and never requested again. Variable names can
        # depend on dimension reassignments, e.g., if some reassignments apply, extra coordinate
        # variables may be added.
        self.relabelVariables()

    def autoReassignCoordinates(self):
        self.reassigneddims = {}
    
    def getcdf(self):
        """Returns a NetCDFFile file object representing the NetCDF file
        at the path in self.datafile. The returned object should follow
        Scientific.IO.NetCDFFile conventions.
        """
        if self.nc is not None: return self.nc
        assert self.datafile is not None, 'The path to the NetCDF file has not yet been set. This may imply that the object has been unlinked.'
        self.nc = openNetCDF(self.datafile,self.mode)
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
    
    def createDimension(self,dimname,length):
        assert self.mode=='w','NetCDF file has not been opened for writing.'
        nc = self.getcdf()
        nc.createDimension(dimname, length)

    def setProperty(self,name,value):
        setattr(self.getcdf(),name,value)
        
    def addVariable(self,varName,dimensions,datatype='d',missingvalue=None):
        assert self.mode=='w','NetCDF file has not been opened for writing.'
        nc = self.getcdf()
        ncvar = nc.createVariable(varName,datatype,dimensions)
        if missingvalue is not None:
            setattr(ncvar,'missing_value',missingvalue)
            setattr(ncvar,'_FillValue',missingvalue)
        return self.getVariable_raw(varName)
                
    def copyVariable(self,variable):
        assert self.mode=='w','NetCDF file has not been opened for writing.'
        assert isinstance(variable,NetCDFStore.NetCDFVariable),'Added variable must be an existing NetCDF variable object, not %s.' % str(variable)
        nc = self.getcdf()
        dims = variable.getDimensions_raw(reassign=False)
        shape = variable.getShape()
        for dim,length in zip(dims,shape):
            if dim not in nc.dimensions: self.createDimension(dim, length)
        data = variable.getSlice((Ellipsis,),dataonly=True)
        nctype = {'float32':'f','float64':'d'}[str(data.dtype)]
        var = self.addVariable(variable.getName(),dims,datatype=nctype)
        for key,value in variable.getProperties().iteritems():
            var.setProperty(key,value)
        var.setData(data)
        return var

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
        
    def isTimeDimension(self,dimname):
        """See if specified dimension is a time dimension according to COARDS convention.
        """
        try:
            timeunit,timeref = self.getTimeReference(dimname)
        except ReferenceTimeParseError:
            return False
        return True

    def getTimeReference(self,dimname):
      """Parses the "units" attribute of the NetCDF variable, and returns the time unit
      (in days) and the reference date. Throws an exception if the "units" attribute does
      not match the COARDS/udunits convention for specifying time offsets.
      """
      nc = self.getcdf()
      if dimname not in nc.variables:
          raise ReferenceTimeParseError('dimensions "%s" does not have an associated variable.' % (dimname,))

      cdfvar = self.getcdf().variables[dimname]
      if not hasattr(cdfvar,'units'):
          raise ReferenceTimeParseError('variable "%s" lacks "units" attribute.' % (dimname,))
        
      return parseNcTimeUnit(cdfvar.units)

class NetCDFStore_GOTM(NetCDFStore):
    """Class encapsulating a GOTM/GETM-produced NetCDF file.
    
    The file is expected to follow the COARDS/CF convention, and in addition assumes
    
    - the GOTM/GETM convention for storing time-variable depth/leyer heights (h + elev).
    - the GETM convention for curvilinear grids (xic, etac -> lonc, latc)
    """
    
    @staticmethod
    def testFile(nc):
        match = False
        ncvars,ncdims = nc.variables,nc.dimensions
        
        # Test for GETM with curvilinear coordinates
        # (either lon,lat or staggered Cartesian coordinates must be available)
        if ('xic'  in ncdims and 'etac' in ncdims and
            (('lonc' in ncvars and 'latc' in ncvars)
             or ('xx' in ncvars and 'yx' in ncvars))): match = True

        # Test for GETM with cartesian coordinates
        if ('xc'  in ncdims and 'yc' in ncdims and
            'lonc' in ncvars and 'latc' in ncvars): match = True

        # Test for GOTM with variable layer heights and sea surface elevation
        if ('z' in ncdims and 'z1' in ncdims and
            'h' in ncvars and 'zeta' in ncvars): match = True

        # Test for GETM with variable heights and sea surface elevation
        if ('level' in ncdims and
            'h' in ncvars and 'elev' in ncvars): match = True

        if ('sigma' in ncdims and
            'bathymetry' in ncvars and 'elev' in ncvars): match = True

        return match

    def __init__(self,path=None,*args,**kwargs):
        self.xname,self.yname,self.hname,self.elevname = 'lon','lat','h','zeta'
        self.bathymetryname = None

        # Link new depth coordinates to an existing NetCDF dimension
        self.depth2coord = {}

        self.generatecartesiancenters = False

        NetCDFStore.__init__(self,path,*args,**kwargs)
        
        # Link centered and staggered coordinates
        self.staggeredcoordinates['z' ] = 'z_stag'
        self.staggeredcoordinates['z1'] = 'z1_stag'
                
    def autoReassignCoordinates(self):
        NetCDFStore.autoReassignCoordinates(self)
        
        # Get reference to NetCDF file and its variables and dimensions.
        nc = self.getcdf()
        ncvars,ncdims = self.getVariableNames_raw(),nc.dimensions

        # --------------------------------------------------------------
        # Re-assign x,y coordinate dimensions
        # --------------------------------------------------------------

        # Re-assign for GETM with curvilinear coordinates
        # Preferentially, x and y are re-assigned to longitude and latitude.
        # If these are not available, they will be re-assigned to projected x and y instead.
        if 'xic' in ncdims and 'etac' in ncdims:
            # Center coordinate are available, re-assign to either lon,lat or projected x,y, if possible.
            self.xname,self.yname = 'xic','etac'   # x,y dimensions to be used for depth
            if 'lonc' in ncvars and 'latc' in ncvars:
                self.reassigneddims['xic' ] = 'lonc'
                self.reassigneddims['etac'] = 'latc'
            elif 'xc' in ncvars and 'yc' in ncvars:
                self.reassigneddims['xic' ] = 'xc'
                self.reassigneddims['etac'] = 'yc'
        if 'xix' in ncdims and 'etax' in ncdims:
            # Boundary coordinate are available, re-assign to either lon,lat or projected x,y, if possible.
            if 'lonx' in ncvars and 'latx' in ncvars:
                self.reassigneddims['xix' ] = 'lonx'
                self.reassigneddims['etax'] = 'latx'
            elif 'xx' in ncvars and 'yx' in ncvars:
                self.reassigneddims['xix' ] = 'xx'
                self.reassigneddims['etax'] = 'yx'

        # Re-assign for GETM with cartesian coordinates.
        # x and y are re-assigned to longitude and latitude, if possible.
        if 'xc' in ncdims and 'yc' in ncdims:
            # Center coordinate are available.
            self.xname,self.yname = 'xc','yc'   # x,y dimensions to be used for depth
            if 'lonc' in ncvars and 'latc' in ncvars:
                self.reassigneddims['xc' ] = 'lonc'
                self.reassigneddims['yc'] = 'latc'
        if 'xx' in ncdims and 'yx' in ncdims:
            # Boundary coordinate are available.
            if 'lonx' in ncvars and 'latx' in ncvars:
                self.reassigneddims['xx'] = 'lonx'
                self.reassigneddims['yx'] = 'latx'

        # For GETM with spherical coordinates, we just need to remember the latitude,longitude
        # names for when we return the dimensions of the new vertical coordinates.
        if self.xname=='lon' and ('lon' not in ncvars) and 'lonc' in ncvars: self.xname = 'lonc'
        if self.yname=='lat' and ('lat' not in ncvars) and 'latc' in ncvars: self.yname = 'latc'

        # --------------------------------------------------------------
        # Re-assign vertical dimension
        # NB the is done automatically for GOTM, because the original
        # z and z1 variables are overwritten.
        # --------------------------------------------------------------

        # Re-assign depth coordinate dimension if using GETM with elevation,layer heights
        if ('level' in ncdims and 'h' in ncvars and 'elev' in ncvars):
            # GETM: "level" reassigned to "z"
            self.reassigneddims['level' ] = 'z'
            self.hname,self.elevname = 'h','elev'
            self.depth2coord['z'] = 'level'
        elif ('sigma' in ncdims and 'bathymetry' in ncvars and 'elev' in ncvars):
            # GETM: "sigma" reassigned to "z"
            self.reassigneddims['sigma' ] = 'z'
            self.bathymetryname,self.elevname = 'bathymetry','elev'
            self.depth2coord['z'] = 'sigma'
            
    def getVariableNames_raw(self):
        names = list(NetCDFStore.getVariableNames_raw(self))
        
        nc = self.getcdf()
        ncvars,ncdims = nc.variables,nc.dimensions
        if self.elevname in ncvars and (self.hname in ncvars or ('sigma' in ncvars and 'bathymetry' in ncvars)):
            names.append('z')
        
            # Only add alternative depth coordinate if it is actually used in the NetCDF file.
            # (note: GETM does not use it, but GOTM does)
            if 'z1' in ncvars: names.append('z1')
            
        self.generatecartesiancenters = self.generatecartesiancenters or ('xx' in ncvars and 'yx' in ncvars and 'xic' in ncdims and 'etac' in ncdims and 'xc' not in ncvars and 'yc' not in ncvars)
        if self.generatecartesiancenters:
            # We have to generate centered Cartesian coordinates
            self.staggeredcoordinates['xc'] = 'xx'
            self.staggeredcoordinates['yc'] = 'yx'
            names += ['xc','yc']
        
        return names

    def getVariable_raw(self,varname):
            
        class CenterVariable(NetCDFStore.NetCDFVariable):
            def __init__(self,store,ncvarname):
                NetCDFStore.NetCDFVariable.__init__(self,store,ncvarname)
                self.centername = ncvarname
                self.stagname = '%sx' % ncvarname[0]

            def getShape(self):
                s = self.store[self.stagname].getShape()
                return (s[0]-1,s[1]-1)

            def getLongName(self):
                return '%s-position' % self.centername

            def getUnit(self):
                return self.store[self.stagname].getUnit()

            def getProperties(self):
                return {'history':'auto-generated from boundary coordinates in variable %s' % self.stagname}

            def getDataType(self):
                return self.store[self.stagname].getDataType()

            def getDimensions_raw(self,reassign=True):
                dims = ('etac','xic')
                if reassign: dims = [self.store.reassigneddims.get(d,d) for d in dims]
                return dims

            def getNcData(self,bounds=None,allowmask=True):
                # If no bounds are set, use complete data range.
                if bounds is None:
                    shape = self.getShape()
                    bounds = (slice(0,shape[0]),slice(0,shape[1]))
                    
                # Convert integer indices to slices so we always have 2 dimensions.
                fullbounds = []
                for b in bounds:
                    if isinstance(b,int): b = slice(b,b+1)
                    fullbounds.append(b)
                
                # Obtain all 4 corners
                stagvar = self.store[self.stagname]
                stagvals = stagvar.getSlice(fullbounds,dataonly=True).copy()
                oldbound0 = fullbounds[0]
                fullbounds[0] = slice(fullbounds[0].start+1,fullbounds[0].stop+1,fullbounds[0].step)
                stagvals += stagvar.getSlice(fullbounds,dataonly=True)
                fullbounds[1] = slice(fullbounds[1].start+1,fullbounds[1].stop+1,fullbounds[1].step)
                stagvals += stagvar.getSlice(fullbounds,dataonly=True)
                fullbounds[0] = oldbound0
                stagvals += stagvar.getSlice(fullbounds,dataonly=True)
                
                # Average the corners to obtain center coordinates
                centers = stagvals/4.
                
                # Eliminate singleton dimensiuons where integer indices were used.
                if bounds is not None:
                    newshape = []
                    for l,b in zip(centers.shape,bounds):
                        if not isinstance(b,int): newshape.append(l)
                    centers.shape = newshape
                    
                # Return center coordinates.
                return centers

        class DepthVariable(NetCDFStore.NetCDFVariable):
            def __init__(self,store,ncvarname,dimname):
                NetCDFStore.NetCDFVariable.__init__(self,store,ncvarname)
                self.dimname = dimname
                self.cachedshape = None
        
            def getName_raw(self):
                return self.dimname

            def getLongName(self):
                return 'depth'

            def getUnit(self):
                return self.store[self.store.elevname].getUnit()

            def getProperties(self):
                return {}

            def getDimensions_raw(self,reassign=True):
                dims = list(self.store[self.store.elevname].getDimensions_raw(reassign=False))
                dims.insert(1,self.store.depth2coord.get(self.dimname,self.dimname))
                if reassign: dims = [self.store.reassigneddims.get(d,d) for d in dims]
                return dims
                
            def getShape(self):
                if self.cachedshape is None:
                    if self.store.bathymetryname is None:
                        self.cachedshape = self.store[self.store.hname].getShape()
                    else:
                        elevshape = self.store[self.store.elevname].getShape()
                        sigmashape = self.store['sigma'].getShape()
                        self.cachedshape = [elevshape[0],sigmashape[0]] + list(elevshape[1:])
                    if self.dimname.endswith('_stag'):
                        self.cachedshape = tuple([l+1 for l in self.cachedshape])
                return self.cachedshape
                
            def getNcData(self,bounds=None,allowmask=True):
                # Return values from cache if available.
                if 'z' in self.store.cachedcoords:
                    if bounds is None:
                        return self.store.cachedcoords[self.dimname]
                    else:                        
                        return self.store.cachedcoords[self.dimname][bounds]

                # Get the bounds to use for the different variables used  to calculate depth.
                cachebasedata = bounds is not None
                elevbounds,hbounds,bathbounds,sigmabounds = (Ellipsis,),(Ellipsis,),(Ellipsis,),(Ellipsis,)
                if bounds is not None:
                    # First translate integer indices into slice objects with length 1.
                    # This ensures all 4 dimensions will be present during the calculations.
                    newbounds = []
                    for l in bounds:
                        if isinstance(l,int):
                            l = slice(l,l+1)
                        elif self.dimname.endswith('_stag'):
                            # If we need staggered coordinates, all dimensions will expand by 1
                            # in the end. Therefore, subtract 1 from their length here.
                            l = slice(l.start,l.stop-l.step,l.step)
                        newbounds.append(l)
                        
                    # Make sure calculations operate on entire depth range
                    newbounds[1] = slice(None)
                    
                    elevbounds = (newbounds[0],newbounds[2],newbounds[3])
                    hbounds = newbounds
                    bathbounds = newbounds[-2:]
                    sigmabounds = (newbounds[1],)
            
                np = numpy
                mask,elevmask = None,None
                data = {}
                            
                # Get elevations
                elev = self.store[self.store.elevname].getSlice(elevbounds,dataonly=True,cache=cachebasedata)

                # Subroutine for creating and updating the depth mask.
                def setmask(mask,newmask):
                    if mask is None:
                        zshape = list(elev.shape)
                        zshape.insert(1,self.store['z'].getShape()[1])
                        mask = numpy.empty(zshape,dtype=numpy.bool)
                        mask[...] = newmask
                    else:
                        mask = numpy.logical_or(mask,newmask)
                    return mask

                # If elevations are (partially) masked, first fill the first layer of masked cells around
                # the data with a nearest-neighbor approach. This improves the elevations of interfaces.
                # Then save the mask so we can reapply it later.
                if hasattr(elev,'_mask'):
                    if isinstance(elev._mask,numpy.ndarray) and numpy.any(elev._mask):
                        mask = setmask(mask,elev._mask[:,numpy.newaxis,...])
                        elev = common.interpolateEdges(elev,dims=(1,2))
                        elevmask = elev._mask
                    elev = elev.filled(0.)

                if self.store.bathymetryname is None:
                    # Get layer heights (dimension 0: time, dimension 1: depth, dimension 2: y coordinate, dimension 3: x coordinate)
                    h = self.store[self.store.hname].getSlice(hbounds,dataonly=True,cache=cachebasedata)
                                        
                    # Fill masked values (we do not want coordinate arrays with masked values)
                    # This should not have any effect, as the value arrays should also be masked at
                    # these locations.
                    # Check for the "filled" attribute to see if these are masked arrays.
                    if hasattr(h,'_mask'):
                        mask = setmask(mask,h._mask)
                        h = h.filled(0.)
                    
                    # Get depths of interfaces
                    z_stag = h.cumsum(axis=1)
                    sliceshape = list(z_stag.shape)
                    sliceshape[1] = 1
                    z_stag = np.concatenate((numpy.zeros(sliceshape,z_stag.dtype),z_stag),axis=1)
                    bottomdepth = z_stag[:,-1,...]-elev
                    z_stag -= bottomdepth[:,numpy.newaxis,...]
                    
                    # Get depths of layer centers
                    z = z_stag[:,1:z_stag.shape[1],...]-0.5*h[:,:,...]
                    
                    # The actual interface coordinate z1 lacks the bottom interface
                    z1 = z_stag[:,1:,...]
                    
                    # Store depth dimension
                    data['z']  = z
                    data['z1'] = z1
                    
                    if bounds is None or self.dimname in ('z_stag','z1_stag'):
                        # Use the actual top and bottom of the column as boundary interfaces for the
                        # grid of the interface coordinate.
                        z1_stag = np.concatenate((np.take(z_stag,(0,),1),z[:,1:,...],np.take(z_stag,(-1,),1)),1)
                        
                        # Use normal staggering for the time, longitude and latitude dimension.
                        data['z_stag']  = common.stagger(z_stag, (0,2,3),defaultdeltafunction=self.store.getDefaultCoordinateDelta,dimnames=self.getDimensions_raw())
                        data['z1_stag'] = common.stagger(z1_stag,(0,2,3),defaultdeltafunction=self.store.getDefaultCoordinateDelta,dimnames=self.getDimensions_raw())
                else:
                    # Get bathymetry (dimension 0: y coordinate, dimension 1: x coordinate)
                    bath = self.store[self.store.bathymetryname].getSlice(bathbounds,dataonly=True,cache=cachebasedata)
                    if hasattr(bath,'_mask'):
                        mask = setmask(mask,bath._mask)
                        if isinstance(bath._mask,numpy.ndarray):
                            bath = common.interpolateEdges(bath)
                        # Fill the bathymetry with the shallowest value in the domain.
                        bath = bath.filled(min(bath.min(),-elev.max()))
                                            
                    # Let elevation follow bathymetry whereever it was originally masked.
                    if elevmask is not None:
                        bigbath = numpy.empty_like(elev)
                        bigbath[:] = bath
                        elev[elevmask] = -bigbath[elevmask]

                    # Calculate water depth at each point in time
                    # Clip it at zero: nearest neighbor interpolation of elevations may have
                    # caused water levels below the bottom.
                    depth = numpy.maximum(bath[numpy.newaxis,:,:]+elev,0.)
                    
                    # Get sigma levels (constant across time and space)
                    sigma = self.store['sigma'].getSlice(sigmabounds,dataonly=True,cache=cachebasedata)
                    
                    # From sigma levels and water depth, calculate the z coordinates.
                    data['z'] = sigma.reshape((1,-1,1,1))*depth[:,numpy.newaxis,:,:] + elev[:,numpy.newaxis,:,:]
                    if bounds is None or self.dimname=='z_stag':
                        # Calculate staggered sigma coordinates
                        sigma_stag = numpy.empty((sigma.shape[0]+1,),dtype=sigma.dtype)
                        sigma_stag[0] = -1.
                        sigma_stag[1:-1] = (sigma[:-1]+sigma[1:])/2.
                        sigma_stag[-1] = 0.

                        # First stagger in deth dimension.
                        z_stag = sigma_stag.reshape((1,-1,1,1))*depth[:,numpy.newaxis,:,:] + elev[:,numpy.newaxis,:,:]
                        
                        # Use default staggering for remaining dimensions of staggered z.
                        data['z_stag'] = common.stagger(z_stag,dimindices=(0,2,3),defaultdeltafunction=self.store.getDefaultCoordinateDelta,dimnames=self.getDimensions_raw())

                # Apply the mask to the center coordinates (if any)
                if mask is not None:
                    data['z'] = numpy.ma.masked_where(mask,data['z'])

                # If we retrieve the entire range, store all coordinates in cache
                # and return the slice we need.
                if bounds is None:
                    self.store.cachedcoords.update(data)
                    return data[self.dimname]
                
                # Retrieve the desired coordinates.
                res = data[self.dimname]
                
                # Now finally take the depth range that we need
                depthslice = bounds[1]
                if isinstance(depthslice,int): depthslice = slice(depthslice,depthslice+1)
                res = res[(slice(None),depthslice,Ellipsis)]
                
                # Undo the staggering for the dimension that we take a single slice through.
                if self.dimname.endswith('_stag'):
                    # This is a staggered variable - average left and right bounds.
                    for i in range(len(bounds)-1,-1,-1):
                        if isinstance(bounds[i],int): res = res.mean(axis=i)
                else:
                    # This is a non-staggered variable - flatten the dimensions.
                    res.shape = [l for i,l in enumerate(res.shape) if not isinstance(bounds[i],int)]

                return res

        if self.generatecartesiancenters and varname in ('xc','yc'):
            return CenterVariable(self,varname)
        elif varname in ('z','z1','z_stag','z1_stag'):
            return DepthVariable(self,varname,varname)
        return NetCDFStore.getVariable_raw(self,varname)

    def getDefaultCoordinateDelta(self,dimname,coords):
        # Only operate on 1D coordinates
        if coords.ndim>1: return NetCDFStore.getDefaultCoordinateDelta(self,dimname,coords)

        # Only operate on time dimension
        try:
            timeunit,timeref = self.getTimeReference(dimname)
        except ReferenceTimeParseError:
            return NetCDFStore.getDefaultCoordinateDelta(self,dimname,coords)
            
        # Take delta as the difference between the reference time and the first time step
        if coords[0]>timeref: return coords[0]-timeref
        
        return 1.

class NetCDFStore_MOM4(NetCDFStore):
    @staticmethod
    def testFile(nc):
        match = False
        ncvars,ncdims = nc.variables,nc.dimensions
        if ('xt_ocean' in ncdims and 'yt_ocean' in ncdims and
            'geolon_t' in ncvars and 'geolat_t' in ncvars): match = True
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