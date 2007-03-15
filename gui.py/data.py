import os, re, datetime, xml.dom.minidom, tempfile, shutil, StringIO

import common, xmlstore, scenario, plot

# Import NetCDF file format support
#import pycdf
from pynetcdf import NetCDFFile
import matplotlib.numerix

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

    def getVariableTree(self,path,otherstores={}):
        xmlschema = xml.dom.minidom.parse(path)
        vardict = self.getVariableLongNames()
        found = set(self.filterNodes(xmlschema.documentElement,vardict))
        remaining = [nodename for nodename in vardict if nodename not in found]
        other = None
        for ch in xmlschema.getElementsByTagName('element'):
            if ch.getAttribute('id')=='other':
                other = ch
                break
        if other!=None:
            if len(remaining)==0:
                other.parentNode.removeChild(other)
            else:
                for varid in sorted(remaining,cmp=lambda x,y: cmp(vardict[x].lower(), vardict[y].lower())):
                    el = xmlschema.createElement('element')
                    el.setAttribute('id',varid)
                    el.setAttribute('label',vardict[varid])
                    el.setAttribute('type','bool')
                    other.appendChild(el)
        return xmlstore.TypedStore(xmlschema,otherstores=otherstores)

    def filterNodes(self,node,vardict):
        nodeid = node.getAttribute('id')
        assert nodeid!='', 'Node lacks "id" attribute.'
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

    def __init__(self):
        pass

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

    # getValues(bounds, staggered=False)
    #   Return type: tuple of (numpy/numeric) arrays
    #   Returns the arrays with coordinates (in order of dimnesions returned by getDimensions), and the value array.
    #   Coordinates may be given as 1D arrays (if the coordinates are constant across all other dimensions), or as arrays with
    #   the same numbers of dimensions as the value array (i.e., for every value a coordinate is supplied). If staggered is True,
    #   coordinates must be given at the interfaces and values at the centers; then coordinate arrays must have one extra value in
    #   every dimension compared to the value array.
    def getValues(self,bounds,staggered=False):
        return ()

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
            filetype = self.store.type
            if filetype=='profilesintime':
                return ('time','z')
            elif filetype=='pointsintime':
                return ('time',)
            else:
                assert False, 'Cannot plot variables from file of unknown type "%s".' % filetype

        def getValues(self,bounds,staggered=False):
            data = self.store.getData()
            timebounds = common.findindices(bounds[0],data[0])
            if len(data)==2:
                return [data[0][timebounds[0]:timebounds[1]+1],data[1][timebounds[0]:timebounds[1]+1,self.index]]
            elif len(data)==3:
                return [data[0][timebounds[0]:timebounds[1]+1],data[1],data[2][timebounds[0]:timebounds[1]+1,:,self.index]]
            assert False, 'Cannot handle variables with %i dimensions; only know how to deal with 2 or 3 dimensions.' % len(data)

    def __init__(self,node,datafile=None):
        self.vardata = []

        if datafile==None: datafile = node.getValue()
        self.datafile = datafile

        finfo = common.findDescendantNode(node.templatenode,['fileinfo'])
        self.type = finfo.getAttribute('type')
        
        fvars = common.findDescendantNode(finfo,['filevariables'])
        if fvars!=None:
            for ch in fvars.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filevariable':
                    longname = ch.getAttribute('label')
                    unit = ch.getAttribute('unit')
                    name = longname
                    self.vardata.append([name,longname,unit])

        self.data = None

    def getVariableNames(self):
        if not self.datafile.isValid(): return []
        return [data[0] for data in self.vardata]

    def getVariableLongNames(self):
        if not self.datafile.isValid(): return {}
        return dict([(data[0],data[1]) for data in self.vardata])

    def getVariable(self,varname):
        if not self.datafile.isValid(): return None
        for (index,data) in enumerate(self.vardata):
            if data[0]==varname:
                return self.LinkedFileVariable(self,data,index)
        assert False, 'Variable with name "%s" not found in store.' % varname

    def getData(self,callback=None):
        if self.data!=None: return self.data
        if not self.datafile.isValid(): return None
        
        varcount = len(self.vardata)
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()

        # Compile regular expression for reading dates.
        datetimere = re.compile('(\d\d\d\d).(\d\d).(\d\d) (\d\d).(\d\d).(\d\d)')

        filetype = self.type
        if filetype=='pointsintime':
            times = []
            values = []
            line = f.readline()
            iline = 1
            while line!='':
                datematch = datetimere.match(line)
                if datematch==None:
                    raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
                refvals = map(lambda(i): int(i),datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                curdate = datetime.datetime(*refvals)
                data = line[datematch.end()+1:].split()
                if len(data)<varcount:
                    raise Exception('Line %i contains only %i observations, where %i are expected.' % (iline,len(data),varcount))
                data = map(lambda(i): float(i),data)
                times.append(curdate)
                values.append(data)
                if callback!=None and iline%1000==0: callback('processed %i lines.' % iline)
                line = f.readline()
                iline += 1
            times = matplotlib.numerix.array(times,matplotlib.numerix.PyObject)
            values = matplotlib.numerix.array(values,matplotlib.numerix.Float32)
            self.data = (times,values)
        elif filetype=='profilesintime':
            line = f.readline()
            times = []
            depths = []
            values = []
            uniquedepths = {}
            iline = 1
            while line!='':
                # Read date & time
                datematch = datetimere.match(line)
                if datematch==None:
                    raise Exception('Line %i does not start with time (yyyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
                refvals = map(lambda(i): int(i),datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                curdate = datetime.datetime(*refvals)

                # Get the number of observations and the depth direction.
                (depthcount,updown) = map(lambda(i): int(i), line[datematch.end()+1:].split())

                # Create arrays that will contains depths and observed values.
                curdepths = matplotlib.numerix.zeros((depthcount,),matplotlib.numerix.Float32)
                curvalues = matplotlib.numerix.zeros((depthcount,varcount),matplotlib.numerix.Float32)
                
                if updown==1:
                    depthindices = range(0,depthcount,1)
                else:
                    depthindices = range(depthcount-1,-1,-1)
                for idepthline in depthindices:
                    if callback!=None and iline%1000==0: callback('processed %i lines.' % iline)
                    line = f.readline()
                    if line=='':
                        raise Exception('Premature end-of-file after line %i; expected %i more observations.' % (iline,depthcount-depthindices.index(idepthline)))
                    iline += 1
                    linedata = map(lambda(i): float(i),line.split())
                    if len(linedata)<varcount+1:
                        raise Exception('Line %i contains only %i value(s), where %i (1 time and %i observations) are expected.' % (iline,len(linedata),varcount+1,varcount))
                    uniquedepths[linedata[0]] = True
                    curdepths[idepthline] = linedata[0]
                    curvalues[idepthline,:] = linedata[1:varcount+1]
                times.append(curdate)
                depths.append(curdepths)
                values.append(curvalues)
                if callback!=None and iline%1000==0: callback('processed %i lines.' % iline)
                line = f.readline()
                iline += 1
            times = matplotlib.numerix.array(times,matplotlib.numerix.PyObject)
            uniquedepths = uniquedepths.keys()
            uniquedepths.sort()
            if len(uniquedepths)<200:
                depthgrid = matplotlib.numerix.array(uniquedepths,matplotlib.numerix.Float32)
            else:
                depthstep = (uniquedepths[-1]-uniquedepths[0])/200
                depthgrid = matplotlib.numerix.arange(uniquedepths[0],uniquedepths[-1]+depthstep,depthstep)
            griddedvalues = matplotlib.numerix.zeros((times.shape[0],depthgrid.shape[0],varcount),matplotlib.numerix.Float32)
            for it in range(len(times)):
                griddedvalues[it,:,:] = common.interp1(depths[it],values[it],depthgrid)
                if callback!=None and (it+1)%20==0: callback('gridded %i profiles.' % (it+1),progress=.5+float(it+1)/len(times)/2)
            self.data = (times,depthgrid,griddedvalues)
        else:
            assert False, 'Cannot plot variables from file of unknown type "%s".' % filetype
        f.close()
        return self.data

# Class that represents a GOTM result.
#   Inherits from PlotVariableStore, as it contains variables that can be plotted.
#   Contains a link to the scenario from which the result was created (if available)
class Result(PlotVariableStore):
    reportdirname = 'reporttemplates'
    reportname2path = None

    @staticmethod
    def getReportTemplates():
        if Result.reportname2path==None:
            Result.reportname2path = {}
            #sourcedir = os.path.join(os.path.dirname(__file__),Result.reportdirname)
            sourcedir = Result.reportdirname
            if os.path.isdir(sourcedir):
                for filename in os.listdir(sourcedir):
                    if filename=='CVS': continue
                    fullpath = os.path.join(sourcedir,filename)
                    if os.path.isdir(fullpath):
                        if os.path.isfile(os.path.join(fullpath,'index.xml')):
                            Result.reportname2path[filename] = fullpath
                        else:
                            print 'WARNING: template directory "%s" does not contain "index.xml"; it will be ignored.' % fullpath
                    else:
                        print 'WARNING: template directory "%s" contains "%s" which is not a directory; the latter will be ignored.' % (sourcedir,filename)
            else:
                print 'WARNING: no report templates will be available, because subdirectory "%s" is not present!' % Result.reportdirname
        return Result.reportname2path

    class ResultVariable(PlotVariable):
        def __init__(self,result,varname):
            PlotVariable.__init__(self)
            self.result = result
            self.varname = str(varname)

        def getName(self):
            return self.varname

        def getLongName(self):
            nc = self.result.getcdf()
            #pycdf return nc.var(self.varname).long_name
            return nc.variables[self.varname].long_name

        def getUnit(self):
            nc = self.result.getcdf()
            #pycdf return nc.var(self.varname).units
            return nc.variables[self.varname].units

        def getDimensions(self):
          nc = self.result.getcdf()
          #pycdf vars = nc.variables()
          #dimnames = vars[self.varname][0]
          #dimcount = len(dimnames)
          #if   dimcount==3:
          #    if dimnames==('time','lat','lon'):
          #        return ('time',)
          #elif dimcount==4:
          #    if (dimnames==('time','z','lat','lon')) or (dimnames==('time','z1','lat','lon')):
          #        return ('time','z')
          #else:
          #  raise Exception('This variable has '+str(dimcount)+' dimensions; I do not know how to handle such variables.')

          dimnames = nc.variables[self.varname].dimensions
          dimcount = len(dimnames)
          if   dimcount==3:
              if dimnames==('time','lat','lon'):
                  return ('time',)
          elif dimcount==4:
              if (dimnames==('time','z','lat','lon')) or (dimnames==('time','z1','lat','lon')):
                  return ('time','z')
          raise Exception('This variable has dimensions %s; I do not know how to handle such variables.' % str(dimnames))

        def getValues(self,bounds,staggered=False):
          nc = self.result.getcdf()
            
          # Get the variable and its dimensions from CDF.

          #pycdf
          #try:
          #    v = nc.var(self.varname)
          #    dims = v.dimensions()
          #except pycdf.CDFError, msg:
          #    print msg
          #    return False
          v = nc.variables[self.varname]
          dims = v.dimensions

          # Get time coordinates and time bounds.
          (t,t_stag) = self.result.getTime()
          timebounds = common.findindices(bounds[0],t)
          if not staggered:
              t_eff = t[timebounds[0]:timebounds[1]+1]
          else:
              t_eff = t_stag[timebounds[0]:timebounds[1]+2]

          dimcount = len(dims)
          if dimcount==4:
              # Four-dimensional variable: longitude, latitude, depth, time
              (z,z1,z_stag,z1_stag) = self.result.getDepth()
              depthbounds = (0,z.shape[1])
              if dims[1]=='z':
                  if staggered:
                      z_cur = z_stag[timebounds[0]:timebounds[1]+2,depthbounds[0]:depthbounds[1]+2]
                  else:
                      z_cur = z[timebounds[0]:timebounds[1]+1,depthbounds[0]:depthbounds[1]+1]
              elif dims[1]=='z1':
                  if staggered:
                      z_cur = z1_stag[timebounds[0]:timebounds[1]+2,depthbounds[0]:depthbounds[1]+2]
                  else:
                      z_cur = z1[timebounds[0]:timebounds[1]+1,depthbounds[0]+1:depthbounds[1]+2]
              try:
                  dat = v[timebounds[0]:timebounds[1]+1,depthbounds[0]:depthbounds[1]+1,0,0]
              except Exception, e:
                  raise Exception('Unable to read values for NetCDF variable "%s". Error: %s' % (self.varname,str(e)))
              return [t_eff,z_cur,dat]
          elif dimcount==3:
              # Three-dimensional variable: longitude, latitude, time
              try:
                  dat = v[timebounds[0]:timebounds[1]+1,0,0]
              except Exception, e:
                  raise Exception('Unable to read values for NetCDF variable "%s". Error: %s' % (self.varname,str(e)))
              return [t_eff,dat]
          else:
            raise Exception('This variable has dimensions %s; I do not know how to handle such variables.' % str(dimensions))

    def __init__(self):
        PlotVariableStore.__init__(self)
        
        self.scenario = None
        self.tempdir = None
        self.datafile = None
        self.nc = None
        self.changed = False
        self.gotmoutput = None

        # Cached coordinates
        self.t = None
        self.t_stag = None
        self.z = None
        self.z1 = None
        self.z_stag = None
        self.z1_stag = None

        impl = xml.dom.minidom.getDOMImplementation()
        self.figuretree = impl.createDocument(None, 'FigureSettings', None)

    def hasChanged(self):
        return self.changed

    def getTempDir(self,empty=False):
        if self.tempdir!=None:
            if empty:
                for f in os.listdir(self.tempdir): 
                    os.remove(os.path.join(self.tempdir,f))
        else:
            self.tempdir = tempfile.mkdtemp('','gotm-')
            print 'Created temporary result directory "%s".' % self.tempdir
        return self.tempdir

    def save(self,path,addfiguresettings=True):
        assert self.datafile!=None, 'The result object was not yet attached to a result file (NetCDF).'

        container = xmlstore.DataContainerZip(path,'w')

        if addfiguresettings:
            df = xmlstore.DataFileXmlDocument(self.figuretree)
            df_added = container.addItem(df,'figuresettings.xml')
            df_added.release()
            df.release()

        if self.scenario!=None:
            fscen = StringIO.StringIO()
            self.scenario.saveAll(fscen,claim=False)
            df = xmlstore.DataFileMemory(fscen.getvalue(),'scenario.gotmscenario')
            fscen.close()
            container.addItem(df)
            df.release()
        
        container.addFile(self.datafile,'result.nc')
        container.persistChanges()
        container.release()

        self.changed = False

    def load(self,path):
        # Basic check: does the specified file exist?
        if not os.path.exists(path): raise Exception('File "%s" does not exist.' % path)

        container = xmlstore.DataContainerZip(path,'r')
        files = container.listFiles()
        if 'scenario.gotmscenario' not in files:
            raise Exception('The archive "%s" does not contain "scenario.gotmscenario"; it cannot be a GOTM result.' % path)
        if 'result.nc' not in files:
            raise Exception('The archive "%s" does not contain "result.nc"; it cannot be a GOTM result.' % path)

        # Create a temporary directory to which we can unpack the archive.
        tempdir = self.getTempDir()

        df = container.getItem('scenario.gotmscenario')
        self.scenario = scenario.Scenario.fromSchemaName(scenario.guiscenarioversion)
        self.scenario.loadAll(df)
        df.release()

        df = container.getItem('result.nc')
        resultpath = os.path.join(tempdir,'result.nc')
        df.saveToFile(resultpath)
        df.release()

        df = container.getItem('figuresettings.xml')
        if df!=None:
            f = df.getAsReadOnlyFile()
            self.figuretree = xml.dom.minidom.parse(f)
            f.close()
            df.release()

        # Close the archive
        container.release()

        # Attach the result, try to open the CDF file
        self.datafile = resultpath
        self.getcdf()

        # Reset "changed" status.
        self.changed = False

    def setFigure(self,name,valuetree):
        self.changed = True
        for ch in self.figuretree.documentElement.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE and ch.getAttribute('name')==name:
                if ch.isSameNode(valuetree): return
                print 'Deleting previous figure node for "%s".' % name
                self.figuretree.documentElement.removeChild(ch)
                break
        node = common.copyNode(valuetree,self.figuretree.documentElement)
        node.setAttribute('name',name)

    def getFigure(self,name):
        for ch in self.figuretree.documentElement.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE and ch.getAttribute('name')==name:
                return ch
        return None

    def unlink(self):
        if self.nc!=None:
            # Close NetCDF result file.
            self.nc.close()
            self.nc = None
        if self.tempdir!=None:
            # Delete temporary directory.
            print 'Deleting temporary result directory "%s".' % self.tempdir
            shutil.rmtree(self.tempdir)
            self.tempdir = None

    def attach(self,srcpath,scenario=None,copy=True):
        self.scenario = scenario
        
        if copy:
            # Create a copy of the result file.
            tempdir = self.getTempDir(empty=True)
            datafile = os.path.join(tempdir,'result.nc')
            shutil.copyfile(srcpath,datafile)
        else:
            datafile = srcpath

        # Store link to result file, and try to open the CDF file
        self.datafile = datafile
        self.getcdf()

        self.changed = False

    def getcdf(self):
        if self.nc!=None: return self.nc
        assert self.datafile!=None, 'The result object has not yet been attached to an actual result.'
        try:
          #pycdf self.nc = pycdf.CDF(str(self.datafile))
          self.nc = NetCDFFile(self.datafile)
        #pycdf except pycdf.CDFError, e:
        except Exception, e:
            raise Exception('An error occured while opening the NetCDF file "%s": %s' % (self.datafile,str(e)))
        return self.nc

    def getVariableNames(self,plotableonly=True):
        nc = self.getcdf()

        # Get names of NetCDF variables
        try:
          #pycdf vars = nc.variables()
          vars = nc.variables
          if plotableonly:
              # Only take variables with 3 or 4 dimensions
              varNames = []
              for v in vars.keys():
                  #pycdf dimnames = vars[v][0]
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

    def getVariableLongNames(self):
      varnames = self.getVariableNames()
      nc = self.getcdf()
      vardict = {}
      for varname in varnames:
          #pycdf varname_str = str(varname)
          #pycdf vardict[varname] = nc.var(varname_str).long_name
          vardict[varname] = nc.variables[varname].long_name
      return vardict

    def getVariable(self,varname,check=True):
        varname = str(varname)
        if check:
            nc = self.getcdf()
            #pycdf vars = nc.variables()
            vars = nc.variables
            if not (varname in vars): return None
        return self.ResultVariable(self,varname)

    def getTime(self):
        if self.t==None:
            nc = self.getcdf()

            # Get time coordinate (in seconds since reference date)
            #pycdf secs = nc.var('time').get()
            secs = nc.variables['time'][:]
            
            # Convert time-in-seconds to Python datetime objects.
            dateref = self.getReferenceDate()
            t = matplotlib.numerix.zeros((secs.shape[0],),matplotlib.numerix.PyObject)
            for it in range(t.shape[0]):
                t[it] = dateref + datetime.timedelta(secs[it]/3600/24)

            # Create staggered time grid.
            t_stag = matplotlib.numerix.zeros((secs.shape[0]+1,),matplotlib.numerix.PyObject)
            halfdt = datetime.timedelta(seconds=float(secs[1]-secs[0])/2)
            t_stag[0]  = t[0]-halfdt
            t_stag[1:] = t[:]+halfdt
            
            # Cache time grids.
            self.t = t
            self.t_stag = t_stag
            
        return (self.t,self.t_stag)

    def getDepth(self):
        if self.z==None:
            nc = self.getcdf()

            # Get layers heights
            #pycdf h = nc.var('h')[:,:,0,0]
            h = nc.variables['h'][:,:,0,0]
            
            # Get depths of interfaces
            z1 = matplotlib.numerix.cumsum(h[:,:],1)
            z1 = matplotlib.numerix.concatenate((matplotlib.numerix.zeros((z1.shape[0],1),matplotlib.numerix.typecode(z1)),z1),1)
            bottomdepth = z1[0,-1]
            z1 = z1[:,:]-bottomdepth

            # Get depth of layer centers
            z = z1[:,1:z1.shape[1]]-0.5*h

            # Interpolate in time to create staggered grid
            z1_med = matplotlib.numerix.concatenate((matplotlib.numerix.take(z1,(0,),0),z1,matplotlib.numerix.take(z1,(-1,),0)),0)
            z_stag = 0.5 * (z1_med[0:z1_med.shape[0]-1,:] + z1_med[1:z1_med.shape[0],:])
            
            z_med = matplotlib.numerix.concatenate((z,matplotlib.numerix.take(z1,(-1,),1)),1)
            z_med = matplotlib.numerix.concatenate((matplotlib.numerix.take(z_med,(0,),0),z_med,matplotlib.numerix.take(z_med,(-1,),0)),0)
            z1_stag = 0.5 * (z_med[0:z_med.shape[0]-1,:] + z_med[1:z_med.shape[0],:])

            self.z = z
            self.z1 = z1
            self.z_stag = z_stag
            self.z1_stag = z1_stag

        return (self.z,self.z1,self.z_stag,self.z1_stag)

    def getReferenceDate(self):
      # Retrieve reference date/time.
      nc = self.getcdf()
      #pycdf timeunit = nc.var('time').units
      timeunit = nc.variables['time'].units
      datematch = re.compile('(\d\d\d\d)[-\/](\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)').search(timeunit, 1)
      assert datematch!=None, 'Unable to parse "units" attribute of "time" variable in NetCDF file!'
      refvals = map(lambda(i): int(i),datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
      dateref = datetime.datetime(*refvals)
      return dateref

    def getVariableTree(self,path):
        otherstores = {}
        if self.scenario!=None: otherstores['scenario'] = self.scenario
        return PlotVariableStore.getVariableTree(self,path,otherstores=otherstores)
    
    def generateReport(self,outputpath,templatepath,plotvariables,dpi=150,columncount=2,figuresize=(10,8),callback=None):
        xmldocument = xml.dom.minidom.parse(os.path.join(templatepath,'index.xml'))
        scenario = self.scenario

        steps = float(2+len(plotvariables))
        istep = 0

        # Create output directory if it does not exist yet.
        if not os.path.isdir(outputpath): os.mkdir(outputpath)

        for f in os.listdir(templatepath):
            fullpath = os.path.join(templatepath,f)
            if f.lower()!='index.xml' and os.path.isfile(fullpath): shutil.copy(fullpath,os.path.join(outputpath,f))

        for node in xmldocument.getElementsByTagName('gotm:scenarioproperty'):
            variablepath = node.getAttribute('variable')
            assert variablepath!='', 'gotm:scenarioproperty node in report template lacks "variable" attribute pointing to a location in the scenario.'
            variablenode = scenario.root.getLocation(variablepath.split('/'))
            assert variablenode!=None, 'Unable to locate "%s" in the scenario.' % variablepath
            val = variablenode.getValueAsString()
            node.parentNode.replaceChild(xmldocument.createTextNode(unicode(val)),node)
            node.unlink()

        scenarionodes = xmldocument.getElementsByTagName('gotm:scenario')
        assert len(scenarionodes)<=1, 'Found more than one "gotm:scenario" node in the report template.'
        if len(scenarionodes)>0:
            if callback!=None: callback(istep/steps,'Creating scenario description...')
            scenarionode = scenarionodes[0]

            sceninterface = xmlstore.TypedStoreInterface(scenario,showhidden=False,omitgroupers=True)

            scentable = xmldocument.createElement('table')
            scentable.setAttribute('id','tableScenario')

            totaldepth = sceninterface.getDepth(scenario.root)

            # Create columns.
            for i in range(totaldepth-2):
                col = xmldocument.createElement('col')
                col.setAttribute('width','25')
                scentable.appendChild(col)
            col = xmldocument.createElement('col')
            scentable.appendChild(col)
            col = xmldocument.createElement('col')
            scentable.appendChild(col)

            # Create rows
            for tr in sceninterface.toHtml(scenario.root,xmldocument,totaldepth-1,level=-1,hidedefaults=True):
                scentable.appendChild(tr)

            scenarionode.parentNode.replaceChild(scentable,scenarionode)

        istep += 1

        figuresnodes = xmldocument.getElementsByTagName('gotm:figures')
        assert len(figuresnodes)<=1, 'Found more than one "gotm:figures" node in the report template.'
        if len(figuresnodes)>0:
            figuresnode = figuresnodes[0]
        else:
            figuresnode = None
        if len(plotvariables)>0 and figuresnode!=None:
            mplfigure = matplotlib.figure.Figure(figsize=(figuresize[0]/2.54,figuresize[1]/2.54))
            canvas = matplotlib.backends.backend_agg.FigureCanvasAgg(mplfigure)
            fig = plot.Figure(mplfigure)
            fig.addDataSource('result',self)
            figurestable = xmldocument.createElement('table')
            icurvar = 0
            tr = None
            for pv in plotvariables:
                longname = self.getVariable(pv).getLongName()
                if callback!=None: callback(istep/steps,'Creating figure for %s...' % longname)
                
                if icurvar % columncount == 0:
                    tr = xmldocument.createElement('tr')
                    figurestable.appendChild(tr)
                fig.setUpdating(False)
                fig.clearProperties()
                fig.addVariable(pv)
                fig.setUpdating(True)
                filename = pv+'.png'
                outputfile = os.path.join(outputpath,filename)
                canvas.print_figure(outputfile,dpi=dpi)

                img = xmldocument.createElement('img')
                img.setAttribute('src',filename)
                img.setAttribute('alt',longname)
                img.setAttribute('style','width:%.2fcm' % figuresize[0])
                td = xmldocument.createElement('td')
                td.appendChild(img)
                tr.appendChild(td)

                icurvar = icurvar+1
                istep += 1
            for i in range(columncount - len(tr.childNodes)):
                tr.appendChild(xmldocument.createElement('td'))
            figuresnode.parentNode.replaceChild(figurestable,figuresnode)
        elif figuresnode!=None:
            figuresnode.parentNode.removeChild(figuresnode)
        
        if callback!=None: callback(istep/steps,'Writing HTML...')

        if outputpath!='':
            import codecs
            f = codecs.open(os.path.join(outputpath,'index.html'),'w','utf-8')
            xmldocument.writexml(f,encoding='utf-8')
            f.close()
        else:
            print xmldocument.toxml('utf-8')
        istep += 1

        if callback!=None: callback(istep/steps,'Done.')

