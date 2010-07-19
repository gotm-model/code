import math,os.path,xml.dom.minidom

import matplotlib, matplotlib.colors, matplotlib.dates, matplotlib.font_manager
import numpy

import xmlstore.xmlstore,xmlstore.util,common,expressions

class BaseExporter:
    @classmethod
    def isCompatible(figure):
        return True

    @classmethod
    def getFileTypes(cls,source):
        return {}

    def __init__(self,source,properties=None):
        self.source = source
        self.properties = properties
        
    def export(self,path,format):
        pass

class Exporter(BaseExporter):
    propertyxml = """<?xml version="1.0"?>
<element name="Settings">
  <element name="Width"      label="width"      type="float" unit="cm"/>
  <element name="Height"     label="height"     type="float" unit="cm"/>
  <element name="Resolution" label="resolution" type="int"   unit="dpi"/>
</element>
    """
    
    def __init__(self,source):
        # Create XML stores for default and customized properties.
        self.schema = xmlstore.xmlstore.Schema(Exporter.propertyxml,sourceisxml=True)
        store = xmlstore.xmlstore.TypedStore(self.schema)
        self.defaultstore = xmlstore.xmlstore.TypedStore(self.schema)
        self.defaultstore['Width' ].setValue(source['Width' ].getValue(usedefault=True))
        self.defaultstore['Height'].setValue(source['Height'].getValue(usedefault=True))
        self.defaultstore['Resolution'].setValue(96)
        store.setDefaultStore(self.defaultstore)
        
        BaseExporter.__init__(self,source,store)

    @classmethod
    def getFileTypes(cls,source):
        types = {}
        for ext,name in matplotlib.backend_bases.FigureCanvasBase.filetypes.items():
            types.setdefault(name,[]).append(ext)
        return types

    def export(self,path,format):
        """Export the contents of the figure to file.
        """
        width  = self.properties['Width' ].getValue(usedefault=True)/2.54
        height = self.properties['Height'].getValue(usedefault=True)/2.54
        dpi    = self.properties['Resolution'].getValue(usedefault=True)
        
        # Restore original figure dimensions.
        oldwidth  = self.source.figure.get_figwidth()
        oldheight = self.source.figure.get_figheight()
        self.source.figure.set_figwidth (width)
        self.source.figure.set_figheight(height)

        # Save the figure to file.
        self.source.canvas.print_figure(unicode(path),dpi=dpi,facecolor='w',edgecolor='w',orientation='portrait')
        
        # Restore original figure dimensions.
        self.source.figure.set_figwidth (oldwidth)
        self.source.figure.set_figheight(oldheight)

def calculateContourLevels(cc,zmin,zmax,logscale=False):
    # Choose a contour locator
    if logscale:
        loc = matplotlib.ticker.LogLocator()
    else:
        loc = matplotlib.ticker.MaxNLocator(cc+1)

    # Choose contours (code taken from matplotlib.contour.py, _autolev function)
    loc.create_dummy_axis()
    loc.set_bounds(zmin, zmax)
    lev = loc()
    zmargin = (zmax - zmin) * 0.000001 # so z < (zmax + zmargin)
    if zmax >= lev[-1]:
        lev[-1] += zmargin
    if zmin <= lev[0]:
        if logscale:
            lev[0] = 0.99 * zmin
        else:
            lev[0] -= zmargin
    return lev
            
colormaps,colormaplist = None,None
def getColorMaps():
    global colormaps,colormaplist
    if colormaps is None:
        colormaps,colormaplist = {},[]
        
        def fromModule(mod,prefix=''):
            for strchild in dir(mod):
                member = getattr(mod,strchild)
                if isinstance(member,matplotlib.colors.Colormap):
                    colormaplist.append(prefix+strchild)
                    colormaps[prefix+strchild] = member

        fromModule(matplotlib.cm)
        
        # Try adding additional colormaps from basemap
        basemapcm = None
        try:
            from mpl_toolkits.basemap import cm as basemapcm
        except:
            pass
        if basemapcm is not None: fromModule(basemapcm,prefix='basemap.')
    return colormaps,colormaplist

xmlstore.datatypes.register('fontname',xmlstore.datatypes.String)

class MapProjectionChoice(xmlstore.datatypes.String):
    def toPrettyString(self):
        try:
            import mpl_toolkits.basemap
        except:
            return self
        return mpl_toolkits.basemap._projnames[self]
xmlstore.datatypes.register('mapprojection',MapProjectionChoice)
      
class MergedVariableStore(common.VariableStore):
    """Class that merges multiple data sources (VariableStore objects) with
    the same variables, thus creating a new dimension corresponding to the
    index of the original data source.
    """
    
    class MergedVariable(common.Variable):
        def __init__(self,store,variables,mergedimid):
            common.Variable.__init__(self,store)
            self.vars = variables
            self.mergedimid = mergedimid

        def getName_raw(self):
            return self.vars[0].getName_raw()

        def getLongName(self):
            return self.vars[0].getLongName()

        def getUnit(self):
            return self.vars[0].getUnit()

        def getDimensions_raw(self):
            return tuple([self.mergedimid]+list(self.vars[0].getDimensions_raw()))

        def getSlice(self,bounds):
            slice = self.Slice(self.getDimensions())
            assert len(bounds)==slice.ndim, 'Number of specified dimensions (%i) does not equal number of data dimensions (%i).' % (len(bounds),slice.ndim)
            
            # Get bound indices for the merged dimension
            ifirst,ilast = 0,len(self.vars)-1
            if bounds[0].start is not None and bounds[0].start>ifirst: ifirst = int(math.floor(bounds[0].start))
            if bounds[0].stop  is not None and bounds[0].stop <ilast : ilast  = int(math.ceil (bounds[0].stop))
            slice.coords[0] = numpy.linspace(float(ifirst),float(ilast),ilast-ifirst+1)
            slice.coords_stag[0] = common.getCenters(slice.coords[0],addends=True)

            first = True
            for ivar,var in enumerate(self.vars[ifirst:ilast+1]):
                curslice = var.getSlice(bounds[1:])
                assert curslice is not None, 'Unable to obtain valid slice from variable %s.' % var
                if first:
                    slice.coords[1:] = curslice.coords
                    slice.coords_stag[1:] = curslice.coords_stag
                    slice.data = numpy.ma.array(numpy.empty(tuple([ilast-ifirst+1]+list(curslice.data.shape)),curslice.data.dtype),copy=False)
                    first = False
                slice.data[ivar,...] = curslice.data
                
            return slice

    def __init__(self,stores,mergedimid='obs',mergedimname='observation'):
        common.VariableStore.__init__(self)
        self.stores = stores
        self.mergedimid = mergedimid
        self.mergedimname = mergedimname

    def getVariableNames_raw(self):
        return self.stores[0].getVariableNames_raw()

    def getVariableLongNames_raw(self):
        return self.stores[0].getVariableLongNames()

    def getDimensionInfo_raw(self,dimname):
        if dimname==self.mergedimid: 
            info = common.VariableStore.getDimensionInfo_raw(self,dimname)
            info['label'] = self.mergedimname
            return info
        return self.stores[0].getDimensionInfo_raw(dimname)

    def getVariable_raw(self,varname):
        vars,missing = [],[]
        for store in self.stores:
            if varname in store:
                vars.append(store[varname])
            else:
                missing.append(store)
        if len(vars)==0: raise KeyError()
        assert len(missing)==0, 'The following stores do not contain variable "%s": %s.' % (varname,', '.join(missing))
        return MergedVariableStore.MergedVariable(self,vars,self.mergedimid)
        
class CustomDateFormatter(matplotlib.dates.DateFormatter):
    """Extends the matplotlib.dates.DateFormatter class, adding support
    for the first letter of the day name (%e), the first letter of the
    month name (%n) and the quarter numbers Q1, Q2, Q3, Q4 (%Q).
    """
    def __init__(self,pattern):
        matplotlib.dates.DateFormatter.__init__(self,pattern)

    def strftime(self, dt, fmt):
        if ('%e' in fmt):
            dayname = matplotlib.dates.DateFormatter.strftime(self,dt,'%A')
            fmt = str(fmt.replace('%e',dayname[0]))
        if ('%n' in fmt):
            month = matplotlib.dates.DateFormatter.strftime(self,dt,'%b')
            fmt = str(fmt.replace('%n',month[0]))
        if ('%Q' in fmt):
            monthnr = int(matplotlib.dates.DateFormatter.strftime(self,dt,'%m'))
            fmt = fmt.replace('%Q','Q%i' % math.ceil(monthnr/3.))
        return matplotlib.dates.DateFormatter.strftime(self,dt,fmt)
        
class VariableTransform(common.Variable):
    """Abstract base class for variable transform. By default it inherits
    most properties (unit, dimensions) from the source variable, while the
    original short- and long name are prefixed with a string describing the
    transformation.
    """
    def __init__(self,sourcevar,nameprefix='',longnameprefix='',name=None,longname=None):
        common.Variable.__init__(self,None)
        assert sourcevar is not None, 'The source variable for a transform cannot be None.'
        self.sourcevar = sourcevar
        if name is None:
            name = nameprefix + self.sourcevar.getName()
        if longname is None: 
            longname = longnameprefix + self.sourcevar.getLongName()
        self.name     = name
        self.longname = longname

    def getName_raw(self):
        """Return short name for the variable.
        """
        return self.name

    def getLongName(self):
        """Return long name for the variable.
        """
        return self.longname

    def getUnit(self):
        """Return variable unit, copied form source variable.
        """
        return self.sourcevar.getUnit()

    def getDimensions_raw(self):
        """Return list of variable dimensions, copied form source variable.
        """
        return self.sourcevar.getDimensions_raw()

    def getDimensionInfo_raw(self,dimname):
        """Return information on specified dimension, copied form source
        variable.
        """
        return self.sourcevar.getDimensionInfo_raw(dimname)

class VariableCombine(common.Variable):
    def __init__(self,sourcevars):
        common.Variable.__init__(self,None)
        self.sourcevars = sourcevars

    def getName_raw(self):
        """Return short name for the variable.
        """
        return '_'.join([v.getName_raw() for v in self.sourcevars])

    def getLongName(self):
        """Return long name for the variable.
        """
        return ', '.join([v.getLongName() for v in self.sourcevars])

    def getUnit(self):
        """Return variable unit, copied form source variable.
        """
        units = [v.getUnit() for v in self.sourcevars]
        if len(set(units))==1: return units[0]
        return ', '.join(units)

    def getDimensions_raw(self):
        """Return list of variable dimensions, copied form source variable.
        """
        return self.sourcevars[0].getDimensions_raw()

    def getDimensionInfo_raw(self,dimname):
        """Return information on specified dimension, copied form source
        variable.
        """
        return self.sourcevars[0].getDimensionInfo_raw(dimname) 

    def getSlice(self,bounds):
        return [v.getSlice(bounds) for v in self.sourcevars]

class VariableReduceDimension(VariableTransform):
    """Abstract base class for a variable transform that reduces the number
    of variable dimensions by one (e.g. average, integral, slice).
    """
    def __init__(self,variable,dimension,**kwargs):
        VariableTransform.__init__(self,variable,**kwargs)
        self.dimension = dimension

        # Retrieve the index of the dimension that we want to take out.
        dims = self.sourcevar.getDimensions()
        for (i,d) in enumerate(dims):
            if d==self.dimension: break
        else:
            assert False, 'Dimension "%s" is not present for this variable.' % self.dimension
        self.idimension = i

    def getDimensions_raw(self):
        """Return the variable dimensions, taken from the source variable but
        with one dimension taken out.
        """
        dims = self.sourcevar.getDimensions_raw()
        return [d for d in dims if d!=self.dimension]
        
class VariableSlice(VariableReduceDimension):
    """Transformation that takes a slice through the variable in one dimension.
    Uses linear interpolation to get the values at the sliced position.
    """
    def __init__(self,variable,slicedimension,slicecoordinate,**kwargs):
        VariableReduceDimension.__init__(self,variable,slicedimension,**kwargs)
        self.sliceval = slicecoordinate

    def getSlice(self,bounds):
        newslice = self.Slice(self.getDimensions())

        newbounds = list(bounds)
        newbounds.insert(self.idimension,(self.sliceval,self.sliceval))
        sourceslice = self.sourcevar.getSlice(newbounds)
        if not sourceslice.isValid: return newslice

        assert sourceslice.coords[self.idimension].ndim==1, 'Slicing is not (yet) supported for dimensions that have coordinates that depend on other dimensions.'
        ipos = sourceslice.coords[self.idimension].searchsorted(self.sliceval)
        if ipos==0 or ipos>=sourceslice.coords[self.idimension].shape[0]: return newslice
        leftx  = sourceslice.coords[self.idimension][ipos-1]
        rightx = sourceslice.coords[self.idimension][ipos]
        deltax = rightx-leftx
        stepx = self.sliceval-leftx
        relstep = stepx/deltax

        if len(dims)==1:
            data.pop(self.idimension)
            for idat in range(len(data)):
                if data[idat].ndim==2:
                    if ipos>0 and ipos<len(data[self.idimension]):
                        # centered: left and right bound available
                        left  = data[idat].take((ipos-1,),self.idimension).squeeze()
                        right = data[idat].take((ipos,  ),self.idimension).squeeze()
                        data[idat] = left + relstep*(right-left)
                    elif ipos==0:
                        # left-aligned (only right bound available)
                        data[idat]=data[idat].take((ipos,),self.idimension).squeeze()
                    else:
                        # right-aligned (only left bound available)
                        data[idat]=data[idat].take((ipos-1,),self.idimension).squeeze()
        else:
            assert False,'Cannot take slice because the result does not have 1 coordinate dimension (instead it has %i: %s).' % (len(dims),dims)
        return newslice
            
class FigureProperties(xmlstore.xmlstore.TypedStore):
    """Class for figure properties, based on xmlstore.TypedStore.
    
    Currently this does nothing specific except automatically selecting the
    correct XML schema, and allowing access to schemas based on their short names.
    In the future this class can host convertors that convert between different
    versions of the XML schema for figures.
    """

    @classmethod
    def getSchemaInfo(cls):
        return xmlstore.xmlstore.schemainfocache[os.path.join(common.getDataRoot(),'schemas/figure')]

    def __init__(self,valueroot=None,adddefault = True,schema=None):
        if schema is None: schema = os.path.join(common.getDataRoot(),'schemas/figure/0003.schema')
        xmlstore.xmlstore.TypedStore.__init__(self,schema,valueroot,adddefault=adddefault)
        
    @classmethod
    def getCustomDataTypes(ownclass):
        dt = xmlstore.xmlstore.TypedStore.getCustomDataTypes()
        dt['colormap']   = xmlstore.datatypes.String
        dt['markertype'] = xmlstore.datatypes.String
        dt['linestyle']  = xmlstore.datatypes.String
        return dt

class FigureAnimator(object):
    def __init__(self,figure,dimension):
        self.figure = figure
        self.dimension = dimension
        
        useddims = set()
        icount = 0
        length = None
        for v in self.getPlottedVariables():
            curdims = list(v.getDimensions())
            if self.dimension in curdims:
                idim = curdims.index(self.dimension)
                shape = v.getShape()
                if shape is not None:
                    curlength = shape[idim]
                else:
                    slcs = [0]*len(curdims)
                    slcs[idim] = slice(None)
                    curdat = v.getSlice(slcs,dataonly=True)
                    if isinstance(curdat,(list,tuple)): curdat = curdat[0]
                    curlength = len(curdat)
                if length is None:
                    length = curlength
                else:
                    assert length==curlength,'Animated dimension %s has different lengths %i and %i for the different plotted variables.' % (self.dimension,length,curlength)
                icount += 1
            useddims.update(curdims)
        assert icount>0,'None of the plotted variables uses animated dimension %s (used: %s).' % (self.dimension,', '.join(useddims))
        self.length = length
        
        self.index = -1
        self.titletemplate = None
        
    def getPlottedVariables(self):
        vars = []
        for seriesnode in self.figure.properties['Data'].getLocationMultiple(['Series']):
            varpath = seriesnode.getSecondaryId()
            if varpath=='': continue
            var = self.figure.source[varpath]
            vars.append(var)
        return vars
        
    def nextFrame(self):
        self.index += 1
        oldupdating = self.figure.setUpdating(False)
        self.figure.slices[self.dimension] = self.index
        if self.titletemplate is None and self.index==0:
            self.titletemplate = self.figure['Title'].getValue(usedefault=False)
            if self.titletemplate is not None: self.titletemplate = str(self.titletemplate)
        self.figure['Title'].setValue(self.getDynamicTitle(self.titletemplate))
        self.figure.update()
        self.figure.setUpdating(oldupdating)
        return self.index<(self.length-1)

    def getDynamicTitle(self,fmt):
        for var in self.getPlottedVariables():
            if self.dimension in var.getDimensions(): break
        if isinstance(var,expressions.VariableExpression):
            store = var.variables[0].store
        else:
            store = var.store
        coordvariable = store.getVariable(self.dimension)
        if coordvariable is not None:
            coorddims = list(coordvariable.getDimensions())
            assert len(coorddims)==1,'Only animations through 1D dimensions are currently supported.'
            assert self.dimension in coorddims, 'Coordinate variable %s does not use its own dimension (dimensions: %s).' % (dim,', '.join(coorddims))
            coordslice = [slice(None)]*len(coorddims)
            coordslice[coorddims.index(self.dimension)] = self.index
            meanval = coordvariable.getSlice(coordslice,dataonly=True).mean()
            
            diminfo = var.getDimensionInfo(self.dimension)

            # Convert the coordinate value to a string
            if fmt is None:
                if diminfo.get('datatype','float')=='datetime':
                    fmt = diminfo['label']+': %Y-%m-%d %H:%M:%S'
                else:
                    fmt = diminfo['label']+': %.4f'
            try:
                if diminfo.get('datatype','float')=='datetime':
                    return common.num2date(meanval).strftime(fmt)
                else:
                    return fmt % meanval
            except:
                #raise Exception('Unable to apply format string "%s" to value %s.' % (fmt,meanval))
                return fmt
        
    def animateAndExport(self,path,dpi=75,verbose=True):
        if os.path.isdir(path):
            nametemplate = '%%0%ii.png' % (1+math.floor(math.log10(self.length-1)))
            targetdir = path
        else:
            try:
                path % (1,)
            except:
                raise Exception('"%s" should either be an existing directory, or a file name template that accepts a single integer as formatting argument.' % path)
            nametemplate = path
            targetdir = '.'
                
        while True:            
            hasmore = self.nextFrame()
            self.figure.setUpdating(True)
            if verbose: print 'Creating frame %i of %s...' % (self.index+1,self.length)
            path = os.path.join(targetdir,nametemplate % self.index)
            self.figure.exportToFile(path,dpi=dpi)
            if not hasmore: break

class Figure(xmlstore.util.referencedobject):
    """Class encapsulating a MatPlotLib figure. The data for the figure is
    provided as one or more VariableStore objects, with data series being
    identified by the name of the VariableStore and the name of the variable
    to be plotted. All configuration of the plots is done through a
    xmlstore.TypedStore object.
    """
    exporters = {'image file':Exporter}
    
    @staticmethod
    def registerExporter(name,cls):
        Figure.exporters[name] = cls
        
    def getExporters(self):
        try:
            import georef
        except ImportError:
            pass
        return self.exporters

    def __init__(self,figure=None,defaultfont=None):
        global matplotlib
    
        xmlstore.util.referencedobject.__init__(self)

        # If no MatPlotLib figure is specified, create a new one, assuming
        # we want to export to file.        
        if figure is None:
            import matplotlib.figure, matplotlib.backends.backend_agg
            figure = matplotlib.figure.Figure(figsize=(10/2.54,8/2.54))
            canvas = matplotlib.backends.backend_agg.FigureCanvasAgg(figure)
        
        # Test if the specified default font is known to MatPlotLib.
        # If not, reset the default font to the MatPlotLib default.
        if defaultfont is not None:
            try:
                if matplotlib.font_manager.findfont(defaultfont)==matplotlib.font_manager.findfont(None): defaultfont = None
            except:
                defaultfont = None

        # If no default font is specified, use the MatPlotLib default.
        if defaultfont is None:
            defaultfont = matplotlib.font_manager.FontProperties().get_name()
        
        self.figure = figure
        self.canvas = figure.canvas

        # Create store for the explicitly set properties
        self.properties = FigureProperties()
        self.propertiesinterface = self.properties.getInterface()
        self.propertiesinterface.processDefaultChange = -1
        self.propertiesinterface.connect('afterChange',self.onPropertyChanged)
        self.propertiesinterface.connect('afterStoreChange',self.onPropertyStoreChanged)
        
        # Create store for property defaults
        self.defaultproperties = FigureProperties()

        # Set some default properties.
        setFontProperties(self.defaultproperties['Font'],family=defaultfont,size=8)
        self.defaultproperties['FontScaling'    ].setValue(100)
        self.defaultproperties['Legend/Location'].setValue('best')
        self.defaultproperties['HasColorMap'    ].setValue(False)
        self.defaultproperties['ColorMap'       ].setValue('jet')
        self.defaultproperties['BackgroundColor'].setValue(xmlstore.datatypes.Color(255,255,255))
        setLineProperties(self.defaultproperties['Grid/LineProperties'],CanHaveMarker=False,mplsection='grid')
        setFontProperties(self.defaultproperties['Title/Font'],family=defaultfont,size=10)
        
        nodePadding = self.defaultproperties['Padding']
        nodePadding['Left'  ].setValue(.125)
        nodePadding['Right' ].setValue(.1)
        nodePadding['Top'   ].setValue(.1)
        nodePadding['Bottom'].setValue(.1)

        nodemap = self.defaultproperties['Map']
        nodemap.setValue(False)
        nodemap['Projection' ].setValue('cyl')
        nodemap['Resolution' ].setValue('c')
        nodemap['DrawCoastlines'].setValue(True)
        nodemap['DrawCoastlines/Color'].setValue(xmlstore.datatypes.Color(0,0,0))
        nodemap['DrawCoastlines/LineWidth'].setValue(1.)
        nodemap['FillContinents'].setValue(True)
        nodemap['FillContinents/Color'].setValue(xmlstore.datatypes.Color(255,255,255))
        nodemap['FillContinents/LakeColor'].setValue(xmlstore.datatypes.Color(255,255,255))
        nodemap['DrawMapBoundary'].setValue(True)
        nodemap['DrawMapBoundary/Color'].setValue(xmlstore.datatypes.Color(0,0,0))
        nodemap['DrawMapBoundary/LineWidth'].setValue(1.)
        nodemap['DrawRivers'].setValue(False)
        nodemap['DrawRivers/Color'].setValue(xmlstore.datatypes.Color(0,0,0))
        nodemap['DrawRivers/LineWidth'].setValue(.5)
        nodemap['DrawCountries'].setValue(False)
        nodemap['DrawCountries/Color'].setValue(xmlstore.datatypes.Color(0,0,0))
        nodemap['DrawCountries/LineWidth'].setValue(.5)
        nodemap['DrawStates'].setValue(False)
        nodemap['DrawStates/Color'].setValue(xmlstore.datatypes.Color(0,0,0))
        nodemap['DrawStates/LineWidth'].setValue(.5)
        nodemap['DrawParallels'].setValue(False)
        nodemap['DrawParallels/Color'].setValue(xmlstore.datatypes.Color(0,0,0))
        nodemap['DrawParallels/LineWidth'].setValue(.5)
        nodemap['DrawMeridians'].setValue(False)
        nodemap['DrawMeridians/Color'].setValue(xmlstore.datatypes.Color(0,0,0))
        nodemap['DrawMeridians/LineWidth'].setValue(.5)

        # Take default figure size from value at initialization
        w,h = self.figure.get_size_inches()
        self.defaultproperties['Width'          ].setValue(w*2.54)
        self.defaultproperties['Height'         ].setValue(h*2.54)

        # Attach the store with figure defaults to the customized store.
        self.properties.setDefaultStore(self.defaultproperties)

        self.source = common.VariableStore()
        self.defaultsource = None
        self.updating = True
        self.dirty = False
        self.haschanged = False
        self.errors = []
        
        # Whether to automatically squeeze out singleton dimensions in the data to plot.
        self.autosqueeze = True
        
        self.callbacks = {'completeStateChange':[]}
        
        # Cache for subset of figure objects
        self.basemap = None
        self.colorbar = None
        self.ismap = False
        
        # Slices to take through the plotted data (dimension name -> index)
        self.slices = {}

    def __getitem__(self,key):
        return self.properties[key]
        
    def unlink(self):
        """Cleans up the figure, releasing the embedded TypedStore objects.
        """
        self.properties.disconnectInterface(self.propertiesinterface)
        self.propertiesinterface = None
        
        self.defaultproperties.release()
        self.defaultproperties = None
        self.properties.release()
        self.properties = None
        
    def registerCallback(self,eventname,callback):
        assert eventname in self.callbacks, 'Event "%s" is unknown.' % eventname
        self.callbacks[eventname].append(callback)

    def setUpdating(self,allowupdates):
        """Enables/disables updating of the figure as its properties change.
        """
        oldval = self.updating
        if oldval != allowupdates:
            self.updating = allowupdates
            if allowupdates and self.dirty: self.update()
        return oldval

    def onPropertyChanged(self,node,feature):
        """Called internally after a property in the TypedStore with customized
        figure settings has changed.
        """
        if feature=='value':
            self.onPropertyStoreChanged()

    def onPropertyStoreChanged(self):
        """Called internally after all properties in the TypedStore with
        customized figure settings have changed at once.
        """
        self.haschanged = True
        self.update()

    def clearSources(self):
        """Clears the list of VariableStore data sources currently registered
        with the figure.
        """
        self.source.removeAllChildren()
        self.defaultsource = None

    def addDataSource(self,name,obj):
        """Adds a VariableStore data source to the figure, using the specified
        name.
        """
        self.source.addChild(obj,name)
        if self.defaultsource is None: self.defaultsource = name

    def removeDataSource(self,name):
        """Removes a VariableStore data source from the figure, using the specified
        name.
        """
        if self.defaultsource==name: self.defaultsource = None
        return self.source.removeChild(name)
        
    def getDataSources(self):
        return self.source.children

    def clearProperties(self,deleteoptional=True):
        """Clear all customized figure properties (which means defaults will be used).
        If deleteoptional is set to True, optional nodes such as data series will be
        deleted as well, resulting in an empty figure.
        """
        self.properties.root.clearValue(recursive=True,deleteclones=deleteoptional)

    def setProperties(self,props):
        """Specifies a new set of custom figure properties.
        The properties can be specified as path to an XML file, an in-memory
        XML node, among others.
        """
        if isinstance(props,(basestring,xmlstore.datatypes.DataFile)):
            # Load properties from file - versions may differ and will be converted automatically
            self.properties.load(props)
        else:
            # Load from XML node or document - versions must be identical
            self.properties.setStore(props)
        self.update()

    def getPropertiesCopy(self):
        """Get a copy of the current figure properties as XML node.
        """
        return self.properties.toXmlDom()

    def clearVariables(self):
        """Clears all data series. This does not automatically clear the
        list of registered data sources (VariableStore objects).
        """
        self.properties['Data'].removeChildren('Series')

    def addVariable(self,varname,source=None,replace=True):
        """Add a variable to the figure. If no data source name if specified,
        the first registered source will be used. The specified variable must
        match the name of a variable in the data source to be used.
        """
        assert source is None or isinstance(source,basestring), 'If the "source" option is specified, it must be a string.'
        if source is None: source = self.defaultsource
        datanode = self.properties['Data']
        varname = self.source.normalizeExpression(varname,source)
        if replace:
            self.defaultproperties['Data'].getChildById('Series',varname,create=True)
            series = datanode.getChildById('Series',varname,create=True)
        else:
            self.defaultproperties['Data'].addChild('Series',id=varname)
            series = datanode.addChild('Series',id=varname)
        self.update()
        return series

    def hasChanged(self):
        """Returns True if the figure properties have changed since the store
        was created or resetChanged was called.
        """
        return self.haschanged

    def resetChanged(self):
        """Resets the "changed" status of the figure properties.
        """
        self.haschanged = False
        
    def exportToFile(self,path,width=None,height=None,dpi=150):
        """Export the contents of the figure to file.
        """
        # Restore original figure dimensions.
        if width  is not None: self.figure.set_figwidth (width)
        if height is not None: self.figure.set_figheight(height)

        # Save the figure to file.
        self.canvas.print_figure(unicode(path),dpi=dpi,facecolor='w',edgecolor='w',orientation='portrait')
        
        # Restore original figure dimensions.
        if width  is not None: self.figure.set_figwidth (self['Width'].getValue(usedefault=True)/2.54)
        if height is not None: self.figure.set_figheight(self['Height'].getValue(usedefault=True)/2.54)
        
    def copyFrom(self,sourcefigure):
        """Copies all plot properties and data sources from the supplied source figure.
        """
        properties = sourcefigure.getPropertiesCopy()
        for name,child in sourcefigure.source.children.iteritems():
            self.source.addChild(child,name)
        self.defaultsource = sourcefigure.defaultsource
        self.setProperties(properties)
                
    def update(self):
        """Update the figure.
        
        Everything happens here. The current set of customized properties is
        interpreted, data slices are obtained from the data sources, default
        figure properties are set based on properties of the obtained data,
        and the figure is built and shown.
        """
        # We are called whenever figure properties change. If we do not want to update now,
        # just register that an update is needed and exit.
        if not self.updating:
            self.dirty = True
            return
            
        # If no target figure is supplied, we will update ourselves.
        validplot = self.draw()
        
        # Draw the plot to screen.
        self.canvas.draw()
        
        # Notify attached objects that the figure state may have changed.
        for cb in self.callbacks['completeStateChange']: cb(validplot)

        # Store that the figure now reflects all current settings.
        self.dirty = False

    def draw(self,figure=None):
        # Below: code for debugging superfluous plot updates (sends stack trace to stdout)
        #import traceback
        #print '--- stack for call to update ---'   
        #trace = traceback.format_list(traceback.extract_stack(limit=10))    
        #for l in trace: print l,
        
        if figure is None: figure = self.figure
            
        # Clear the current MatPlotLib figure.
        figure.clear()
        self.errors = []
        
        w = self.properties['Width'].getValue(usedefault=True)
        h = self.properties['Height'].getValue(usedefault=True)
        figure.set_size_inches(w/2.54,h/2.54)
                
        # Obtain text scaling property (from % to fraction)
        textscaling = self.properties['FontScaling'].getValue(usedefault=True)/100.
        
        # First scale the default font size; this takes care of all relative font sizes (e.g. "small")
        #matplotlib.font_manager.fontManager.set_default_size(textscaling*matplotlib.rcParams['font.size'])
        
        # Get default font properties
        fontpropsdict = getFontProperties(self.properties['Font'],textscaling=textscaling)
        fontprops = matplotlib.font_manager.FontProperties(**fontpropsdict)

        fontpropsdict_title = dict(fontpropsdict)
        fontpropsdict_title['size'] = round(1.25*fontpropsdict_title['size'])
        setFontProperties(self.defaultproperties['Title/Font'],**fontpropsdict_title)
            
        # Line colors to cycle through
        linecolors = ((0,0,255),(0,255,0),(255,0,0),(0,255,255),(255,0,255),(255,255,0),(0,0,0))

        # Get forced axes boundaries (will be None if not set; then we autoscale)
        # These boundaries are retrieved before data are obtained, in order to be able
        # to skip reading out unnecessary data (which currently does not work!).
        axis2data = {}
        defaultaxes = self.defaultproperties['Axes']
        forcedaxes = self.properties['Axes']
        for forcedaxis in forcedaxes.getLocationMultiple(['Axis']):
            istimeaxis = forcedaxis['IsTimeAxis'].getValue(usedefault=True)
            logscale = False
            if istimeaxis:
                axmin = forcedaxis['MinimumTime'].getValue()
                axmax = forcedaxis['MaximumTime'].getValue()
                if axmin is not None: axmin = common.date2num(axmin)
                if axmax is not None: axmax = common.date2num(axmax)
            else:
                axmin = forcedaxis['Minimum'].getValue()
                axmax = forcedaxis['Maximum'].getValue()
                logscale = forcedaxis['LogScale'].getValue()
            axis2data[forcedaxis.getSecondaryId()] = {'forcedrange':[axmin,axmax],'logscale':logscale}

        # Shortcuts to the nodes specifying the series to plot.
        forcedseries = self.properties['Data'].getLocationMultiple(['Series'])

        # Shortcut to the node that will hold defaults for the plotted series.
        defaultdatanode = self.defaultproperties['Data']
        olddefaults = [node.getSecondaryId() for node in defaultdatanode.getLocationMultiple(['Series'])]

        # This variable will hold all long names of the plotted variables.
        # These will later be joined to create the plot title.
        titles = []
        
        # No colorbar created, and no colormap used (yet).
        cb = None
        hascolormap = False
        
        # Obtain the currently selected colormap, and make sure NaNs are plotted as white.
        cmdict,cmlist = getColorMaps()
        cm = cmdict[self.properties['ColorMap'].getValue(usedefault=True)]
        cm.set_bad('k',alpha=0.0)
                
        # Start with z order index 0 (incrementing it with every item added)
        zorder = 0

        # Default projection: rectilinear (= no transformation)
        projection = 'rectilinear'
        
        # Dictionary holding number of data series per number of independent dimensions.
        plotcount = {1:0,2:0}
        
        # Dictionary with legend information (handles to drawn data series and the series
        # label) to be filled while adding data series.
        legenddata = {'handles':[],'labels':[]}

        seriesslices,seriesvariables,seriesinfo = [],[],[]
        for iseries,seriesnode in enumerate(forcedseries):
            # Get the path of the data source (data source identifier + variable id)
            varpath = seriesnode.getSecondaryId()
            if varpath=='':
                print 'Skipping data series %i because the secondary node id (i.e., variable source and name) is not set.' % iseries
                continue
                
            var = self.source[varpath]
            itemcount = var.getItemCount()
            assert itemcount>0, 'No variable expression recognized in "%s".' % varpath
            assert itemcount<=2, 'Plots with more than two dependent variables are not supported yet.'
            longname = var.getLongName()
            
            # Create default series information
            defaultseriesnode = defaultdatanode.getChildById('Series',varpath,create=True)
            defaultseriesnode['Label'].setValue(longname)
            defaultseriesnode['PlotType3D'].setValue(0)
            defaultseriesnode['PlotTypeTwoVariable'].setValue(0)
            defaultseriesnode['HasConfidenceLimits'].setValue(False)
            setLineProperties(defaultseriesnode['LineProperties'])
            defaultseriesnode['ShowEdges'].setValue(False)
            defaultseriesnode['UseColorMap'].setValue(True)
            defaultseriesnode['EdgeColor'].setValue(xmlstore.datatypes.Color(0,0,0))
            defaultseriesnode['EdgeWidth'].setValue(1.)
            defaultseriesnode['ArrowColor'].setValue(xmlstore.datatypes.Color(0,0,0))
            defaultseriesnode['ArrowPivot'].setValue('tail')
            defaultseriesnode['ArrowKey'].setValue(True)
            defaultseriesnode['ArrowKey/Label'].setValue('arrow')
            defaultseriesnode['ArrowKey/LabelPosition'].setValue('N')
            defaultseriesnode['ArrowKey/X'].setValue(.9)
            defaultseriesnode['ArrowKey/Y'].setValue(.1)
            
            # Old defaults will be removed after all series are plotted.
            # Register that the current variable is active, ensuring its default will remain.
            if varpath in olddefaults: olddefaults.remove(varpath)

            # Build list of dimension boundaries for current variable.
            originaldims = list(var.getDimensions())
            dimbounds = [slice(None)]*len(originaldims)
            
            # Apply slices (if any)
            for dim,index in self.slices.iteritems():
                if dim in originaldims:
                    dimbounds[originaldims.index(dim)] = index
                    
            #for dimname in originaldims:
            #    if dimname in dim2data:
            #        # We have boundaries set on the current dimension.
            #        forcedrange = dim2data[dimname].get('forcedrange',(None,None))
            #        if forcedrange[0] is not None: forcedrange[0] = forcedrange[0]
            #        if forcedrange[1] is not None: forcedrange[1] = forcedrange[1]
            #        if forcedrange[0]==forcedrange[1] and forcedrange[0] is not None:
            #            # Equal upper and lower boundary: take a slice.
            #            var = VariableSlice(var,dimname,forcedrange[0])
            #        else:
            #            dimbounds.append(slice(forcedrange[0],forcedrange[1]))
            #    else:
            #        # No boundaries set.
            #        dimbounds.append(slice(None))
                    
            # Get the data
            varslices = var.getSlice(tuple(dimbounds))
            if not isinstance(varslices,(list,tuple)):
                varslices = [varslices]
            elif len(varslices)==1 and isinstance(varslices[0],(list,tuple)):
                varslices = varslices[0]
            assert len(varslices)>0, 'Unable to retrieve any variable slices.'
            
            # Skip this variable if (parts of) its data are unavailable.
            valid = True
            for varslice in varslices:
                if (not varslice.isValid()) or 0 in varslice.data.shape:
                    valid = False
                    break
            if not valid: continue

            # Basic checks: eliminate singleton dimensions and mask invalid values.
            for i in range(len(varslices)):
                # Eliminate singleton dimensions (singleton dimension: dimension with length one)
                # Store singleton dimensions as fixed extra coordinates.
                if self.autosqueeze: varslices[i] = varslices[i].squeeze()
                
                # Mask infinite/nan values, if any - only do this if the array is not masked
                # already, because it seems isfinite is not supported on masked arrays.
                if not hasattr(varslices[i].data,'_mask'):
                    invalid = numpy.logical_not(numpy.isfinite(varslices[i].data))
                    if invalid.any():
                        varslices[i].data = numpy.ma.masked_where(invalid,varslices[i].data,copy=False)

            # Get the number of dimensions from the data slice, and add it to the plot properties.
            typ = varslices[0].ndim
            if len(varslices)>1:
                if typ==2:
                    # Vector or wind rose plot
                    typ = 3
                else:
                    # Wind rose plot
                    typ = 4
            defaultseriesnode['Type'].setValue(typ)

            # Get the plot type for 3D plots.
            plottype3d = seriesnode['PlotType3D'].getValue(usedefault=True)
            plottype2var = seriesnode['PlotTypeTwoVariable'].getValue(usedefault=True)
            if projection=='rectilinear' and ((typ==3 and plottype2var==1) or typ==4):
                if iseries>0: raise Exception('Series %i requires a wind rose projection, but earlier series use a rectilinear projection. Cannot combine both in the same plot.' % iseries)
                projection = 'windrose'
            elif projection=='windrose':
                raise Exception('Series %i requires a rectilinear projection, but earlier series use a wind rose projection. Cannot combine both in the same plot.' % iseries)

            # We use a staggered grid (coordinates at interfaces,
            # values at centers) for certain 3D plot types.
            staggered = (len(varslices)==1 and varslices[0].ndim==2 and plottype3d==0)
            
            # Create shortcut to applicable coordinate set.
            if staggered:
                coords = varslices[0].coords_stag
            else:
                coords = varslices[0].coords

            # Get the minimum and maximum values; store these as default.
            vardata = {'label':var.getLongName(),
                       'unit':var.getUnit(),
                       'datatype':'float',
                       'tight':False,
                       'reversed':False,
                       'datarange':[varslices[0].data.min(),varslices[0].data.max()]}
            if hasattr(vardata['datarange'][0],'_mask') and vardata['datarange'][0]._mask: vardata['datarange'] = [None,None]
                
            # Now determine the axes ranges
            varslice = varslices[0]
            info = {}
            X,Y,U,V,C,wd = None,None,None,None,None,None
            xinfo,yinfo,cinfo = None,None,None
            if projection=='windrose':
                # Get flattened direction and speed, and eliminate masked values (if any)
                wd,C = varslices[0].data.ravel(),varslices[1].data.ravel()
                if hasattr(C,'_mask'): wd,C = wd.compressed(),C.compressed()
                
                # If all data were masked, were are now left with an empty array.
                # Detect this, and if it is the case, continue with other data series.
                if 0 in wd.shape: continue
                
                cname = 'speed'
                cinfo = {'label':'speed',
                         'unit':var.getUnit(),
                         'datatype':'float',
                         'tight':False,
                         'reversed':False,
                         'datarange':[C.min(),C.max()]}
            elif varslice.ndim==1:
                # One coordinate dimension
                X,Y = varslice.coords[0], varslice.data
                xinfo = var.getDimensionInfo(varslice.dimensions[0])
                yinfo = vardata
                switchaxes = xinfo['preferredaxis']=='y'
                xname,yname = varslice.dimensions[0], varpath
                if switchaxes:
                    X,Y = Y,X
                    xinfo,yinfo = yinfo,xinfo
                    xname,yname = yname,xname
                info['switchaxes'] = switchaxes
            elif varslice.ndim==2:
                # Two coordinate dimensions
                
                # Determine which independent dimension to allocate to which axis.
                xdim,ydim = 0,1
                if var.hasReversedDimensions(): xdim,ydim = 1,0
                xname,yname = varslice.dimensions[xdim],varslice.dimensions[ydim]
                xinfo,yinfo = var.getDimensionInfo(xname),var.getDimensionInfo(yname)
                xpref,ypref = xinfo['preferredaxis'],yinfo['preferredaxis']
                if (xpref=='y' and ypref!='y') or (ypref=='x' and xpref!='x'):
                    # One independent dimension prefers to switch axis and the other does not disagree.
                    xdim,ydim = ydim,xdim
                    xname,yname = yname,xname
                    xinfo,yinfo = yinfo,xinfo
                    
                # Get the coordinates
                X,Y = coords[xdim],coords[ydim]

                # Get length of coordinate dimensions. Coordinates can be provided as vectors
                # valid over the whole domain, or as n-D array that match the shape of the values.
                if X.ndim==1:
                    xlength = X.shape[0]
                else:
                    xlength = X.shape[xdim]
                if Y.ndim==1:
                    ylength = Y.shape[0]
                else:
                    ylength = Y.shape[ydim]
                    
                # Adjust X dimension (make sure it is 2D)
                if X.ndim==1:
                    X = X.reshape((1,-1)).repeat(ylength, 0)
                elif xdim<ydim:
                    X = X.transpose()
                    
                # Adjust Y dimension (make sure it is 2D)
                if Y.ndim==1:
                    Y = Y.reshape((-1,1)).repeat(xlength, 1)
                elif xdim<ydim:
                    Y = Y.transpose()

                # Get the values to plot
                if len(varslices)==1:
                    cinfo = vardata
                    cname = varpath
                    C = varslice.data
                else:
                    assert len(varslices)==2,'Only plots with one or two dependent variables are currently supported.'
                    U = varslices[0].data
                    V = varslices[1].data
                    defaultseriesnode['UseColorMap'].setValue(False)
                    if seriesnode['UseColorMap'].getValue(usedefault=True):
                        C = numpy.sqrt(U*U+V*V)
                        cname = 'arrowlength'
                        cinfo = {'label':'arrow length',
                                 'unit':'',
                                   'datatype':'float',
                                   'tight':False,
                                   'reversed':False,
                                   'datarange':[C.min(),C.max()]}
                        if hasattr(cinfo['datarange'][0],'_mask') and cinfo['datarange'][0]._mask: cinfo['datarange'] = [None,None]
                    
                # Transpose values if needed
                if xdim<ydim:
                    if C is not None: C = C.transpose()
                    if U is not None: U,V = U.transpose(),V.transpose()
                
            if X  is not None: info['x' ] = X
            if Y  is not None: info['y' ] = Y
            if wd is not None: info['wd'] = wd
            if C  is not None: info['C' ] = C
            if U  is not None: info['U' ] = U
            if V  is not None: info['V' ] = V
            if xinfo is not None:
                axis2data.setdefault('x',{'forcedrange':[None,None]}).update(xinfo)
                axis2data['x'].setdefault('dimensions',[]).append(xname)
            if yinfo is not None:
                axis2data.setdefault('y',{'forcedrange':[None,None]}).update(yinfo)
                axis2data['y'].setdefault('dimensions',[]).append(yname)
            if cinfo is not None:
                axis2data.setdefault('colorbar',{'forcedrange':[None,None]}).update(cinfo)
                axis2data['colorbar'].setdefault('dimensions',[]).append(cname)
                    
            seriesvariables.append(var)
            seriesslices.append(varslices)
            seriesinfo.append(info)
            
        # Remove unused dimensions (recognizable by the lack of attributes such as "datatype")
        for axisname in axis2data.keys():
            if 'datatype' not in axis2data[axisname]: del axis2data[axisname]

        def getrange(seriesinfo,axis):
            range = [None,None]
            for info in seriesinfo:
                if axis in info and not (hasattr(info[axis],'_mask') and numpy.all(info[axis]._mask)):
                    curmin,curmax = info[axis].min(),info[axis].max()
                    if range[0] is None or curmin<range[0]: range[0] = curmin
                    if range[1] is None or curmax>range[1]: range[1] = curmax
            return range
        xrange = getrange(seriesinfo,'x')
        yrange = getrange(seriesinfo,'y')

        # Load alternative projection if needed.
        if projection=='windrose':
            try:
                import windrose
            except ImportError:
                raise Exception('This plot is configured to display a wind rose, but the Python package "windrose" is not found. Please install this first.')
            windrose.WindroseAxes.name = 'windrose'
            matplotlib.projections.register_projection(windrose.WindroseAxes)

        # Create one subplot only.
        bg = self.properties['BackgroundColor'].getValue(usedefault=True)
        nodePadding = self.properties['Padding']
        padLeft   = nodePadding['Left'  ].getValue(usedefault=True)
        padRight  = nodePadding['Right' ].getValue(usedefault=True)
        padTop    = nodePadding['Top'   ].getValue(usedefault=True)
        padBottom = nodePadding['Bottom'].getValue(usedefault=True)
        figure.subplots_adjust(left=padLeft,right=1.-padRight,top=1.-padTop,bottom=padBottom)
        axes = figure.add_subplot(111,axis_bgcolor=bg.getNormalized(),projection=projection)

        # Handle transformations due to map projection (if any)
        xcanbelon = projection=='rectilinear' and xrange[0] is not None and xrange[0]>=-361 and xrange[1]<=361
        ycanbelat = projection=='rectilinear' and yrange[0] is not None and yrange[0]>=-91  and yrange[1]<=91
        if xcanbelon and ycanbelat:
            # Try importing basemap
            try:
                import mpl_toolkits.basemap
            except ImportError:
                xcanbelon,ycanbelat = False,False
        self.defaultproperties['CanBeMap'].setValue(xcanbelon and ycanbelat)
        ismap = xcanbelon and ycanbelat and self.properties['Map'].getValue(usedefault=True)
        drawaxes = axes
        if ismap:
            # Create the basemap object
            nodemap = self.properties['Map']
            defnodemap = self.defaultproperties['Map']

            # Determine whether the map is global (approximately 180 degrees latitude, 360 degrees longitude)
            isglobal = False
            for info in seriesinfo:
                if 'x' not in info: continue
                lons = info['x']
                missinglon = 360.-(lons[:,-1]-lons[:,0])
                if (missinglon<2*360/lons.shape[1]).all():
                    isglobal = True
                    break
            defnodemap['IsGlobal'].setValue(isglobal)

            if isglobal:
                # Shift longitude grid where needed
                defnodemap['LeftLongitude'].setValue(xrange[0])
                leftlon = nodemap['LeftLongitude'].getValue(usedefault=True)
                leftlon = xrange[0] + (leftlon-xrange[0]) % 360.
                for info in seriesinfo:
                    if 'x' not in info or 'y' not in info: continue
                    lons = info['x']
                    missinglon = 360.-(lons[:,-1]-lons[:,0])

                    # If we use staggered coordinates, the total map width may exceed 360 degrees longitude.
                    # If this happens, correct it.
                    addcyclic = (missinglon>0.).any()
                    if (missinglon<0.).any() and (lons[:,-2]-lons[:,0]<360.).all():
                        # The first and final cell overlap, but the first and one-but-last do not.
                        # Assume overlap is due to numerical artifacts, and adjust the rightmost coordinate.
                        #print 'synchronizing left and right boundary because longitude overlaps for %i latitudes' % (missinglon<0.).sum()
                        lons[:,-1] = lons[:,0]+360.-1.e-4
                        addcyclic = False
                    
                    # Add cyclic boundary point and shift longitude grid if needed.
                    if leftlon!=xrange[0]:
                        i0 = numpy.fabs(lons-leftlon).argmin(axis=1)
                        assert (i0==i0[0]).all(),'Cannot shift map because new leftmost longitude index varies with latitude.'
                        i0 = i0[0]
                        if i0==lons.shape[1]-1: i0 = 0
                        for k in 'xyUVC':
                            if k not in info or i0==0: continue
                            if addcyclic:
                                #print 'Adding cyclic point for '+k
                                info[k] = numpy.ma.concatenate((info[k],info[k].take((0,),axis=1)),axis=1)
                                if k=='x': info[k][:,-1] += 360.-1.e-4
                            if hasattr(info[k],'mask'):
                                newdata = numpy.ma.empty_like(info[k])
                            else:
                                newdata = numpy.empty_like(info[k])
                            n = newdata.shape[1]
                            newdata[:,:n-i0] = info[k][:,i0:]
                            newdata[:,n-i0:] = info[k][:,1:i0+1] # Skip very first (cyclic) point, and add a new cyclic point
                            if k=='x' and i0!=0:
                                newdata[:,n-i0:] += 360.
                                newdata[:,-1] -= 1.e-4
                            info[k] = newdata
                xrange = getrange(seriesinfo,'x')
                            
            res  = nodemap['Resolution'].getValue(usedefault=True)
            proj = nodemap['Projection'].getValue(usedefault=True)
            if isglobal:
                lonrange = [xrange[0],xrange[0]+360.]
                if lonrange[0]<-360:
                    lonrange[0],lonrange[1] = lonrange[0]+360,lonrange[1]+360
                elif lonrange[1]>720:
                    lonrange[0],lonrange[1] = lonrange[0]-360,lonrange[1]-360
                latrange = [-90,90]
            else:
                lonrange = [max(-360,xrange[0]-0.5),min(720,xrange[1]+0.5)]
                latrange = [max(- 90,yrange[0]-0.5),min( 90,yrange[1]+0.5)]
            defnodemap['Range/LowerLeftLatitude'].setValue(latrange[0])
            defnodemap['Range/LowerLeftLongitude'].setValue(lonrange[0])
            defnodemap['Range/UpperRightLatitude'].setValue(latrange[1])
            defnodemap['Range/UpperRightLongitude'].setValue(lonrange[1])
            llcrnrlon=nodemap['Range/LowerLeftLongitude' ].getValue(usedefault=True)
            llcrnrlat=nodemap['Range/LowerLeftLatitude'  ].getValue(usedefault=True)
            urcrnrlon=nodemap['Range/UpperRightLongitude'].getValue(usedefault=True)
            urcrnrlat=nodemap['Range/UpperRightLatitude' ].getValue(usedefault=True)
            if (self.basemap is None or self.basemap.projection!=proj or self.basemap.resolution!=res or 
               self.basemap.llcrnrlon!=llcrnrlon or self.basemap.llcrnrlat!=llcrnrlat or self.basemap.urcrnrlon!=urcrnrlon or self.basemap.urcrnrlat!=urcrnrlat):
                # Basemap object does not exist yet or has different settings than needed - create a new basemap.
                self.basemap = mpl_toolkits.basemap.Basemap(llcrnrlon=llcrnrlon,
                                                       llcrnrlat=llcrnrlat,
                                                       urcrnrlon=urcrnrlon,
                                                       urcrnrlat=urcrnrlat,
                                                       projection=proj,
                                                       resolution=res,
                                                       ax=axes,
                                                       suppress_ticks=False,
                                                       lon_0=(xrange[0]+xrange[1])/2.,
                                                       lat_0=(yrange[0]+yrange[1])/2.)
            else:
                # A matching basemap object exists: just set its axes and continue.
                self.basemap.ax = axes
            basemap = self.basemap
            drawaxes = basemap

            # Transform x,y coordinates
            for info in seriesinfo:
                if 'x' in info and 'y' in info:
                    if 'U' in info and 'V' in info:
                        info['U'],info['V'],info['x'],info['y'] = basemap.rotate_vector(info['U'],info['V'],info['x'],info['y'],returnxy=True)
                    else:
                        info['x'],info['y'] = basemap(info['x'],info['y'])
            axis2data['x'].update({'unit':'','label':'','hideticks':True})
            axis2data['y'].update({'unit':'','label':'','hideticks':True})

        for seriesnode,var,varslices,info in zip(forcedseries,seriesvariables,seriesslices,seriesinfo):
            varpath = seriesnode.getSecondaryId()
            defaultseriesnode = defaultdatanode.getChildById('Series',varpath,create=False)

            # Store the [default or custom] variable long name; it will be used for building the plot title.
            label = seriesnode['Label'].getValue(usedefault=True)
            titles.append(label)

            # Find axes ranges
            for axisname in ('x','y'):
                if axisname not in info: continue
                curcoords = info[axisname]
                
                # Get minimum and maximum coordinates.
                if curcoords.ndim==1:
                    # Coordinates provided as vector (1D) valid over whole domain.
                    datamin = curcoords[0]
                    datamax = curcoords[-1]
                else:
                    # Coordinates are provided as multidimensional array, with a value for every
                    # coordinate (data point) in the domain. We assume that for a given point
                    # in the space of the other coordinates, the current cordinate increases
                    # monotonously (i.e., position 0 holds the lowest value and position -1 the
                    # highest)
                    #datamin = curcoords.take((0, ),idim).min()
                    #datamax = curcoords.take((-1,),idim).max()

                    datamin = curcoords.min()
                    datamax = curcoords.max()
                    
                if hasattr(datamin,'_mask'): datamin,datamax = None,None

                # Update effective dimension bounds                    
                effrange = axis2data.setdefault(axisname,{}).setdefault('datarange',[None,None])
                if effrange[0] is None or datamin<effrange[0]: effrange[0] = datamin
                if effrange[1] is None or datamax>effrange[1]: effrange[1] = datamax

            varslice = varslices[0]
            curhascolormap = False
            
            # Plot the data series
            if projection=='windrose':
                wd,ws = info['wd'],info['C']

                # Set default properties for polar rose
                defaultseriesnode['RoseBinCount'].setValue(7)
                defaultseriesnode['ShowEdges'].setValue(True)
                defaultseriesnode['PolarRoseType'].setValue(0)
                defaultseriesnode['Rotate180'].setValue(False)
                defaultseriesnode['SectorCount'].setValue(16)
                defaultseriesnode['RoseSectorSpacing'].setValue(0.2)
                defaultseriesnode['NormalizeCounts'].setValue(False)

                # Calculate velocity bins
                wsrange = list(axis2data.get('colorbar',{}).get('forcedrange',[None,None]))
                wsmin,wsmax = wsrange
                if wsmin is None: wsmin = ws.min()
                if wsmax is None: wsmax = ws.max()
                realbins = calculateContourLevels(seriesnode['RoseBinCount'].getValue(usedefault=True), wsmin, wsmax)
                bins = realbins[:-1]
                
                # Get active properties for polar rose
                rosetype = seriesnode['PolarRoseType'].getValue(usedefault=True)
                nsector = seriesnode['SectorCount'].getValue(usedefault=True)
                opening = 1.-seriesnode['RoseSectorSpacing'].getValue(usedefault=True)
                norm = seriesnode['NormalizeCounts'].getValue(usedefault=True)
                blowto = seriesnode['Rotate180'].getValue(usedefault=True)
                edgewidth = seriesnode['EdgeWidth'].getValue(usedefault=True)
                edgecolor = seriesnode['EdgeColor'].getValue(usedefault=True).toXmlString(empty='None')
                if rosetype!=2 and not seriesnode['ShowEdges'].getValue(usedefault=True): edgecolor = 'None'
                
                # Draw polar rose
                if rosetype==0:
                    pc = axes.bar(wd, ws, bins=bins, cmap=cm, normed=norm, nsector=nsector, blowto=blowto, edgecolor=edgecolor, linewidth=edgewidth, opening=opening)
                elif rosetype==1:
                    pc = axes.box(wd, ws, bins=bins, cmap=cm, normed=norm, nsector=nsector, blowto=blowto, edgecolor=edgecolor, linewidth=edgewidth)
                elif rosetype==2:
                    pc = axes.contour (wd, ws, bins=bins, cmap=cm, normed=norm, nsector=nsector, blowto=blowto, linewidth=edgewidth)
                else:
                    pc = axes.contourf(wd, ws, bins=bins, cmap=cm, normed=norm, nsector=nsector, blowto=blowto)
                    if edgecolor!='None': axes.contour (wd, ws, bins=bins, colors=edgecolor, normed=norm, nsector=nsector, blowto=blowto, linewidth=edgewidth)
                curhascolormap = True

                # Create colorbar
                assert cb is None, 'Currently only one object that needs a colorbar is supported per figure.'
                cax, kw = matplotlib.colorbar.make_axes(axes)
                binvalues = numpy.linspace(realbins[0],realbins[-1],len(bins))
                if rosetype!=2:
                    cb = matplotlib.colorbar.ColorbarBase(cax,cmap=cm,boundaries=realbins,values=binvalues)
                    if edgecolor!='None': cb.add_lines(realbins,edgecolor,linewidths=edgewidth)
                else:
                    cb = matplotlib.colorbar.ColorbarBase(cax,cmap=cm,boundaries=realbins,filled=False)
                    cb.add_lines(realbins,['None']+list(axes._colors(cm,len(bins))),linewidths=edgewidth)

                yinfo = {'label':'',
                         'unit':'',
                         'datatype':'float',
                         'tight':True,
                         'reversed':False,
                         'datarange':[0.,axes.get_rmax()]}
                axis2data.setdefault('r',{'forcedrange':[None,None]}).update(yinfo)
                axis2data['r'].setdefault('dimensions',[]).append('bincount')

            elif varslice.ndim==0:
                # Zero-dimensional coordinate space (i.e., only a single data value is available)
                # No plotting of coordinate-less data (yet)
                pass
            elif varslice.ndim==1:
                # One-dimensional coordinate space (x).
                
                # Retrieve cached coordinates
                X,Y,switchaxes = info['x'],info['y'],info['switchaxes']
                
                # Get data series style settings
                defcolor = linecolors[plotcount[1]%len(linecolors)]
                defaultseriesnode['LineProperties/Line/Color'].setValue(xmlstore.datatypes.Color(*defcolor))
                plotargs = getLineProperties(seriesnode['LineProperties'])
                
                # plot confidence interval (if any)
                hasconfidencelimits = (varslice.ubound is not None or varslice.lbound is not None)
                defaultseriesnode['HasConfidenceLimits'].setValue(hasconfidencelimits)
                if hasconfidencelimits:
                    ubound = varslice.ubound
                    if ubound is None: ubound = varslice.data
                    lbound = varslice.lbound
                    if lbound is None: lbound = varslice.data
                    
                    if seriesnode['LineProperties/Marker'].getValue(usedefault=True)==0:
                        defaultseriesnode['ConfidenceLimits/Style'].setValue(2)
                    else:
                        defaultseriesnode['ConfidenceLimits/Style'].setValue(1)
                    errorbartype = seriesnode['ConfidenceLimits/Style'].getValue(usedefault=True)
                    
                    if errorbartype==0:
                        pass
                    elif errorbartype==1:
                        # Plot error bars
                        xerr = None
                        yerr = numpy.vstack((varslice.data-lbound,ubound-varslice.data))
                        if switchaxes: xerr,yerr = yerr,xerr
                        axes.errorbar(X,Y,fmt=None,xerr=xerr,yerr=yerr,ecolor=plotargs['color'],zorder=zorder)
                    elif errorbartype==2:
                        # Plot shaded confidence area (filled polygon)
                        errX = numpy.hstack((varslice.coords[0],varslice.coords[0][::-1]))
                        errY = numpy.hstack((lbound,ubound[::-1]))
                        if switchaxes: errX,errY = errY,errX
                        areacolor = seriesnode['LineProperties/Line/Color'].getValue(usedefault=True)
                        areacolor.brighten(.5)
                        alpha = .7
                        axes.fill(errX,errY,facecolor=areacolor.getNormalized(),linewidth=0, alpha=alpha, zorder=zorder)
                    else:
                        assert False, 'Unknown error bar type %i.' % errorbartype
                    zorder += 1
                
                # Plot line and/or markers
                if plotargs['linestyle']!='' or plotargs['marker']!='':
                    hline = axes.plot(X,Y,zorder=zorder,label=label,**plotargs)
                    legenddata['handles'].append(hline)
                    legenddata['labels'].append(label)
                                
                plotcount[1] += 1
            elif varslice.ndim==2:
                plottype3d = seriesnode['PlotType3D'].getValue(usedefault=True)

                # Retrieve cached coordinates
                X,Y,C = info['x'],info['y'],None
                if 'C' in info: C = info['C']
                
                pc = None       # object using colormap
                norm = None     # color normalization object
                curhascolormap = C is not None
                cbborders = None # borders for discrete colors in colorbar
                
                if C is not None:
                    axisdata = axis2data.get('colorbar',{})
                    canhavelogscale = axisdata['datatype']!='datetime' and axisdata['datarange'][0] is not None
                    if canhavelogscale: canhavelogscale = axisdata['datarange'][0]>0 or axisdata['datarange'][1]>0
                    logscale = canhavelogscale and axis2data.get('colorbar',{}).get('logscale',False)
                    crange = list(axis2data.get('colorbar',{}).get('forcedrange',[None,None]))
                    if logscale:
                        norm = matplotlib.colors.LogNorm()
                        
                        # Mask values <= 0 manually, because color bar locators choke on them.
                        invalid = C<=0
                        if invalid.any(): C = numpy.ma.masked_where(invalid,C,copy=False)
                        axisdata['datarange'] = [C.min(),C.max()]
                        
                        # If we will make a vector plot: use color mask for u,v data as well.
                        if 'U' in info:
                            info['U'] = numpy.ma.masked_where(C._mask,info['U'],copy=False)
                            info['V'] = numpy.ma.masked_where(C._mask,info['V'],copy=False)
                    
                        # Ignore nonpositive color limits since a log scale is to be used.
                        if crange[0] is not None and crange[0]<=0.: crange[0] = None
                        if crange[1] is not None and crange[1]<=0.: crange[1] = None
                    
                    if crange[0] is None and crange[1] is None:
                        # Automatic color bounds: first check if we are not dealing with data with all the same value.
                        # if so, explicitly set the color range because MatPlotLib 0.90.0 chokes on identical min and max.
                        if hasattr(C,'_mask'):
                            flatC = C.compressed()
                        else:
                            flatC = C.ravel()
                        if len(flatC)>0:
                            # One or more unmasked values
                            if (flatC==flatC[0]).all(): crange = (flatC[0]-1.,flatC[0]+1.)
                        else:
                            # All values are masked
                            crange = (0.,1.)
                    
                if len(varslices)==1:
                    # Only one dependent variable: X,Y,C plot using contour, contourf and/or pcolormesh
                    
                    if plottype3d==1 or plottype3d==2:
                        # We have to make a contour plot (filled or empty)
                        
                        # Get contour properties
                        showedges = seriesnode['ShowEdges'].getValue(usedefault=True)
                        edgecolor = (seriesnode['EdgeColor'].getValue(usedefault=True).getNormalized(),)
                        if plottype3d==2 and seriesnode['UseColorMap'].getValue(usedefault=True): edgecolor = None
                        edgewidth = float(seriesnode['EdgeWidth'].getValue(usedefault=True))
                        borders,fill = (showedges or plottype3d==2),plottype3d==1
                        
                        cc = seriesnode['ContourCount'].getValue()
                        if cc is None: cc = 7

                        zmin,zmax = crange
                        if zmin is None: zmin = C.min()
                        if zmax is None: zmax = C.max()
                        lev = calculateContourLevels(cc,zmin,zmax,logscale)
                        defaultseriesnode['ContourCount'].setValue(len(lev)-2)
                        defaultseriesnode['ContourLevels'].setValue(list(lev[1:-1]))
                        if not fill: lev = lev[1:-1]
                        
                        forcedlev = seriesnode['ContourLevels'].getValue()
                        if forcedlev is not None:
                            levcount = len(forcedlev)
                            if fill:
                                lev = [lev[0]]+list(forcedlev)+[lev[-1]]
                            else:
                                lev = forcedlev
                            defaultseriesnode['ContourCount'].setValue(levcount)
                        
                        if fill:
                            pc = drawaxes.contourf(X,Y,C,lev,norm=norm,zorder=zorder,cmap=cm)
                            lev = lev[1:-1] # If we will also draw the contour lines themselves, the outside bounds must be omitted.
                        if borders:
                            if fill: zorder += 1
                            contourcm = cm
                            if edgecolor is not None: contourcm = None
                            cpc = drawaxes.contour(X,Y,C,lev,norm=norm,zorder=zorder,colors=edgecolor,linewidths=edgewidth,cmap=contourcm)
                            if edgecolor is not None: cbborders = cpc
                            if len(lev)>1 and not fill: pc = cpc
                            
                        if plottype3d==2 and edgecolor is not None: curhascolormap = False
                    else:
                        # We have to plot a colored quadrilinear mesh
                        edgecolor = 'None'
                        if seriesnode['ShowEdges'].getValue(usedefault=True):
                            # NB at present MatPlotLib just detects whether the edge color differs
                            # from None, and then draws black borders. I.e., the actual color is ignored.
                            edgecolor = seriesnode['EdgeColor'].getValue(usedefault=True).getNormalized()
                        pc = drawaxes.pcolormesh(X,Y,C,cmap=cm,norm=norm,edgecolors=edgecolor)
                      
                else:
                    # Two dependent variables: X,Y,U,V,C plot using quiver or barbs
                    U,V = info['U'],info['V']
                    
                    # Calculate velocities (needed for arrow auto-scaling)
                    vel = C
                    if vel is None: vel = numpy.sqrt(U*U+V*V)
                    keylength = numpy.abs(U).max()

                    remove_UV_mask = False  # used to be needed for old matplotlib, not anymore as of 0.99.1.1
                    if remove_UV_mask:
                        # Get combined mask of U,V and (optionally) C
                        mask = None
                        def addmask(mask,newmask):
                            if mask is None:
                                mask = numpy.empty(U.shape,dtype=numpy.bool)
                                mask.fill(False)
                            return numpy.logical_or(mask,newmask)
                        if hasattr(U,'_mask'): mask = addmask(mask,U._mask)
                        if hasattr(V,'_mask'): mask = addmask(mask,V._mask)
                        if C is not None and hasattr(C,'_mask'): mask = addmask(mask,C._mask)
                        
                        # Quiver with masked arrays has bugs in MatPlotlib 0.98.5
                        # Therefore we mask here only the color array, making sure that its mask combines
                        # the masks of U,V,C.
                        if mask is not None:
                            if C is not None: C = numpy.ma.masked_where(mask,C,copy=False)
                            U,V = U.filled(0.),V.filled(0.)
                            
                    scale = seriesnode['ArrowScale'].getValue(usedefault=True)
                    width = seriesnode['ArrowWidth'].getValue(usedefault=True)
                    pivot = seriesnode['ArrowPivot'].getValue(usedefault=True)
                    if C is None:
                        # Quiver without color values
                        arrowcolor = seriesnode['ArrowColor'].getValue(usedefault=True)
                        pc = drawaxes.quiver(X,Y,U,V,color=arrowcolor.getNormalized(),scale=scale,pivot=pivot,width=width)
                    else:
                        # Quiver with color values
                        pc = drawaxes.quiver(X,Y,U,V,C,cmap=cm,norm=norm,scale=scale,pivot=pivot,width=width)

                    # Auto-scale code taken from matplotlib.quiver
                    pc._init()
                    sn = max(10, math.sqrt(pc.N))
                    scale = 1.8 * vel.mean() * sn / pc.span # crude auto-scaling
                    sn = max(8, min(25, math.sqrt(pc.N)))
                    shaftwidth = 0.06 * pc.span / sn
                    defaultseriesnode['ArrowScale'].setValue(scale)  
                    defaultseriesnode['ArrowWidth'].setValue(shaftwidth)  
                    if pc.scale is None: pc.scale = scale
                    
                    keynode = seriesnode['ArrowKey']
                    defaultseriesnode['ArrowKey/Length'].setValue(keylength)
                    keyu = keynode['Length'].getValue(usedefault=True)
                    defaultseriesnode['ArrowKey/Label'].setValue(str(keyu))
                    if keynode.getValue(usedefault=True):
                        keyx,keyy = keynode['X'].getValue(usedefault=True),keynode['Y'].getValue(usedefault=True)
                        keylabel = keynode['Label'].getValue(usedefault=True)
                        keylabelpos = keynode['LabelPosition'].getValue(usedefault=True)
                        axes.quiverkey(pc,keyx,keyy,keyu,label=keylabel,labelpos=keylabelpos,coordinates='axes',fontproperties=fontpropsdict)
                        
                    if C is None: pc = None
                
                if pc is not None:
                    # Create colorbar
                    assert cb is None, 'Currently only one object that needs a colorbar is supported per figure.'
                    pc.set_clim(crange)
                    cb = figure.colorbar(pc,ax=axes)
                    if cbborders is not None: cb.add_lines(cbborders)

                plotcount[2] += 1
            
            else:
                self.errors.append('Data series %s ("%s") has %i independent coordinate dimensions, but only series with one or two independent dimensions can be plotted.' % (varpath,label,varslice.ndim))

            hascolormap |= curhascolormap

            # Increase z-order.
            zorder += 1

            # Hold all plot properties so we can plot additional data series.
            axes.hold(True)

        # Remove unused default series
        # (remaining from previous plots that had these other data series)
        for oldname in olddefaults:
            defaultdatanode.removeChild('Series',oldname)
            
        # Add map objects if needed
        if ismap:
            nodemap,defaultnodemap = self.properties['Map'],self.defaultproperties['Map']
            if nodemap['FillContinents'].getValue(usedefault=True):
                contcolor = nodemap['FillContinents/Color'].getValue(usedefault=True)
                lakecolor = nodemap['FillContinents/LakeColor'].getValue(usedefault=True)
                basemap.fillcontinents(contcolor.getNormalized(),lakecolor.getNormalized())
            if nodemap['DrawCoastlines'].getValue(usedefault=True):
                color = nodemap['DrawCoastlines/Color'].getValue(usedefault=True)
                linewidth = nodemap['DrawCoastlines/LineWidth'].getValue(usedefault=True)
                basemap.drawcoastlines(color=color.getNormalized(),linewidth=linewidth)
            if nodemap['DrawMapBoundary'].getValue(usedefault=True):
                color = nodemap['DrawMapBoundary/Color'].getValue(usedefault=True)
                linewidth = nodemap['DrawMapBoundary/LineWidth'].getValue(usedefault=True)
                basemap.drawmapboundary(color=color.getNormalized(),linewidth=linewidth)
            if nodemap['DrawRivers'].getValue(usedefault=True):
                color = nodemap['DrawRivers/Color'].getValue(usedefault=True)
                linewidth = nodemap['DrawRivers/LineWidth'].getValue(usedefault=True)
                basemap.drawrivers(color=color.getNormalized(),linewidth=linewidth)
            if nodemap['DrawCountries'].getValue(usedefault=True):
                color = nodemap['DrawCountries/Color'].getValue(usedefault=True)
                linewidth = nodemap['DrawCountries/LineWidth'].getValue(usedefault=True)
                basemap.drawcountries(color=color.getNormalized(),linewidth=linewidth)
            if nodemap['DrawStates'].getValue(usedefault=True):
                color = nodemap['DrawStates/Color'].getValue(usedefault=True)
                linewidth = nodemap['DrawStates/LineWidth'].getValue(usedefault=True)
                basemap.drawstates(color=color.getNormalized(),linewidth=linewidth)
            if nodemap['DrawParallels'].getValue(usedefault=True):
                llcrnrlat = nodemap['Range/LowerLeftLatitude' ].getValue(usedefault=True)
                urcrnrlat = nodemap['Range/UpperRightLatitude'].getValue(usedefault=True)
                loc = matplotlib.ticker.MaxNLocator(8)
                loc.create_dummy_axis()
                loc.set_bounds(llcrnrlat, urcrnrlat)
                lev = loc()
                defaultnodemap['DrawParallels/Parallels'].setValue(tuple(lev))
                defaultnodemap['DrawParallels/XOffset'].setValue((basemap.urcrnrx-basemap.llcrnrx)/100.)
                defaultnodemap['DrawParallels/YOffset'].setValue(basemap.aspect*(basemap.urcrnry-basemap.llcrnry)/100.)

                parallels = nodemap['DrawParallels/Parallels'].getValue(usedefault=True)
                if parallels:
                    basemap.drawparallels(parallels,labels=[1,0,0,1],
                                          color    =nodemap['DrawParallels/Color'    ].getValue(usedefault=True).getNormalized(),
                                          linewidth=nodemap['DrawParallels/LineWidth'].getValue(usedefault=True),
                                          xoffset  =nodemap['DrawParallels/XOffset'  ].getValue(usedefault=True),
                                          yoffset  =nodemap['DrawParallels/YOffset'  ].getValue(usedefault=True),
                                          **fontpropsdict)
            if nodemap['DrawMeridians'].getValue(usedefault=True):
                llcrnrlon = nodemap['Range/LowerLeftLongitude' ].getValue(usedefault=True)
                urcrnrlon = nodemap['Range/UpperRightLongitude'].getValue(usedefault=True)
                loc = matplotlib.ticker.MaxNLocator(8)
                loc.create_dummy_axis()
                loc.set_bounds(llcrnrlon, urcrnrlon)
                lev = loc()
                defaultnodemap['DrawMeridians/Meridians'].setValue(tuple(lev))
                defaultnodemap['DrawMeridians/XOffset'].setValue((basemap.urcrnrx-basemap.llcrnrx)/100.)
                defaultnodemap['DrawMeridians/YOffset'].setValue(basemap.aspect*(basemap.urcrnry-basemap.llcrnry)/100.)
                
                meridians = nodemap['DrawMeridians/Meridians'].getValue(usedefault=True)
                if meridians:
                    basemap.drawmeridians(meridians,labels=[1,0,0,1],
                                          color    =nodemap['DrawMeridians/Color'    ].getValue(usedefault=True).getNormalized(),
                                          linewidth=nodemap['DrawMeridians/LineWidth'].getValue(usedefault=True),
                                          xoffset  =nodemap['DrawMeridians/XOffset'  ].getValue(usedefault=True),
                                          yoffset  =nodemap['DrawMeridians/YOffset'  ].getValue(usedefault=True),
                                          **fontpropsdict)

        # Create and store title
        title = ''
        if titles:
            title = titles[0]
            for ln in titles[1:]:
                if ln!=title:
                    title = ', '.join(titles)
                    break
        self.defaultproperties['Title'].setValue(title)
        nodetitle = self.properties['Title']
        title = nodetitle.getValue(usedefault=True)
        assert title is not None, 'Title must be available, either explicitly set or as default.'
        if title!='':
            curfontprops = getFontProperties(nodetitle['Font'],textscaling=textscaling)
            axes.set_title(title,verticalalignment='baseline',**curfontprops)
        
        # Show legend
        legend = None
        self.defaultproperties['CanHaveLegend'].setValue(plotcount[1]>0)
        if plotcount[1]>0:
            self.defaultproperties['Legend'].setValue(plotcount[1]>1)
            legprop = self.properties['Legend']
            if legprop.getValue(usedefault=True):
                legend = axes.legend(legenddata['handles'],legenddata['labels'],loc=legprop['Location'].getValue(usedefault=True),prop=fontprops)
                #legend = figure.legend(legenddata['handles'],legenddata['labels'],1,prop=fontprops)
                legend.set_zorder(zorder)
                zorder += 1

        # Auto-show grid if we use 1 independent dimensions
        self.defaultproperties['Grid'].setValue(plotcount[2]==0)

        # Set whether the figure uses a colormap
        self.defaultproperties['HasColorMap'].setValue(hascolormap)

        # Transform axes to log-scale where specified.
        for axisname in ('x','y','z','colorbar','r'):
            if axisname not in axis2data: continue
            
            # Get default and forced axis properties
            axisnode = forcedaxes.getChildById('Axis',axisname,create=True)
            defaxisnode = defaultaxes.getChildById('Axis',axisname,create=True)
            
            # Determine whether the axis can be log-transformed.
            axisdata = axis2data.get(axisname,{})
            datarange = axisdata.get('datarange',[None,None])
            canhavelogscale = axisname!='r' and axisdata['datatype']!='datetime' and datarange[0] is not None
            if canhavelogscale: canhavelogscale = datarange[0]>0 or datarange[1]>0
            
            # Set log transformation defaults.
            defaxisnode['LogScale'].setValue(False)
            defaxisnode['CanHaveLogScale'].setValue(canhavelogscale)
            
            # Log transform axis if needed.
            if not (canhavelogscale and axisnode['LogScale'].getValue(usedefault=True)):
                continue
            if axisname=='x':
                axes.set_xscale('log')
            elif axisname=='y':
                axes.set_yscale('log')

        # Get effective ranges for each dimension (based on forced limits and natural data ranges)
        oldaxes    = [node.getSecondaryId() for node in forcedaxes.getLocationMultiple(['Axis'])]
        olddefaxes = [node.getSecondaryId() for node in defaultaxes.getLocationMultiple(['Axis'])]
        for axisname in ('x','y','z','colorbar','r'):
            if axisname not in axis2data: continue

            axisdata = axis2data[axisname]
            istimeaxis = axisdata['datatype']=='datetime'
            
            # Get the explicitly set and the default properties.
            axisnode = forcedaxes.getChildById('Axis',axisname,create=True)
            defaxisnode = defaultaxes.getChildById('Axis',axisname,create=True)
            if axisname in oldaxes: oldaxes.remove(axisname)
            if axisname in olddefaxes: olddefaxes.remove(axisname)

            # Range selected by MatPlotLib
            if axisdata.get('tight',True):
                naturalrange = axisdata['datarange'][:]
                if axisdata['datarange'][0] is None: naturalrange = [0.,1.]
            elif axisname=='x':
                naturalrange = axes.get_xlim()
            elif axisname in 'yr':
                naturalrange = axes.get_ylim()
            else:
                # Color range has been enforced before if needed (via pc.set_clim).
                # Thus we can no longer ask MatPlotLib for "natural" bounds - just use data limits.
                naturalrange = axisdata['datarange'][:]
                if axisdata['datarange'][0] is None: naturalrange = [0.,1.]
                
            # Get range forced by user
            if istimeaxis:
                mintime,maxtime = axisnode['MinimumTime'].getValue(),axisnode['MaximumTime'].getValue()
                if mintime is not None: mintime = common.date2num(mintime)
                if maxtime is not None: maxtime = common.date2num(maxtime)
                forcedrange = [mintime,maxtime]
            else:
                forcedrange = [axisnode['Minimum'].getValue(),axisnode['Maximum'].getValue()]
            bothforced = (forcedrange[0] is not None and forcedrange[1] is not None)
            reverse = (bothforced and forcedrange[0]>forcedrange[1]) or (axisdata['reversed'] and not bothforced)
            if reverse: forcedrange[0],forcedrange[1] = forcedrange[1],forcedrange[0]
                
            # Make sure forced ranges are valid if log transform is applied.
            if axisnode['LogScale'].getValue(usedefault=True):
                if forcedrange[0]<=0: forcedrange[0] = None
                if forcedrange[1]<=0: forcedrange[1] = None
            
            # Effective range used by data, after taking forced range into account.
            effdatarange = axisdata['datarange'][:]
            if forcedrange[0] is not None: effdatarange[0] = forcedrange[0]
            if forcedrange[1] is not None: effdatarange[1] = forcedrange[1]

            # Effective range, combining natural range with user overrides.
            effrange = list(forcedrange)
            if effrange[0] is None: effrange[0]=naturalrange[0]
            if effrange[1] is None: effrange[1]=naturalrange[1]
            
            # The natural range will now only be used to set default axes bounds.
            # Filter out infinite values (valid in MatPlotLib but not in xmlstore)
            naturalrange = list(naturalrange)
            if not numpy.isfinite(naturalrange[0]): naturalrange[0] = None
            if not numpy.isfinite(naturalrange[1]): naturalrange[1] = None
            
            # Reverse bpunds where needed
            if axisdata['reversed']: naturalrange[0],naturalrange[1] = naturalrange[1],naturalrange[0]
            if reverse: effrange[1],effrange[0] = effrange[0],effrange[1]

            # Build default label for this axis
            deflab = axisdata['label']
            if axisdata['unit']!='' and axisdata['unit'] is not None: deflab += ' ('+axisdata['unit']+')'
            
            # Set default axis properties.
            defaxisnode['Label'].setValue(deflab)
            defaxisnode['Dimensions'].setValue(';'.join(axisdata['dimensions']))    # Note! Used by pyncview!
            defaxisnode['Unit'].setValue(axisdata['unit'])
            defaxisnode['TicksMajor'].setValue(not axisdata.get('hideticks',False))
            defaxisnode['TicksMajor/ShowLabels'].setValue(True)
            defaxisnode['TicksMinor'].setValue(False)
            defaxisnode['TicksMinor/ShowLabels'].setValue(False)
            defaxisnode['IsTimeAxis'].setValue(istimeaxis)

            # Get the MatPlotLib axis object.
            mplaxis = None
            if axisname=='x':
                mplaxis = axes.get_xaxis()
            elif axisname in 'yr':
                mplaxis = axes.get_yaxis()
            elif cb is not None:
                mplaxis = cb.ax.get_yaxis()

            if istimeaxis:
                assert axisname!='colorbar', 'The color bar cannot be a time axis.'
                
                if mplaxis:
                    # Major ticks
                    dayspan = (effdatarange[1]-effdatarange[0])
                    location,interval,tickformat,tickspan = getTimeTickSettings(dayspan,axisnode['TicksMajor'],defaxisnode['TicksMajor'])
                    mplaxis.set_major_locator(getTimeLocator(location,interval))
                    mplaxis.set_major_formatter(CustomDateFormatter(tickformat))

                    # Minor ticks
                    location,interval,tickformat,tickspan = getTimeTickSettings(min(tickspan,dayspan),axisnode['TicksMinor'],defaxisnode['TicksMinor'])
                    mplaxis.set_minor_locator(getTimeLocator(location,interval))

                # Set the "natural" axis limits based on the data ranges.
                defaxisnode['MinimumTime'].setValue(common.num2date(naturalrange[0]))
                defaxisnode['MaximumTime'].setValue(common.num2date(naturalrange[1]))
            else:
                # Set the "natural" axis limits based on the data ranges.
                defaxisnode['Minimum'].setValue(naturalrange[0])
                defaxisnode['Maximum'].setValue(naturalrange[1])

            # Remove axis ticks if required.
            if mplaxis:
                if not axisnode['TicksMajor'].getValue(usedefault=True):
                    mplaxis.set_major_locator(matplotlib.ticker.FixedLocator([]))
                if not axisnode['TicksMinor'].getValue(usedefault=True):
                    mplaxis.set_minor_locator(matplotlib.ticker.FixedLocator([]))

            # Obtain label for axis.
            label = axisnode['Label'].getValue(usedefault=True)
            if label is None: label=''

            # Set axis labels and boundaries.
            if axisname=='x':
                if label!='': axes.set_xlabel(label,fontproperties=fontprops)
                axes.set_xlim(effrange[0],effrange[1])
            elif axisname=='y':
                if label!='': axes.set_ylabel(label,fontproperties=fontprops)
                axes.set_ylim(effrange[0],effrange[1])
            elif axisname=='r':
                if label!='': axes.set_ylabel(label,fontproperties=fontprops)
                loc = matplotlib.ticker.MaxNLocator(7)
                loc.create_dummy_axis()
                loc.set_bounds(0, effrange[1])
                radii = loc()[1:-1]
                axes.set_rmax(effrange[1])
                axes.set_rgrids(radii=radii, angle=axes.radii_angle)
                mplaxis.set_major_formatter(matplotlib.ticker.ScalarFormatter())
            elif axisname=='colorbar' and cb is not None:
                if label!='': cb.set_label(label,fontproperties=fontprops)

        for oldaxis in oldaxes:
            forcedaxes.removeChild('Axis',oldaxis)
        for oldaxis in olddefaxes:
            defaultaxes.removeChild('Axis',oldaxis)

        # Create grid
        gridnode = self.properties['Grid']
        if gridnode.getValue(usedefault=True):
            lineargs = getLineProperties(gridnode['LineProperties'])
            axes.grid(True,**lineargs)
        
        # Scale the text labels for x- and y-axis.
        for l in axes.get_xaxis().get_ticklabels(): l.set_fontproperties(fontprops)
        for l in axes.get_yaxis().get_ticklabels(): l.set_fontproperties(fontprops)
        axes.get_xaxis().get_offset_text().set_fontproperties(fontprops)
        axes.get_yaxis().get_offset_text().set_fontproperties(fontprops)
        
        # Transfer font properties to color bar.
        if cb is not None:
            cb.ax.yaxis.get_offset_text().set_fontproperties(fontprops)
            for l in cb.ax.yaxis.get_ticklabels(): l.set_fontproperties(fontprops)
        self.colorbar,self.ismap = cb,ismap
        self.onAspectChange(redraw=False)

        defaulttextsnode = self.defaultproperties['FigureTexts']
        for i,textnode in enumerate(self.properties['FigureTexts'].children):
            defaulttextnode = defaulttextsnode.getChildByNumber('Text',i,create=True)
            defaulttextnode['HorizontalAlignment'].setValue('center')
            defaulttextnode['VerticalAlignment'].setValue('center')
            defaulttextnode['X'].setValue(.5)
            defaulttextnode['Y'].setValue(.5)
            defaulttextnode['Rotation'].setValue(0.)
            setFontProperties(defaulttextnode['Font'],**fontpropsdict)
            x,y = textnode['X'].getValue(usedefault=True),textnode['Y'].getValue(usedefault=True)
            curfontprops = getFontProperties(textnode['Font'],textscaling=textscaling)
            figure.text(x,y,textnode.getValue(usedefault=True),fontdict=curfontprops,
                        ha=textnode['HorizontalAlignment'].getValue(usedefault=True),
                        va=textnode['VerticalAlignment'].getValue(usedefault=True),
                        rotation=textnode['Rotation'].getValue(usedefault=True))

        # Determine whether a valid plot is now shown.
        validplot = (plotcount[1]+plotcount[2])>0 or projection=='windrose'
        
        return validplot

    def onAspectChange(self,redraw=True):
        if self.colorbar is not None and self.ismap:
            # Adjust colorbar top and height based on basemap-modified axes
            axes = self.figure.gca()
            axes.apply_aspect()
            p = axes.get_position(original=False)
            cbp = self.colorbar.ax.get_position(original=False)
            cbp = cbp.from_bounds(cbp.x0,p.y0,cbp.width,p.height)
            self.colorbar.ax.set_position(cbp)
            if redraw: self.canvas.draw()
        
def setLineProperties(propertynode,mplsection='lines',**kwargs):
    """Sets the values under a xmlstore.TypedStore node describing line
    properties all at once.
    
    Internal use only. Used to quickly set default line properties.
    """
    deflinewidth = matplotlib.rcParams[mplsection+'.linewidth']
    deflinecolor = matplotlib.rcParams[mplsection+'.color']
    deflinecolor = matplotlib.colors.colorConverter.to_rgb(deflinecolor)
    deflinecolor = xmlstore.datatypes.Color.fromNormalized(*deflinecolor)
    deflinestyle = matplotlib.rcParams[mplsection+'.linestyle']

    defmarkersize = matplotlib.rcParams.get(mplsection+'.markersize',6.)
    defedgewidth = matplotlib.rcParams.get(mplsection+'.markeredgewidth',0.5)
    defedgecolor = matplotlib.rcParams.get(mplsection+'.markeredgecolor','black')
    defedgecolor = matplotlib.colors.colorConverter.to_rgb(defedgecolor)
    defedgecolor = xmlstore.datatypes.Color.fromNormalized(*defedgecolor)

    propertynode['CanHaveMarker'].setValue(kwargs.get('CanHaveMarker',True))
    
    line = propertynode['Line']
    line.setValue(kwargs.get('LineStyle',deflinestyle))
    line['Width'].setValue(kwargs.get('LineWidth',deflinewidth))
    line['Color'].setValue(kwargs.get('Color',deflinecolor))
    
    marker = propertynode['Marker']
    marker.setValue(kwargs.get('MarkerType',''))
    marker['Size'].setValue(kwargs.get('MarkerSize',defmarkersize))
    marker['FaceColor'].setValue(kwargs.get('MarkerFaceColor',deflinecolor))
    marker['EdgeColor'].setValue(kwargs.get('MarkerEdgeColor',defedgecolor))
    marker['EdgeWidth'].setValue(kwargs.get('MarkerEdgeWidth',defedgewidth))
    
def getLineProperties(propertynode):
    """Returns a dictionary with line properties based on the specified
    xmlstore.TypedStore node.
    
    Internal use only.
    """
    marker = propertynode['Marker']
    markertype = marker.getValue(usedefault=True)
    
    line = propertynode['Line']
    linestyle = line.getValue(usedefault=True)
    
    linewidth = line['Width'].getValue(usedefault=True)
    color = line['Color'].getValue(usedefault=True)
    markersize = marker['Size'].getValue(usedefault=True)
    markerfacecolor = marker['FaceColor'].getValue(usedefault=True)
    markeredgecolor = marker['EdgeColor'].getValue(usedefault=True)
    markeredgewidth = marker['EdgeWidth'].getValue(usedefault=True)
    
    return {'linestyle':linestyle,
            'marker':markertype,
            'linewidth':linewidth,
            'color':color.getNormalized(),
            'markersize':markersize,
            'markerfacecolor':markerfacecolor.getNormalized(),
            'markeredgecolor':markeredgecolor.getNormalized(),
            'markeredgewidth':markeredgewidth}

def setFontProperties(node,family=None,size=8,style='normal',weight=400):
    node['Family'].setValue(family)
    node['Size'].setValue(size)
    node['Style'].setValue(style)
    node['Weight'].setValue(weight)
    
def getFontProperties(node,textscaling=1.):
    return {'family':node['Family'].getValue(usedefault=True),
            'size':node['Size'].getValue(usedefault=True)*textscaling,
            'style':node['Style'].getValue(usedefault=True),
            'weight':node['Weight'].getValue(usedefault=True)}
    
def getTimeLocator(location,interval):
    """Creates a time locator based on the unit ("location") and interval
    chosen.
    
    Internal use only.
    """
    if location==0:
        return matplotlib.dates.YearLocator(base=interval)
    elif location==1:
        return matplotlib.dates.MonthLocator(interval=interval)
    elif location==2:
        return matplotlib.dates.DayLocator(interval=interval)
    elif location==3:
        return matplotlib.dates.HourLocator(interval=interval)
    elif location==4:
        return matplotlib.dates.MinuteLocator(interval=interval)
    else:
        assert False, 'unknown tick location %i' % location
    
def getTimeTickSettings(dayspan,settings,defsettings,preferredcount=8):
    """Reads the time tock settings from the specified TypedStore.xmlstore node.
    
    Internal use only.
    """
    unitlengths = {0:365,1:30.5,2:1.,3:1/24.,4:1/1440.}
    if dayspan/365>=2:
        location,tickformat = 0,'yyyy'
    elif dayspan>=61:
        location,tickformat = 1,'m'
    elif dayspan>=2:
        location,tickformat = 2,'dd/mm'
    elif 24*dayspan>=2:
        location,tickformat = 3,'HH:MM'
    else:
        location,tickformat = 4,'HH:MM'

    defsettings['LocationTime'].setValue(location)
    defsettings['FormatTime'].setValue(tickformat)
    location   = settings['LocationTime'].getValue(usedefault=True)
    tickformat = settings['FormatTime'].getValue(usedefault=True)

    # Calculate optimal interval between ticks, aiming for max. 8 ticks total.
    tickcount = dayspan/unitlengths[location]
    interval = math.ceil(float(tickcount)/preferredcount)
    if interval<1: interval = 1
    
    # Save default tick interval, then get effective tick interval.
    defsettings['IntervalTime'].setValue(interval)
    interval = settings['IntervalTime'].getValue(usedefault=True)

    # Make sure we do not plot more than 100 ticks: non-informative and very slow!
    if tickcount/interval>100: interval=math.ceil(tickcount/100.)
    
    return location,interval,str(common.convertMatlabDateFormat(tickformat)),interval*unitlengths[location]
