import math,os.path,xml.dom.minidom

import matplotlib
import matplotlib.colors
import matplotlib.dates
import numpy

import common,xmlstore.xmlstore,xmlstore.util

colormaps = {0:'jet',
             1:'hsv',
             2:'hot',
             3:'cool',
             4:'spring',
             5:'summer',
             6:'autumn',
             7:'winter',
             8:'gray',
             9:'bone',
             10:'copper',
             11:'pink'}
      
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
            if bounds[0].start!=None and bounds[0].start>ifirst: ifirst = int(math.floor(bounds[0].start))
            if bounds[0].stop !=None and bounds[0].stop <ilast : ilast  = int(math.ceil (bounds[0].stop))
            slice.coords[0] = numpy.linspace(float(ifirst),float(ilast),ilast-ifirst+1)
            slice.coords_stag[0] = common.getCenters(slice.coords[0],addends=True)

            first = True
            for ivar,var in enumerate(self.vars[ifirst:ilast+1]):
                curslice = var.getSlice(bounds[1:])
                assert curslice!=None, 'Unable to obtain valid slice from variable %s.' % var
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
            dayname = str(matplotlib.dates.DateFormatter.strftime(self,dt,'%A'))
            fmt = fmt.replace('%e',dayname[0])
        if ('%n' in fmt):
            month = str(matplotlib.dates.DateFormatter.strftime(self,dt,'%b'))
            fmt = fmt.replace('%n',month[0])
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
        assert sourcevar!=None, 'The source variable for a transform cannot be None.'
        self.sourcevar = sourcevar
        if name==None:
            name = nameprefix + self.sourcevar.getName()
        if longname==None: 
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
    correct XML schema. In the future this class can host convertors that
    convert between different versions of the XML schema for figures.
    """

    def __init__(self,valueroot=None,adddefault = True):
        schemadom = os.path.join(common.getDataRoot(),'schemas/figure/0001.xml')
        xmlstore.xmlstore.TypedStore.__init__(self,schemadom,valueroot,adddefault=adddefault)

    schemadict = None
    @staticmethod
    def getDefaultSchemas():
        if FigureProperties.schemadict==None:
            FigureProperties.schemadict = xmlstore.xmlstore.ShortcutDictionary.fromDirectory(os.path.join(common.getDataRoot(),'schemas/figure'))
        return FigureProperties.schemadict
        
class Figure(xmlstore.util.referencedobject):
    """Class encapsulating a MatPlotLib figure. The data for the figure is
    provided as one or more VariableStore objects, with data series being
    identified by the name of the VariableStore and the name of the variable
    to be plotted. All configuration of the plots is done through a
    xmlstore.TypedStore object.
    """

    def __init__(self,figure=None,defaultfont=None):
        xmlstore.util.referencedobject.__init__(self)

        # If no MatPlotLib figure is specified, create a new one, assuming
        # we want to export to file.        
        if figure==None:
            figure = matplotlib.figure.Figure(figsize=(10/2.54,8/2.54))
            canvas = matplotlib.backends.backend_agg.FigureCanvasAgg(figure)
        
        # If no default font is specified, use the MatPlotLib default.
        if defaultfont==None:
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
        self.defaultproperties['FontName'       ].setValue(defaultfont)
        self.defaultproperties['FontScaling'    ].setValue(100)
        self.defaultproperties['Grid'           ].setValue(False)
        self.defaultproperties['Legend/Location'].setValue(0)
        self.defaultproperties['HasColorMap'    ].setValue(False)
        self.defaultproperties['ColorMap'       ].setValue(0)
        setLineProperties(self.defaultproperties['Grid/LineProperties'],CanHaveMarker=False,mplsection='grid')

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
        
        self.callbacks = {'completeStateChange':[]}
        
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
        self.source.deleteAllChildren()
        self.defaultsource = None

    def addDataSource(self,name,obj):
        """Adds a VariableStore data source to the figure, using the specified
        name.
        """
        self.source.addChild(name,obj)
        if self.defaultsource==None: self.defaultsource = name

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
        self.properties.setStore(props)
        self.update()

    def getPropertiesCopy(self):
        """Get a copy of the current figure properties as XML node.
        """
        return self.properties.toXmlDom()

    def clearVariables(self):
        """Clears all data series. Thsi does not automatically clear the
        list of registered data sources (VariableStore objects).
        """
        self.properties['Data'].removeChildren('Series')

    def addVariable(self,varname,source=None,replace=True):
        """Add a variable to the figure. If no data source name if specified,
        the first registered source will be used. The specified variable must
        match the name of a variable in the data source to be used.
        """
        assert source==None or isinstance(source,basestring), 'If the "source" option is specified, it must be a string.'
        datanode = self.properties['Data']
        varname = self.normalizeExpression(varname,source)
        if replace:
            series = datanode.getChildById('Series',varname,create=True)
            self.defaultproperties['Data'].getChildById('Series',varname,create=True)
        else:
            series = datanode.addChild('Series',id=varname)
            self.defaultproperties['Data'].addChild('Series',id=varname)
        self.update()
        return series
        
    def normalizeExpression(self,expression,source=None):
        if source==None: source = self.defaultsource
        return self.source.getExpression(expression,defaultchild=source).buildExpression()

    def hasChanged(self):
        """Returns True if the figure properties have changed since the store
        was created or resetChanged was called.
        """
        return self.haschanged

    def resetChanged(self):
        """Resets the "changed" status of the figure properties.
        """
        self.haschanged = False
        
    def exportToFile(self,path,dpi=150):
        """Export the contents of the figure to file.
        """
        w = self['Width'].getValue(usedefault=True)
        h = self['Height'].getValue(usedefault=True)
        self.figure.set_size_inches(w/2.54,h/2.54)
        self.canvas.print_figure(str(path),dpi=dpi)
        
    def copyFrom(self,sourcefigure):
        """Copies all plot properties and data sources from the supplied source figure.
        """
        properties = sourcefigure.getPropertiesCopy()
        for name,child in sourcefigure.source.children.iteritems():
            self.source.addChild(name,child)
        self.defaultsource = sourcefigure.defaultsource
        self.setProperties(properties)
                
    def update(self):
        """Update the figure.
        
        Everything happens here. The current set of customized properties is
        interpreted, data slice are obtained from the data sources, default
        figure properties are set based on properties of the obtained data,
        and the figure is built and shown.
        """
                
        # We are called whenever figure properties change. If we do not want to update now,
        # just register that an update is needed and exit.
        if not self.updating:
            self.dirty = True
            return

        # Below: code for debugging superfluous plot updates (sends stack trace to stdout)
        #import traceback
        #print '--- stack for call to update ---'   
        #trace = traceback.format_list(traceback.extract_stack(limit=10))    
        #for l in trace: print l,
            
        # Clear the current MatPlotLib figure.
        self.figure.clear()

        # Create one subplot only.
        axes = self.figure.add_subplot(111)
        
        # Obtain text scaling property (from % to fraction)
        textscaling = self.properties['FontScaling'].getValue(usedefault=True)/100.
        
        # First scale the default font size; this takes care of all relative font sizes (e.g. "small")
        matplotlib.font_manager.fontManager.set_default_size(textscaling*matplotlib.rcParams['font.size'])
        
        # Now get some relevant font sizes.
        # Scale font sizes with text scaling parameter if they are absolute sizes.
        # (if they are strings, they are relative sizes already)
        fontfamily = self.properties['FontName'].getValue(usedefault=True)
        fontsizes = {
            'axes.titlesize' :10,#matplotlib.rcParams['axes.titlesize'],
            'axes.labelsize' :8, #matplotlib.rcParams['axes.labelsize'],
            'xtick.labelsize':8, #matplotlib.rcParams['xtick.labelsize'],
            'ytick.labelsize':8, #matplotlib.rcParams['ytick.labelsize']
            'legend':8
        }
        for k,v in fontsizes.iteritems():
            if not isinstance(v,basestring): fontsizes[k]=v*textscaling
            
        # Line colors to cycle through
        linecolors = ((0,0,255),(0,255,0),(255,0,0),(0,255,255),(255,0,255),(255,255,0),(0,0,0))

        # Get forced axes boundaries (will be None if not set; then we autoscale)
        # These boundaries are retrieved before data are obtained, in order to be able
        # to skip reading out unnecessary data (which currently does not work!).
        dim2data = {}
        defaultaxes = self.defaultproperties['Axes']
        forcedaxes = self.properties['Axes']
        for forcedaxis in forcedaxes.getLocationMultiple(['Axis']):
            istimeaxis = forcedaxis['IsTimeAxis'].getValue(usedefault=True)
            logscale = False
            if istimeaxis:
                axmin = forcedaxis['MinimumTime'].getValue()
                axmax = forcedaxis['MaximumTime'].getValue()
                if axmin!=None: axmin = common.date2num(axmin)
                if axmax!=None: axmax = common.date2num(axmax)
            else:
                axmin = forcedaxis['Minimum'].getValue()
                axmax = forcedaxis['Maximum'].getValue()
                logscale = forcedaxis['LogScale'].getValue()
            dim2data[forcedaxis.getSecondaryId()] = {'forcedrange':[axmin,axmax],'logscale':logscale}

        # Shortcuts to the nodes specifying the series to plot.
        forceddatanode = self.properties['Data']
        forcedseries = forceddatanode.getLocationMultiple(['Series'])

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
        cm = getattr(matplotlib.cm,colormaps[self.properties['ColorMap'].getValue(usedefault=True)])
        cm.set_bad('w')
        
        # Start with z order index 0 (incrementing it with every item added)
        zorder = 0
        
        # Dictionary holding number of data series per number of independent dimensions.
        plotcount = {1:0,2:0}
        
        # Dictionary with legend information (handles to drawn data series and the series
        # label) to be filled while adding data series.
        legenddata = {'handles':[],'labels':[]}

        for (iseries,seriesnode) in enumerate(forcedseries):
            # Get the path of the data source (data source identifier + variable id)
            varpath = seriesnode.getSecondaryId()
            if varpath=='':
                print 'Skipping data series %i because the secondary node id (i.e., variable source and name) is not set.'
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
            defaultseriesnode['HasConfidenceLimits'].setValue(False)
            setLineProperties(defaultseriesnode['LineProperties'])
            defaultseriesnode['ShowEdges'].setValue(False)
            defaultseriesnode['UseColorMap'].setValue(True)
            defaultseriesnode['EdgeColor'].setValue(xmlstore.xmlstore.StoreColor(0,0,0))
            defaultseriesnode['EdgeWidth'].setValue(1.)
            
            # Old defaults will be removed after all series are plotted.
            # Register that the current variable is active, ensuring its default will remain.
            if varpath in olddefaults: olddefaults.remove(varpath)

            # Store the [default or custom] variable long name; it will be used for building the plot title.
            label = seriesnode['Label'].getValue(usedefault=True)
            titles.append(label)

            # Build list of dimension boundaries for current variable.
            # For dimensions that have equal lower and upper bound, take a slice.
            dimbounds = []
            originaldims = var.getDimensions()
            for dimname in originaldims:
                if dimname in dim2data:
                    # We have boundaries set on the current dimension.
                    forcedrange = dim2data[dimname].get('forcedrange',(None,None))
                    if forcedrange[0]!=None: forcedrange[0] = forcedrange[0]
                    if forcedrange[1]!=None: forcedrange[1] = forcedrange[1]
                    if forcedrange[0]==forcedrange[1] and forcedrange[0]!=None:
                        # Equal upper and lower boundary: take a slice.
                        var = VariableSlice(var,dimname,forcedrange[0])
                    else:
                        dimbounds.append(slice(forcedrange[0],forcedrange[1]))
                else:
                    # No boundaries set.
                    dimbounds.append(slice(None))
                    
            # Get the data
            varslices = var.getSlice(tuple(dimbounds))
            assert len(varslices)>0, 'Unable to retrieve any variable slices.'
            
            # Skip this variable if (parts of) its data are unavailable.
            if not all([varslice.isValid() for varslice in varslices]): continue

            # Now we are at the point where getting the data worked.
            # Register all used dimensions (even the "sliced out" ones)
            # as used, and get information on them.
            for dimname in originaldims:
                dimdata = dim2data.setdefault(dimname,{'forcedrange':[None,None]})
                dimdata['used'] = True
                diminfo = var.getDimensionInfo(dimname)
                dimdata.update(diminfo)

            # Add the variable itself to the dimension list.
            dimdata = dim2data.setdefault(varpath,{'forcedrange':(None,None)})
            dimdata.update({'label':var.getLongName(),'unit':var.getUnit(),'datatype':'float','tight':False,'logscale':False,'reversed':False})
            
            for i in range(len(varslices)):
                # Eliminate singleton dimensions (singleton dimension: dimension with length one)
                # Store singleton dimensions as fixed extra coordinates.
                varslices[i] = varslices[i].squeeze()
                
                # Mask infinite/nan values, if any - only do this if the array is not masked
                # already, because it seems isfinite is not supported on masked arrays.
                if not hasattr(varslices[i].data,'_mask'):
                    invalid = numpy.logical_not(numpy.isfinite(varslices[i].data))
                    if invalid.any():
                        print 'WARNING: masking %i invalid values (inf or nan) out of %i.' % (invalid.sum(),invalid.size)
                        varslices[i].data = numpy.ma.array(varslices[i].data,mask=invalid)

            # Get the number of dimensions from the data slice, and add it to the plot properties.
            defaultseriesnode['DimensionCount'].setValue(varslices[0].ndim)

            # Get the plot type for 3D plots.
            plottype3d = seriesnode['PlotType3D'].getValue(usedefault=True)

            # We use a staggered grid (coordinates at interfaces,
            # values at centers) for certain 3D plot types.
            staggered = (len(varslices)==1 and varslices[0].ndim==2 and plottype3d==0)
            
            # Create shortcut to applicable coordinate set.
            if staggered:
                coords = varslices[0].coords_stag
            else:
                coords = varslices[0].coords

            # Get the minimum and maximum values; store these as default.
            dim2data[varpath]['datarange'] = [varslices[0].data.min(),varslices[0].data.max()]

            # Enumerate over the dimension of the variable.
            for idim,dimname in enumerate(varslices[0].dimensions):
                # Get minimum and maximum coordinates.
                if coords[idim].ndim==1:
                    # Coordinates provided as vector (1D) valid over whole domain.
                    datamin = coords[idim][0]
                    datamax = coords[idim][-1]
                else:
                    # Coordinates are provided as multidimensional array, with a value for every
                    # coordinate (data point) in the domain. We assume that for a given point
                    # in the space of the other coordinates, the current cordinate increases
                    # monotonously (i.e., position 0 holds the lowest value and position -1 the
                    # highest)
                    datamin = coords[idim].take((0, ),idim).min()
                    datamax = coords[idim].take((-1,),idim).max()

                # Update effective dimension bounds                    
                effrange = dim2data[dimname].setdefault('datarange',[None,None])
                if effrange[0]==None or datamin<effrange[0]: effrange[0] = datamin
                if effrange[1]==None or datamax>effrange[1]: effrange[1] = datamax
                
            varslice = varslices[0]
            
            # Plot the data series
            if varslice.ndim==0:
                # Zero-dimensional coordinate space (i.e., only a single data value is available)
                # No plotting of coordinate-less data (yet)
                pass
            if varslice.ndim==1:
                # One-dimensional coordinate space (x). Use x-axis for coordinates, unless the
                # dimension information states it is preferably uses the y-axis.
                X,Y = varslice.coords[0], varslice.data
                xname,yname = varslice.dimensions[0], varpath
                switchaxes = (dim2data[varslice.dimensions[0]]['preferredaxis']=='y')
                if switchaxes:
                    X,Y = Y,X
                    xname,yname = yname,xname
                
                # Get data series style settings
                defaultseriesnode['LineProperties/Color'].setValue(xmlstore.xmlstore.StoreColor(*linecolors[plotcount[1]%len(linecolors)]))
                plotargs = getLineProperties(seriesnode['LineProperties'])
                
                # plot confidence interval (if any)
                hasconfidencelimits = (varslice.ubound!=None or varslice.lbound!=None)
                defaultseriesnode['HasConfidenceLimits'].setValue(hasconfidencelimits)
                if hasconfidencelimits:
                    ubound = varslice.ubound
                    if ubound==None: ubound = varslice.data
                    lbound = varslice.lbound
                    if lbound==None: lbound = varslice.data
                    
                    if seriesnode['LineProperties/MarkerType'].getValue(usedefault=True)==0:
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
                        areacolor = seriesnode['LineProperties/Color'].getValue(usedefault=True)
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
                
                dim2data[xname]['axis'] = 'x'
                dim2data[yname]['axis'] = 'y'
                
                plotcount[1] += 1
            elif varslice.ndim==2:
                # Two-dimensional coordinate space (x,y). Use x-axis for first coordinate dimension,
                # and y-axis for second coordinate dimension.
                xdim,ydim = 0,1
                if var.hasReversedDimensions(): xdim,ydim = 1,0
                prefaxis = (dim2data[varslice.dimensions[0]]['preferredaxis'],dim2data[varslice.dimensions[1]]['preferredaxis'])
                if (prefaxis[xdim]=='y' and prefaxis[ydim]!='y') or (prefaxis[ydim]=='x' and prefaxis[xdim]!='x'):
                    # One independent dimension prefers to switch axis and the
                    # other does not disagree.
                    xdim,ydim = ydim,xdim

                dim2data[varslice.dimensions[xdim]]['axis'] = 'x'
                dim2data[varslice.dimensions[ydim]]['axis'] = 'y'
                
                X = coords[xdim]
                Y = coords[ydim]
                
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
                    
                # Adjust X dimension.
                if X.ndim==1:
                    X = X.reshape((1,-1)).repeat(ylength, 0)
                elif xdim<ydim:
                    X = X.transpose()
                    
                # Adjust Y dimension.
                if Y.ndim==1:
                    Y = Y.reshape((-1,1)).repeat(xlength, 1)
                elif xdim<ydim:
                    Y = Y.transpose()
                    
                pc = None       # object using colormap
                norm = None     # color normalization object
                hascolormap = True
                logscale = dim2data.get('colorbar',{}).get('logscale',False)
                if logscale: norm = matplotlib.colors.LogNorm()

                if len(varslices)==1:
                    # Only one dependent variable
                    Z = varslices[0].data
                    if xdim<ydim: Z = Z.transpose()
                    C = Z

                    # Allocate colorbar axis to Z variable.
                    if plottype3d!=2: dim2data[varpath]['axis'] = 'colorbar'

                    if plottype3d==1 or plottype3d==2:
                        # We have to make a contour plot (filled or empty)
                        
                        loc = None
                        if logscale: loc = matplotlib.ticker.LogLocator()

                        cc = seriesnode['ContourCount'].getValue()
                        showedges = seriesnode['ShowEdges'].getValue(usedefault=True)
                        edgecolor = (seriesnode['EdgeColor'].getValue(usedefault=True).getNormalized(),)
                        if plottype3d==2 and seriesnode['UseColorMap'].getValue(usedefault=True): edgecolor = None
                        edgewidth = seriesnode['EdgeWidth'].getValue(usedefault=True)
                        borders,fill = (showedges or plottype3d==2),plottype3d==1
                        cset,csetf = None,None
                        if cc!=None:
                            # Contour count was specified
                            if fill:
                                csetf = axes.contourf(X,Y,Z,cc,norm=norm,locator=loc,zorder=zorder,cmap=cm)
                            if borders:
                                if fill: zorder += 1
                                contourcm = cm
                                if edgecolor!=None: contourcm = None
                                cset = axes.contour(X,Y,Z,cc,norm=norm,locator=loc,zorder=zorder,colors=edgecolor,linewidths=edgewidth,cmap=contourcm)
                        else:
                            # Automatically determine contour count
                            if fill:
                                csetf = axes.contourf(X,Y,Z,norm=norm,locator=loc,zorder=zorder,cmap=cm)
                            if borders:
                                if fill: zorder += 1
                                contourcm = cm
                                if edgecolor!=None: contourcm = None
                                cset = axes.contour(X,Y,Z,norm=norm,locator=loc,zorder=zorder,colors=edgecolor,linewidths=edgewidth,cmap=contourcm)

                            # Retrieve the number of contours chosen by MatPlotLib
                            constset = csetf
                            if constset==None: constset = cset
                            defaultseriesnode['ContourCount'].setValue(len(constset.levels)-2)
                        pc = csetf
                        if plottype3d==2 and edgecolor!=None: hascolormap = False
                    else:
                        # We have to plot a colored quadrilinear mesh
                        
                        #edgecolors = 'None'
                        #if seriesnode['ShowEdges'].getValue(usedefault=True): edgecolors = 'k'
                        shading = 'flat'
                        if seriesnode['ShowEdges'].getValue(usedefault=True): shading = 'faceted'
                        pc = axes.pcolormesh(X,Y,Z,cmap=cm,norm=norm,shading=shading)
                      
                else:
                    # More than one dependent variable
                    assert len(varslices)==2,'Only plots with one or two dependent variables are currently supported.'
                    
                    U,V = varslices[0].data,varslices[1].data
                    if xdim<ydim: U,V = U.transpose(),V.transpose()

                    C = numpy.sqrt(U**2+V**2)
                    pc = axes.quiver(X,Y,U,V,C,cmap=cm,norm=norm)

                    # Allocate colorbar axis to arrow length.
                    dimdata = dim2data.setdefault('arrowlength',{'forcedrange':(None,None)})
                    dimdata.update({'label':var.getLongName(),'unit':var.getUnit(),'datatype':'float','tight':False,'logscale':False,'axis':'colorbar'})
                    dimdata['datarange'] = [C.min(),C.max()]
                
                if pc!=None:
                    # Create colorbar
                    assert cb==None, 'Currently only one object that needs a colorbar is supported per figure.'
                    if isinstance(C,numpy.ma.MaskedArray):
                        flatC = C.compressed()
                    else:
                        flatC = C.ravel()
                    if len(flatC)>0 and (flatC==flatC[0]).all():
                        # All color values are equal. Explicitly set color range,
                        # because MatPlotLib 0.90.0 chokes on identical min and max.
                        pc.set_clim((Z[0,0]-1,Z[0,0]+1))
                    else:
                        pc.set_clim(dim2data.get('colorbar',{}).get('forcedrange',(None,None)))
                    cb = self.figure.colorbar(pc)

                plotcount[2] += 1
            
            else:
                print 'We can only plot variables with 1 or 2 dimensions, but "%s" has %i dimensions. Skipping it.' % (varpath,varslice.ndim)

            # Increase z-order.
            zorder += 1

            # Hold all plot properties so we can plot additional data series.
            axes.hold(True)

        # Remove unused default series
        # (remaining from previous plots that had these other data series)
        for oldname in olddefaults:
            defaultdatanode.removeChild('Series',oldname)

        # Create and store title
        title = ''
        if titles:
            title = titles[0]
            for ln in titles[1:]:
                if ln!=title:
                    title = ', '.join(titles)
                    break
        self.defaultproperties['Title'].setValue(title)
        title = self.properties['Title'].getValue(usedefault=True)
        assert title!=None, 'Title must be available, either explicitly set or as default.'
        if title!='': axes.set_title(title,size=fontsizes['axes.titlesize'],fontname=fontfamily)
        
        # Show legend
        legend = None
        self.defaultproperties['CanHaveLegend'].setValue(plotcount[1]>0)
        if plotcount[1]>0:
            self.defaultproperties['Legend'].setValue(plotcount[1]>1)
            legprop = self.properties['Legend']
            if legprop.getValue(usedefault=True):
                legend = axes.legend(legenddata['handles'],legenddata['labels'],loc=legprop['Location'].getValue(usedefault=True),prop=matplotlib.font_manager.FontProperties(size=fontsizes['legend'],family=fontfamily))
                #legend = self.figure.legend(legenddata['handles'],legenddata['labels'],1,prop=matplotlib.font_manager.FontProperties(size=fontsizes['legend'],family=fontfamily))
                legend.set_zorder(zorder)
                zorder += 1

        # Set whether the figure uses a colormap
        self.defaultproperties['HasColorMap'].setValue(hascolormap)

        # Build table linking axis to data dimension.
        axis2dim = {}
        for dim,dat in dim2data.iteritems():
            if 'axis' in dat: axis2dim.setdefault(dat['axis'],[]).append(dim)

        # Transform axes to log-scale where specified.
        for axisname in ('x','y','z','colorbar'):
            if axisname not in axis2dim: continue
            
            # Get default and forced axis properties
            axisnode = forcedaxes.getChildById('Axis',axisname,create=True)
            defaxisnode = defaultaxes.getChildById('Axis',axisname,create=True)
            
            # Determine whether the axis can be log-transformed.
            dimdata = dim2data[axis2dim[axisname][0]]
            datarange = dimdata['datarange']
            canhavelogscale = dimdata['datatype']!='datetime' and (datarange[0]>0 or datarange[1]>0)
            
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
        for axisname in ('x','y','z','colorbar'):
            if axisname not in axis2dim: continue
            
            dim = axis2dim[axisname][0]
            dat = dim2data[dim]
            istimeaxis = dat['datatype']=='datetime'
            
            # Get the explicitly set and the default properties.
            axisnode = forcedaxes.getChildById('Axis',axisname,create=True)
            defaxisnode = defaultaxes.getChildById('Axis',axisname,create=True)
            if axisname in oldaxes: oldaxes.remove(axisname)
            if axisname in olddefaxes: olddefaxes.remove(axisname)

            # Range selected by MatPlotLib
            if dat.get('tight',True):
                naturalrange = dat['datarange'][:]
            elif axisname=='x':
                naturalrange = axes.get_xlim()
            elif axisname=='y':
                naturalrange = axes.get_ylim()
            else:
                # Color range has been enforced before if needed (via pc.set_clim).
                # Thus we can no longer ask MatPlotLib for "natural" bounds - just use data limits.
                naturalrange = dat['datarange'][:]
                
            # Get range forced by user
            if istimeaxis:
                mintime,maxtime = axisnode['MinimumTime'].getValue(),axisnode['MaximumTime'].getValue()
                if mintime!=None: mintime = common.date2num(mintime)
                if maxtime!=None: maxtime = common.date2num(maxtime)
                forcedrange = [mintime,maxtime]
            else:
                forcedrange = [axisnode['Minimum'].getValue(),axisnode['Maximum'].getValue()]
                
            # Make sure forced ranges are valid if log transform is applied.
            if axisnode['LogScale'].getValue(usedefault=True):
                if forcedrange[0]<=0: forcedrange[0] = None
                if forcedrange[1]<=0: forcedrange[1] = None
            
            # Effective range used by data, after taking forced range into account.
            effdatarange = dat['datarange'][:]
            if forcedrange[0]!=None: effdatarange[0] = forcedrange[0]
            if forcedrange[1]!=None: effdatarange[1] = forcedrange[1]

            # Effective range, combining natural range with user overrides.
            effrange = list(forcedrange)
            if effrange[0]==None: effrange[0]=naturalrange[0]
            if effrange[1]==None: effrange[1]=naturalrange[1]
            
            # The natural range will now only be used to set default axes bounds.
            # Filter out infinite values (valid in MatPlotLib but not in xmlstore)
            naturalrange = list(naturalrange)
            if not numpy.isfinite(naturalrange[0]): naturalrange[0] = None
            if not numpy.isfinite(naturalrange[1]): naturalrange[1] = None
            
            # Build default label for this axis
            deflab = dat['label']
            if dat['unit']!='' and dat['unit']!=None: deflab += ' ('+dat['unit']+')'
            
            # Set default axis properties.
            defaxisnode['Dimensions'].setValue(','.join(axis2dim[axisname]))
            defaxisnode['Label'].setValue(deflab)
            defaxisnode['Unit'].setValue(dat['unit'])
            defaxisnode['TicksMajor'].setValue(True)
            defaxisnode['TicksMajor/ShowLabels'].setValue(True)
            defaxisnode['TicksMinor'].setValue(False)
            defaxisnode['TicksMinor/ShowLabels'].setValue(False)
            defaxisnode['IsTimeAxis'].setValue(istimeaxis)

            # Get the MatPlotLib axis object.
            if axisname=='x':
                mplaxis = axes.get_xaxis()
            elif axisname=='y':
                mplaxis = axes.get_yaxis()
            else:
                mplaxis = cb.ax.get_yaxis()

            if istimeaxis:
                assert axisname!='colorbar', 'The color bar cannot be a time axis.'
                
                # Tick formats
                #DATEFORM number   DATEFORM string         Example
                #   0             'dd-mmm-yyyy HH:MM:SS'   01-Mar-2000 15:45:17 
                #   1             'dd-mmm-yyyy'            01-Mar-2000  
                #   2             'mm/dd/yy'               03/01/00     
                #   3             'mmm'                    Mar          
                #   4             'm'                      M            
                #   5             'mm'                     3            
                #   6             'mm/dd'                  03/01        
                #   7             'dd'                     1            
                #   8             'ddd'                    Wed          
                #   9             'd'                      W            
                #  10             'yyyy'                   2000         
                #  11             'yy'                     00           
                #  12             'mmmyy'                  Mar00        
                #  13             'HH:MM:SS'               15:45:17     
                #  14             'HH:MM:SS PM'             3:45:17 PM  
                #  15             'HH:MM'                  15:45        
                #  16             'HH:MM PM'                3:45 PM     
                #  17             'QQ-YY'                  Q1-01        
                #  18             'QQ'                     Q1        
                #  19             'dd/mm'                  01/03        
                #  20             'dd/mm/yy'               01/03/00     
                #  21             'mmm.dd,yyyy HH:MM:SS'   Mar.01,2000 15:45:17 
                #  22             'mmm.dd,yyyy'            Mar.01,2000  
                #  23             'mm/dd/yyyy'             03/01/2000 
                #  24             'dd/mm/yyyy'             01/03/2000 
                #  25             'yy/mm/dd'               00/03/01 
                #  26             'yyyy/mm/dd'             2000/03/01 
                #  27             'QQ-YYYY'                Q1-2001        
                #  28             'mmmyyyy'                Mar2000                               
                tickformats = {0:'%d-%b-%Y %H:%M:%S',
                                1:'%d-%b-%Y',
                                2:'%m/%d/%y',
                                3:'%b',
                                4:'%n',
                                5:'%m',
                                6:'%m/%d',
                                7:'%d',
                                8:'%a',
                                9:'%e',
                                10:'%Y',
                                11:'%y',
                                12:'%b%y',
                                13:'%H:%M:%S',
                                14:'%I:%M:%S %p',
                                15:'%H:%M',
                                16:'%I:%M %p',
                                17:'%Q-%y',
                                18:'%Q',
                                19:'%d/%m',
                                20:'%d/%m/%y',
                                21:'%b.%d,%Y %H:%M:%S',
                                22:'%b.%d,%Y',
                                23:'%m/%d/%Y',
                                24:'%d/%m/%Y',
                                25:'%y/%m/%d',
                                26:'%Y/%m/%d',
                                27:'%Q-%Y',
                                28:'%b%Y'}
                
                # Major ticks
                dayspan = (effdatarange[1]-effdatarange[0])
                location,interval,tickformat,tickspan = getTimeTickSettings(dayspan,axisnode['TicksMajor'],defaxisnode['TicksMajor'])
                mplaxis.set_major_locator(getTimeLocator(location,interval))
                assert tickformat in tickformats, 'Unknown tick format %i.' % tickformat
                mplaxis.set_major_formatter(CustomDateFormatter(tickformats[tickformat]))

                # Minor ticks
                location,interval,tickformat,tickspan = getTimeTickSettings(min(tickspan,dayspan),axisnode['TicksMinor'],defaxisnode['TicksMinor'])
                mplaxis.set_minor_locator(getTimeLocator(location,interval))
                assert tickformat in tickformats, 'Unknown tick format %i.' % tickformat
                #mplaxis.set_minor_formatter(CustomDateFormatter(tickformats[tickformat]))

                # Set the "natural" axis limits based on the data ranges.
                defaxisnode['MinimumTime'].setValue(common.num2date(naturalrange[0]))
                defaxisnode['MaximumTime'].setValue(common.num2date(naturalrange[1]))
            else:
                # Set the "natural" axis limits based on the data ranges.
                defaxisnode['Minimum'].setValue(naturalrange[0])
                defaxisnode['Maximum'].setValue(naturalrange[1])

            # Remove axis ticks if required.
            if not axisnode['TicksMajor'].getValue(usedefault=True):
                mplaxis.set_major_locator(matplotlib.ticker.FixedLocator([]))
            if not axisnode['TicksMinor'].getValue(usedefault=True):
                mplaxis.set_minor_locator(matplotlib.ticker.FixedLocator([]))

            # Obtain label for axis.
            label = axisnode['Label'].getValue(usedefault=True)
            if label==None: label=''

            # Reverse axis if needed
            if dat['reversed']: effrange[1],effrange[0] = effrange[0],effrange[1]

            # Set axis labels and boundaries.
            if axisname=='x':
                if label!='': axes.set_xlabel(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
                axes.set_xlim(effrange[0],effrange[1])
            elif axisname=='y':
                if label!='': axes.set_ylabel(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
                axes.set_ylim(effrange[0],effrange[1])
            elif axisname=='colorbar':
                assert cb!=None, 'No colorbar has been created.'
                if label!='': cb.set_label(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)

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
        for l in axes.get_xaxis().get_ticklabels():
            l.set_size(fontsizes['xtick.labelsize'])
            l.set_name(fontfamily)
        for l in axes.get_yaxis().get_ticklabels():
            l.set_size(fontsizes['ytick.labelsize'])
            l.set_name(fontfamily)
        offset = axes.get_xaxis().get_offset_text()
        offset.set_size(fontsizes['xtick.labelsize'])
        offset.set_name(fontfamily)
        offset = axes.get_yaxis().get_offset_text()
        offset.set_size(fontsizes['ytick.labelsize'])
        offset.set_name(fontfamily)
        
        # Scale text labels for color bar.
        if cb!=None:
            offset = cb.ax.yaxis.get_offset_text()
            offset.set_size(fontsizes['ytick.labelsize'])
            offset.set_name(fontfamily)
            for l in cb.ax.yaxis.get_ticklabels():
                l.set_size(fontsizes['ytick.labelsize'])
                l.set_name(fontfamily)

        # Draw the plot to screen.
        self.canvas.draw()
        
        for cb in self.callbacks['completeStateChange']: cb(len(forcedseries)>0)

        self.dirty = False
        
def setLineProperties(propertynode,mplsection='lines',**kwargs):
    """Sets the values under a xmlstore.TypedStore node describing line
    properties all at once.
    
    Internal use only. Used to quickly set default line properties.
    """
    deflinewidth = matplotlib.rcParams[mplsection+'.linewidth']
    deflinecolor = matplotlib.rcParams[mplsection+'.color']
    deflinecolor = matplotlib.colors.colorConverter.to_rgb(deflinecolor)
    deflinecolor = xmlstore.xmlstore.StoreColor.fromNormalized(*deflinecolor)
    deflinestyle = matplotlib.rcParams[mplsection+'.linestyle']
    linestyles = {'-':1,'--':2,'-.':3,':':4}
    if deflinestyle in linestyles:
        deflinestyle = linestyles[deflinestyle]
    else:
        deflinestyle = 0
    defmarkersize = matplotlib.rcParams.get(mplsection+'.markersize',6.)

    propertynode['CanHaveMarker'].setValue(kwargs.get('CanHaveMarker',True))
    propertynode['LineStyle'].setValue(kwargs.get('LineStyle',deflinestyle))
    propertynode['LineWidth'].setValue(kwargs.get('LineWidth',deflinewidth))
    propertynode['Color'].setValue(kwargs.get('Color',deflinecolor))
    propertynode['MarkerType'].setValue(kwargs.get('MarkerType',0))
    propertynode['MarkerSize'].setValue(kwargs.get('MarkerSize',defmarkersize))
    propertynode['MarkerFaceColor'].setValue(kwargs.get('MarkerFaceColor',deflinecolor))
    
def getLineProperties(propertynode):
    """Returns a dictionary with line properties based on the specified
    xmlstore.TypedStore node.
    
    Internal use only.
    """
    markertype = propertynode['MarkerType'].getValue(usedefault=True)
    markertypes = {0:'',1:'.',2:',',3:'o',4:'^',5:'s',6:'+',7:'x',8:'D'}
    markertype = markertypes[markertype]
    
    linestyle = propertynode['LineStyle'].getValue(usedefault=True)
    linestyles = {0:'',1:'-',2:'--',3:'-.',4:':'}
    linestyle = linestyles[linestyle]
    
    linewidth = propertynode['LineWidth'].getValue(usedefault=True)
    color = propertynode['Color'].getValue(usedefault=True)
    markersize = propertynode['MarkerSize'].getValue(usedefault=True)
    markerfacecolor = propertynode['MarkerFaceColor'].getValue(usedefault=True)
    
    return {'linestyle':linestyle,'marker':markertype,'linewidth':linewidth,'color':color.getNormalized(),'markersize':markersize,'markerfacecolor':markerfacecolor.getNormalized()}
    
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
        location,tickformat = 0,10
    elif dayspan>=61:
        location,tickformat = 1,4
    elif dayspan>=2:
        location,tickformat = 2,19
    elif 24*dayspan>=2:
        location,tickformat = 3,15
    else:
        location,tickformat = 4,15

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
    
    return location,interval,tickformat,interval*unitlengths[location]
