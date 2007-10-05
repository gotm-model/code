import datetime,math,os.path

import matplotlib
import matplotlib.numerix,matplotlib.numerix.ma,matplotlib.colors
import matplotlib.dates
import numpy

import common,xmlstore,data

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
        
class VariableTransform(data.PlotVariable):
    def __init__(self,sourcevar,nameprefix='',longnameprefix='',name=None,longname=None):
        data.PlotVariable.__init__(self,None)
        assert sourcevar!=None, 'The source variable for a transform cannot be None.'
        self.sourcevar = sourcevar
        if name==None:
            name = nameprefix + self.sourcevar.getName()
        if longname==None: 
            longname = longnameprefix + self.sourcevar.getLongName()
        self.name     = name
        self.longname = longname

    def getName(self):
        return self.name

    def getLongName(self):
        return self.longname

    def getUnit(self):
        return self.sourcevar.getUnit()

    def getDimensions(self):
        return self.sourcevar.getDimensions()

    def getDimensionInfo(self,dimname):
        return self.sourcevar.getDimensionInfo(dimname)

class VariableReduceDimension(VariableTransform):
    def __init__(self,variable,dimension,**kwargs):
        VariableTransform.__init__(self,variable,**kwargs)
        self.dimension = dimension

        dims = self.sourcevar.getDimensions()
        for (i,d) in enumerate(dims):
            if d==self.dimension: break
        else:
            assert False, 'Dimension "%s" is not present for this variable.' % self.dimension
        self.idimension = i

    def getDimensions(self):
        dims = self.sourcevar.getDimensions()
        return [d for d in dims if d!=self.dimension]
        
class VariableSlice(VariableReduceDimension):
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

class VariableAverage(VariableReduceDimension):

    def __init__(self,variable,dimname,centermeasure=0,boundsmeasure=0,percentilewidth=.5,**kwargs):
        dimlongname = variable.getDimensionInfo(dimname)['label']
        kwargs.setdefault('nameprefix',  'avg_')
        kwargs.setdefault('longnameprefix',dimlongname+'-averaged ')
        VariableReduceDimension.__init__(self,variable,dimname,**kwargs)
        self.centermeasure = centermeasure
        self.boundsmeasure = boundsmeasure
        self.percentilewidth = percentilewidth

    def getSlice(self,bounds):
        newbounds = list(bounds)
        newbounds.insert(self.idimension,(None,None))
        sourceslice = self.sourcevar.getSlice(newbounds)
        if not sourceslice.isValid(): return self.Slice()
        
        slice = self.Slice(self.getDimensions())
        for idim in range(len(sourceslice.coords)):
            if idim==self.idimension: continue
            coords = sourceslice.coords[idim]
            coords_stag = sourceslice.coords_stag[idim]
            if sourceslice.coords[idim].ndim>1:
                coords = coords.take((0,),self.idimension)
                coords_stag = coords_stag.take((0,),self.idimension)
                coords.shape = coords.shape[:self.idimension]+coords.shape[self.idimension+1:]
                coords_stag.shape = coords_stag.shape[:self.idimension]+coords_stag.shape[self.idimension+1:]
            itargetdim = idim
            if idim>self.idimension: itargetdim-=1
            slice.coords[itargetdim] = coords
            slice.coords_stag[itargetdim] = coords_stag
        
        weights = sourceslice.coords_stag[self.idimension]
        if weights.ndim==1:
            weights = common.replicateCoordinates(numpy.diff(weights),sourceslice.data,self.idimension)
        else:
            print weights.shape
            weights = numpy.diff(weights,axis=self.idimension)
            print weights.shape
            print sourceslice.data.shape
        
        # Normalize weights so their sum over the dimension to analyze equals one
        summedweights = weights.sum(axis=self.idimension)
        newshape = list(summedweights.shape)
        newshape.insert(self.idimension,1)
        weights /= summedweights.reshape(newshape).repeat(sourceslice.data.shape[self.idimension],self.idimension)
        
        if self.centermeasure==0 or self.boundsmeasure==0:
            # We need the mean and/or standard deviation. Calculate the mean,
            # which is needed for either measure.
            mean = (sourceslice.data*weights).sum(axis=self.idimension)
        
        if self.centermeasure==1 or self.boundsmeasure==1:
            # We will need percentiles. Sort the data along dimension to analyze,
            # and calculate cumulative (weigth-based) distribution.
            
            # Sort the data along the dimension to analyze, and sort weights
            # in the same order
            sortedindices = sourceslice.data.argsort(axis=self.idimension)
            sorteddata    = common.argtake(sourceslice.data,sortedindices,axis=self.idimension)
            sortedweights = common.argtake(weights,sortedindices,self.idimension)
            
            # Calculate cumulative distribution values along dimension to analyze.
            cumsortedweights = sortedweights.cumsum(axis=self.idimension)
            
            # Calculate coordinates for interfaces between data points, to be used
            # as grid for cumulative distribution
            sorteddata = (numpy.concatenate((sorteddata.take((0,),axis=self.idimension),sorteddata),axis=self.idimension) + numpy.concatenate((sorteddata,sorteddata.take((-1,),axis=self.idimension)),axis=self.idimension))/2.
            cumsortedweights = numpy.concatenate((numpy.zeros(cumsortedweights.take((0,),axis=self.idimension).shape,cumsortedweights.dtype),cumsortedweights),axis=self.idimension)
        
        if self.centermeasure==0:
            # Use mean for center
            slice.data = mean
        elif self.centermeasure==1:
            # Use median for center
            slice.data = common.getPercentile(sorteddata,cumsortedweights,.5,self.idimension)
        else:
            assert False, 'Unknown choice %i for center measure.' % self.centermeasure

        if self.boundsmeasure==0:
            # Standard deviation will be used as bounds.
            var = (sourceslice.data**2*weights).sum(axis=self.idimension) - mean**2
            sd = numpy.sqrt(var)
            slice.lbound = slice.data-sd
            slice.ubound = slice.data+sd
        elif self.boundsmeasure==1:
            # Percentiles will be used as bounds.
            lowcrit = (1.-self.percentilewidth)/2.
            highcrit = 1.-lowcrit
            slice.lbound = common.getPercentile(sorteddata,cumsortedweights, lowcrit,self.idimension)
            slice.ubound = common.getPercentile(sorteddata,cumsortedweights,highcrit,self.idimension)
        else:
            assert False, 'Unknown choice %i for bounds measure.' % self.boundsmeasure
        
        return slice

class VariableFlat(VariableReduceDimension):

    def __init__(self,variable,dimname,targetdim,**kwargs):
        dimlongname = variable.getDimensionInfo(dimname)['label']
        kwargs.setdefault('nameprefix',  'flat_')
        kwargs.setdefault('longnameprefix',dimlongname+'-combined ')
        VariableReduceDimension.__init__(self,variable,dimname,**kwargs)
        
        self.targetdim = targetdim
        self.itargetdim = list(self.sourcevar.getDimensions()).index(self.targetdim)
        self.inewtargetdim = self.itargetdim
        if self.idimension<self.itargetdim: self.inewtargetdim -= 1
        
    def getSlice(self,bounds):
        assert len(bounds)==len(self.sourcevar.getDimensions())-1, 'Invalid number of dimension specified.'
        
        newbounds = list(bounds)
        newbounds.insert(self.idimension,(None,None))
        sourceslice = self.sourcevar.getSlice(newbounds)
        newslice = self.Slice(self.getDimensions())
        
        assert sourceslice.coords[self.idimension].ndim==1,'Currently, the dimension to flatten cannot depend on other dimensions.'
        assert sourceslice.coords[self.itargetdim].ndim==1,'Currently, the dimension to absorb flattened values cannot depend on other dimensions.'
        
        # Get length of dimension to flatten, and of dimension to take flattened values.
        sourcecount = sourceslice.coords[self.idimension].shape[0]
        targetcount = sourceslice.coords[self.itargetdim].shape[0]

        # Create new coordinates for dimension that absorbs flattened values.
        newtargetcoords = matplotlib.numerix.empty((targetcount*sourcecount,),matplotlib.numerix.typecode(sourceslice.coords[self.itargetdim]))
        
        # Create a new value array.
        newdatashape = list(sourceslice.data.shape)
        newdatashape[self.itargetdim] *= sourcecount
        del newdatashape[self.idimension]
        newdata = matplotlib.numerix.empty(newdatashape,matplotlib.numerix.typecode(sourceslice.data))
            
        for i in range(0,targetcount):
            newtargetcoords[i*sourcecount:(i+1)*sourcecount] = sourceslice.coords[self.itargetdim][i]
            for j in range(0,sourcecount):
                sourceindices = [slice(0,None,1) for k in range(sourceslice.ndim)]
                sourceindices[self.itargetdim] = slice(i,i+1,1)
                sourceindices[self.idimension] = slice(j,j+1,1)
                targetindices = [slice(0,None,1) for k in range(newdata.ndim)]
                targetindices[self.inewtargetdim] = slice(i*sourcecount+j,i*sourcecount+j+1,1)
                newdata[tuple(targetindices)] = sourceslice.data[tuple(sourceindices)].copy()

        newslice.coords      = [c for i,c in enumerate(sourceslice.coords     ) if i!=self.idimension]
        newslice.coords_stag = [c for i,c in enumerate(sourceslice.coords_stag) if i!=self.idimension]
        newslice.coords[self.inewtargetdim] = newtargetcoords
        newslice.data = newdata
        return newslice
        
class Figure(common.referencedobject):

    schemadirname = 'schemas/figure'
    @staticmethod
    def setRoot(rootpath):
        Figure.schemadirname = os.path.join(rootpath,'schemas/figure')

    def __init__(self,figure=None,size=(10,8),defaultfont=None):
        common.referencedobject.__init__(self)

        # If not matPlotLib figure is specified, create a new one, assuming
        # we want to export to file.        
        if figure==None:
            figure = matplotlib.figure.Figure(figsize=(size[0]/2.54,size[1]/2.54))
            canvas = matplotlib.backends.backend_agg.FigureCanvasAgg(figure)
        
        # If no default font is specified, use the MatPlotLib default.
        if defaultfont==None:
            defaultfont = matplotlib.font_manager.FontProperties().get_name()
        
        self.figure = figure
        self.canvas = figure.canvas

        # Create store for the explicitly set properties
        self.properties = xmlstore.TypedStore(os.path.join(Figure.schemadirname,'gotmgui.xml'))
        self.propertiesinterface = self.properties.getInterface()
        self.propertiesinterface.notifyOnDefaultChange = False
        self.propertiesinterface.connect('afterChange',self.onPropertyChanged)
        self.propertiesinterface.connect('afterStoreChange',self.onPropertyStoreChanged)
        
        # Create store for property defaults
        self.defaultproperties = xmlstore.TypedStore(os.path.join(Figure.schemadirname,'gotmgui.xml'))

        # Set some default properties.
        self.defaultproperties.setProperty('FontName',defaultfont)
        self.defaultproperties.setProperty('FontScaling',100)
        self.defaultproperties.setProperty('Grid',False)
        self.defaultproperties.setProperty('Legend/Location',0)
        setLineProperties(self.defaultproperties.root['Grid/LineProperties'],CanHaveMarker=False,mplsection='grid')

        self.properties.setDefaultStore(self.defaultproperties)

        self.sources = {}
        self.defaultsource = None
        self.updating = True
        self.dirty = False
        self.haschanged = False
        
        self.callbacks = {'completeStateChange':[]}
        
    def unlink(self):
        self.propertiesinterface = None
        self.defaultproperties.release()
        self.defaultproperties = None
        self.properties.release()
        self.properties = None
        
    def registerCallback(self,eventname,callback):
        assert eventname in self.callbacks, 'Event "%s" is unknown.' % eventname
        self.callbacks[eventname].append(callback)

    def setUpdating(self,allowupdates):
        if self.updating != allowupdates:
            self.updating = allowupdates
            if allowupdates and self.dirty: self.update()

    def onPropertyChanged(self,node,feature):
        if feature=='value':
            self.onPropertyStoreChanged()

    def onPropertyStoreChanged(self):
        self.haschanged = True
        self.update()

    def clearSources(self):
        self.sources = {}
        self.defaultsource = None

    def addDataSource(self,name,obj):
        self.sources[name] = obj
        if self.defaultsource==None: self.defaultsource = name

    def clearProperties(self):
        self.properties.root.clearValue(recursive=True)

    def setProperties(self,props):
        self.properties.setStore(props)
        self.update()

    def getPropertiesCopy(self):
        return self.properties.toXmlDom()

    def clearVariables(self):
        self.properties.root['Data'].removeChildren('Series')

    def addVariable(self,varname,source=None,replace=True):
        datanode = self.properties.root['Data']
        varpath = '/'+varname
        if source!=None: varpath = source+varpath
        if replace:
            series = datanode.getChildById('Series',varpath,create=True)
            self.defaultproperties.root['Data'].getChildById('Series',varpath,create=True)
        else:
            series = datanode.addChild('Series',id=varpath)
            self.defaultproperties.root['Data'].addChild('Series',id=varpath)
        self.update()
        return series

    def hasChanged(self):
        return self.haschanged

    def resetChanged(self):
        self.haschanged = False
        
    def exportToFile(self,path,dpi=150):
        self.canvas.print_figure(path,dpi=dpi)

    def update(self):
        if not self.updating:
            self.dirty = True
            return

        self.figure.clear()

        axes = self.figure.add_subplot(111)
        
        textscaling = self.properties.getProperty('FontScaling',usedefault=True)/100.
        
        # First scale the default font size; this takes care of all relative font sizes (e.g. "small")
        matplotlib.font_manager.fontManager.set_default_size(textscaling*matplotlib.rcParams['font.size'])
        
        # Now get some relevant font sizes.
        # Scale font sizes with text scaling parameter if they are absolute sizes.
        # (if they are strings, they are relative sizes already)
        fontfamily = self.properties.getProperty('FontName',usedefault=True)
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
        dim2data = {}
        defaultaxes = self.defaultproperties.root['Axes']
        forcedaxes = self.properties.root['Axes']
        for forcedaxis in forcedaxes.getLocationMultiple(['Axis']):
            istimeaxis = forcedaxis['IsTimeAxis'].getValueOrDefault()
            if istimeaxis:
                axmin = forcedaxis['MinimumTime'].getValue()
                axmax = forcedaxis['MaximumTime'].getValue()
                if axmin!=None: axmin = common.date2num(axmin)
                if axmax!=None: axmax = common.date2num(axmax)
            else:
                axmin = forcedaxis['Minimum'].getValue()
                axmax = forcedaxis['Maximum'].getValue()
            dim2data[forcedaxis.getSecondaryId()] = {'forcedrange':[axmin,axmax]}

        # Shortcuts to the nodes specifying the variables to plot.
        forceddatanode = self.properties.root['Data']
        forcedseries = forceddatanode.getLocationMultiple(['Series'])

        # Shortcut to the node that will hold defaults for the plotted variables.
        defaultdatanode = self.defaultproperties.root['Data']
        olddefaults = [node.getSecondaryId() for node in defaultdatanode.getLocationMultiple(['Series'])]

        # This variable will hold all long names of the plotted variables.
        # It will be used to create the plot title.
        titles = []
        
        # No colorbar created (yet).
        cb = None
        zorder = 0
        plotcount = {1:0,2:0}

        for (iseries,seriesnode) in enumerate(forcedseries):
            varpath = seriesnode.getSecondaryId()
            varsource,varname = varpath.split('/',1)
            if varsource=='':
                # No data source specified; take default.
                assert self.defaultsource!=None, 'No data source set for variable "%s", but no default source available either.' % varname
                varsource = self.defaultsource
                
            # Get variable object.
            varstore = self.sources[varsource]
            var = varstore.getVariable(varname)
            assert var!=None, 'Source "%s" does not contain variable with name "%s".' % (varsource,varname)
            longname = var.getLongName()
            
            # Create default series information
            defaultseriesnode = defaultdatanode.getChildById('Series',varpath,create=True)
            defaultseriesnode['Label'].setValue(longname)
            defaultseriesnode['PlotType3D'].setValue(0)
            defaultseriesnode['LogScale'].setValue(False)
            defaultseriesnode['HasConfidenceLimits'].setValue(False)
            setLineProperties(defaultseriesnode['LineProperties'])
            label = seriesnode['Label'].getValueOrDefault()
            
            # Old defaults will be removed after all series are plotted.
            # Register that the current variable is active, ensuring its default will remain.
            if varpath in olddefaults: olddefaults.remove(varpath)

            # Store the variable long name (to be used for building title)
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
                        dimbounds.append(forcedrange)
                else:
                    # No boundaries set.
                    dimbounds.append((None,None))
                    
            # Get the data
            varslice = var.getSlice(dimbounds)
            
            # Skip this variable if no data are available.
            if not varslice.isValid(): continue

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
            dimdata.update({'label':var.getLongName(),'unit':var.getUnit(),'datatype':'float','logscale':False,'tight':False})
            
            # Find non-singleton dimensions (singleton dimension: dimension with length one)
            # Store singleton dimensions as fixed extra coordinates.
            varslice = varslice.squeeze()

            defaultseriesnode['DimensionCount'].setValue(varslice.ndim)

            # Get the plot type for 3D plots.
            plottype3d = seriesnode['PlotType3D'].getValueOrDefault()

            # We use a staggered grid (coordinates at interfaces,
            # values at centers) for certain 3D plot types.
            staggered = (varslice.ndim==2 and plottype3d==0)
            
            # Create shortcut to applicable coordinate set.
            if staggered:
                coords = varslice.coords_stag
            else:
                coords = varslice.coords

            # Get the minimum and maximum values; store these as default.
            dim2data[varpath]['datarange'] = [varslice.data.min(),varslice.data.max()]

            # Mask values that are not within (minimum, maximum) range.
            #minimum = seriesnode['Minimum'].getValue()
            #maximum = seriesnode['Maximum'].getValue()
            #if minimum!=None and maximum!=None:
            #    data[-1] = matplotlib.numerix.ma.masked_array(data[-1],matplotlib.numerix.logical_or(data[-1]<minimum, data[-1]>maximum))
            #elif minimum!=None:
            #    data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<minimum)
            #elif maximum!=None:
            #    data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]>maximum)

            # Transform to log-scale if needed (first mask values <= zero)
            logscale = seriesnode['LogScale'].getValueOrDefault()
            if logscale:
                #data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<=0.)
                #data[-1] = matplotlib.numerix.ma.log10(data[-1])
                dim2data[varpath]['logscale'] = True

            # Get label
            #defaultlabel = '%s (%s)' % (var.getLongName(),var.getUnit())
            #if logscale: defaultlabel = 'log10 '+defaultlabel
            #defaultseriesnode['Label'].setValue(defaultlabel)
            #label = seriesnode['Label'].getValueOrDefault()

            # Enumerate over the dimension of the variable.
            for idim,dimname in enumerate(varslice.dimensions):
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
                defaultseriesnode['LineProperties/Color'].setValue(xmlstore.StoreColor(*linecolors[plotcount[1]%len(linecolors)]))
                plotargs = getLineProperties(seriesnode['LineProperties'])
                
                # plot confidence interval (if any)
                hasconfidencelimits = (varslice.ubound!=None or varslice.lbound!=None)
                defaultseriesnode['HasConfidenceLimits'].setValue(hasconfidencelimits)
                if hasconfidencelimits:
                    ubound = varslice.ubound
                    if ubound==None: ubound = varslice.data
                    lbound = varslice.lbound
                    if lbound==None: lbound = varslice.data
                    
                    if seriesnode['LineProperties/MarkerType'].getValueOrDefault()==0:
                        defaultseriesnode['ConfidenceLimits/Style'].setValue(2)
                    else:
                        defaultseriesnode['ConfidenceLimits/Style'].setValue(1)
                    errorbartype = seriesnode['ConfidenceLimits/Style'].getValueOrDefault()
                    
                    if errorbartype==0:
                        pass
                    elif errorbartype==1:
                        # Plot error bars
                        xerr = None
                        yerr = numpy.vstack((varslice.data-lbound,ubound-varslice.data))
                        if switchaxes: xerr,yerr = yerr,xerr
                        (line,errbars) = axes.errorbar(X,Y,fmt=None,xerr=xerr,yerr=yerr,ecolor=plotargs['color'],zorder=zorder)
                    elif errorbartype==2:
                        # Plot shaded confidence area (filled polygon)
                        errX = numpy.hstack((varslice.coords[0],varslice.coords[0][::-1]))
                        errY = numpy.hstack((lbound,ubound[::-1]))
                        if switchaxes: errX,errY = errY,errX
                        areacolor = seriesnode['LineProperties/Color'].getValueOrDefault()
                        areacolor.brighten(.5)
                        alpha = .7
                        axes.fill(errX,errY,facecolor=areacolor.getNormalized(),linewidth=0, alpha=alpha, zorder=zorder)
                    else:
                        assert False, 'Unknown error bar type %i.' % errorbartype
                    zorder += 1
                
                # Plot line and/or markers
                if plotargs['linestyle']!='' or plotargs['marker']!='':
                    lines = axes.plot(X,Y,zorder=zorder,label=label,**plotargs)
                
                dim2data[xname]['axis'] = 'x'
                dim2data[yname]['axis'] = 'y'
                
                plotcount[1] += 1
            elif varslice.ndim==2:
                # Two-dimensional coordinate space (x,y). Use x-axis for first coordinate dimension,
                # and y-axis for second coordinate dimension.
                xdim = 0
                ydim = 1
                prefaxis = (dim2data[varslice.dimensions[0]]['preferredaxis'],dim2data[varslice.dimensions[1]]['preferredaxis'])
                if (prefaxis[0]=='y' and prefaxis[1]!='y') or (prefaxis[1]=='x' and prefaxis[0]!='x'):
                    # One independent dimension prefers to switch axis and the
                    # other does not disagree.
                    xdim = 1
                    ydim = 0

                dim2data[varslice.dimensions[xdim]]['axis'] = 'x'
                dim2data[varslice.dimensions[ydim]]['axis'] = 'y'
                dim2data[varpath                  ]['axis'] = 'colorbar'

                X = coords[xdim]
                Y = coords[ydim]
                Z = varslice.data
                
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
                    
                # Adjust Z dimension.
                if xdim<ydim:
                    Z = Z.transpose()
                
                norm = None
                if logscale: norm = matplotlib.colors.LogNorm()

                if plottype3d==1:
                    loc = None
                    if logscale: loc = matplotlib.ticker.LogLocator()

                    cc = seriesnode['ContourCount'].getValue()
                    if cc!=None:
                        pc = axes.contourf(X,Y,Z,cc,norm=norm,locator=loc,zorder=zorder)
                    else:
                        pc = axes.contourf(X,Y,Z,norm=norm,locator=loc,zorder=zorder)
                    if cc==None:
                      defaultseriesnode['ContourCount'].setValue(len(pc.levels)-2)
                else:
                    pc = axes.pcolormesh(X,Y,Z,shading='flat', cmap=matplotlib.cm.jet,norm=norm)
                  
                # Create colorbar
                assert cb==None, 'Currently only one object that needs a colorbar is supported per figure.'
                if isinstance(Z,matplotlib.numerix.ma.MaskedArray):
                    flatZ = Z.compressed()
                else:
                    flatZ = Z.ravel()
                if (flatZ==flatZ[0]).all():
                    # All z values are equal. Explicitly set color range,
                    # because MatPlotLib 0.90.0 chokes on identical min and max.
                    pc.set_clim((Z[0,0]-1,Z[0,0]+1))
                else:
                    pc.set_clim(dim2data[varpath]['forcedrange'])
                cb = self.figure.colorbar(pc)

                plotcount[2] += 1
            
            # Increase z-order.
            zorder += 1

            # Hold all plot properties so we can plot additional data series.
            axes.hold(True)

        # Remove unused default series
        # (remaining from previous plots that had these other data series)
        for oldname in olddefaults:
            defaultdatanode.removeChild('Series',oldname)

        # Create and store title
        title = titles[0]
        for ln in titles[1:]:
            if ln!=title:
                title = ', '.join(titles)
                break
        self.defaultproperties.setProperty('Title',title)
        title = self.properties.getProperty('Title',usedefault=True)
        assert title!=None, 'Title must be available, either explicitly set or as default.'
        if title!='': axes.set_title(title,size=fontsizes['axes.titlesize'],fontname=fontfamily)
        
        # Show legend
        legend = None
        self.defaultproperties.setProperty('CanHaveLegend',plotcount[1]>0)
        if plotcount[1]>0:
            self.defaultproperties.setProperty('Legend',plotcount[1]>1)
            legprop = self.properties.root['Legend']
            if legprop.getValueOrDefault():
                legend = axes.legend(loc=legprop['Location'].getValueOrDefault(),prop=matplotlib.font_manager.FontProperties(size=fontsizes['legend'],family=fontfamily))

        # Build table linking axis to data dimension.
        axis2dim = dict([(dat['axis'],dim) for dim,dat in dim2data.iteritems() if 'axis' in dat])

        # Get effective ranges for each dimension (based on forced limits and natural data ranges)
        for axisname in ('x','y','z','colorbar'):
            if axisname not in axis2dim: continue
            
            dim = axis2dim[axisname]
            dat = dim2data[dim]
            
            # Range selected by MatPlotLib
            if axisname=='x' and not dat.get('tight',True):
                naturalrange = axes.get_xlim()
            elif axisname=='y' and not dat.get('tight',True):
                naturalrange = axes.get_ylim()
            else:
                naturalrange = dat['datarange'][:]
                
            # Range forced by user
            forcedrange = dat.get('forcedrange',(None,None))
            
            # Effective range used by data, after taking forced range into account.
            effdatarange = dat['datarange'][:]
            if forcedrange[0]!=None: effdatarange[0] = forcedrange[0]
            if forcedrange[1]!=None: effdatarange[1] = forcedrange[1]

            # Effective forced range, combining natural range with user overrides.
            effrange = list(forcedrange)
            if effrange[0]==None: effrange[0]=naturalrange[0]
            if effrange[1]==None: effrange[1]=naturalrange[1]
            
            # Get the explicitly set and the default properties.
            axisnode = forcedaxes.getChildById('Axis',dim,create=True)
            defaxisnode = defaultaxes.getChildById('Axis',dim,create=True)

            # Build default label for this axis
            deflab = dat['label']
            if dat['unit']!='': deflab += ' ('+dat['unit']+')'
            defaxisnode['Label'].setValue(deflab)
            defaxisnode['Unit'].setValue(dat['unit'])
            defaxisnode['TicksMajor'].setValue(True)
            defaxisnode['TicksMajor/ShowLabels'].setValue(True)
            defaxisnode['TicksMinor'].setValue(False)
            defaxisnode['TicksMinor/ShowLabels'].setValue(False)

            istimeaxis = dat['datatype']=='datetime'
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
            if not axisnode['TicksMajor'].getValueOrDefault():
                mplaxis.set_major_locator(matplotlib.ticker.FixedLocator([]))
            if not axisnode['TicksMinor'].getValueOrDefault():
                mplaxis.set_minor_locator(matplotlib.ticker.FixedLocator([]))

            # Obtain label for axis.
            label = axisnode['Label'].getValueOrDefault()
            if label==None: label=''

            # Set axis labels and boundaries.
            if axisname=='x':
                if label!='': axes.set_xlabel(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
                axes.set_xlim(effrange[0],effrange[1])
                if dat['logscale']: axes.set_xscale('log')
            elif axisname=='y':
                if label!='': axes.set_ylabel(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
                axes.set_ylim(effrange[0],effrange[1])
                if dat['logscale']: axes.set_yscale('log')
            elif axisname=='colorbar':
                assert cb!=None, 'No colorbar has been created.'
                if label!='': cb.set_label(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
                
        # Create grid
        gridnode = self.properties.root['Grid']
        if gridnode.getValueOrDefault():
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
    deflinewidth = matplotlib.rcParams[mplsection+'.linewidth']
    deflinecolor = matplotlib.rcParams[mplsection+'.color']
    deflinecolor = matplotlib.colors.colorConverter.to_rgb(deflinecolor)
    deflinecolor = xmlstore.StoreColor.fromNormalized(*deflinecolor)
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
    markertype = propertynode['MarkerType'].getValueOrDefault()
    markertypes = {0:'',1:'.',2:',',3:'o',4:'^',5:'s',6:'+',7:'x',8:'D'}
    markertype = markertypes[markertype]
    
    linestyle = propertynode['LineStyle'].getValueOrDefault()
    linestyles = {0:'',1:'-',2:'--',3:'-.',4:':'}
    linestyle = linestyles[linestyle]
    
    linewidth = propertynode['LineWidth'].getValueOrDefault()
    color = propertynode['Color'].getValueOrDefault()
    markersize = propertynode['MarkerSize'].getValueOrDefault()
    markerfacecolor = propertynode['MarkerFaceColor'].getValueOrDefault()
    
    return {'linestyle':linestyle,'marker':markertype,'linewidth':linewidth,'color':color.getNormalized(),'markersize':markersize,'markerfacecolor':markerfacecolor.getNormalized()}
    
def getTimeLocator(location,interval):
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
    
def getTimeTickSettings(dayspan,settings,defsettings):
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
    location   = settings['LocationTime'].getValueOrDefault()
    tickformat = settings['FormatTime'].getValueOrDefault()

    # Calculate optimal interval between ticks, aiming for max. 8 ticks total.
    tickcount = dayspan/unitlengths[location]
    interval = math.ceil(tickcount/8.)
    if interval<1: interval = 1
    
    # Save default tick interval, then get effective tick interval.
    defsettings['IntervalTime'].setValue(interval)
    interval = settings['IntervalTime'].getValueOrDefault()

    # Make sure we do not plot more than 100 ticks: non-informative and very slow!
    if tickcount/interval>100: interval=math.ceil(tickcount/100.)
    
    return location,interval,tickformat,interval*unitlengths[location]
