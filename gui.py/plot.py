import datetime,math,os.path

import matplotlib
import matplotlib.numerix,matplotlib.numerix.ma,matplotlib.colors
import matplotlib.dates

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

    def getValues(self,bounds,staggered=False,coordinatesonly=False):
        bounds.insert(self.idimension,(self.sliceval,self.sliceval))
        dims = self.getDimensions()
        data = self.sourcevar.getValues(bounds,staggered=staggered,coordinatesonly=coordinatesonly)
        if data==None or 0 in data[-1].shape: return None
        assert data[self.idimension].ndim==1, 'Slicing is not (yet) supported for dimensions that have coordinates that depend on other dimensions.'
        ipos = data[self.idimension].searchsorted(self.sliceval)
        if ipos==0 or ipos>=data[self.idimension].shape[0]: return None
        leftx  = data[self.idimension][ipos-1]
        rightx = data[self.idimension][ipos]
        deltax = rightx-leftx
        stepx = self.sliceval-leftx
        if isinstance(deltax,datetime.timedelta):
            relstep = common.timedelta2float(stepx)/common.timedelta2float(deltax)
        else:
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
        return data

class VariableAverage(VariableReduceDimension):

    def __init__(self,variable,dimname,**kwargs):
        dimlongname = variable.getDimensionInfo(dimname)['label']
        kwargs.setdefault('nameprefix',  'avg_')
        kwargs.setdefault('longnameprefix',dimlongname+'-averaged ')
        VariableReduceDimension.__init__(self,variable,dimname,**kwargs)

    def getValues(self,bounds,staggered=False,coordinatesonly=False):
        newbounds = list(bounds)
        newbounds.insert(self.idimension,(None,None))
        vals = self.sourcevar.getValues(newbounds,staggered=staggered,coordinatesonly=coordinatesonly)
        
        del vals[self.idimension]
        count = vals[-1].shape[self.idimension]
        vals[-1] = vals[-1].sum(axis=self.idimension)/float(count)
        return vals

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
        
    def getValues(self,bounds,staggered=False,coordinatesonly=False):
        assert len(bounds)==len(self.sourcevar.getDimensions())-1, 'Invalid number of dimension specified.'
        
        newbounds = list(bounds)
        newbounds.insert(self.idimension,(None,None))
        vals = self.sourcevar.getValues(newbounds,staggered=staggered,coordinatesonly=coordinatesonly)
        
        assert vals[self.idimension].ndim==1,'Currently, the dimension to flatten cannot depend on other dimensions.'
        assert vals[self.itargetdim].ndim==1,'Currently, the dimension to absorb flattened values cannot depend on other dimensions.'
        
        # Get length of dimension to flatten, and of dimension to take flattened values.
        sourcecount = vals[self.idimension].shape[0]
        targetcount = vals[self.itargetdim].shape[0]

        # Create new coordinates for dimension that absorbs flattened values.
        newtargetcoords = matplotlib.numerix.empty((targetcount*sourcecount,),matplotlib.numerix.typecode(vals[self.itargetdim]))
        
        # Create a new value array.
        if not coordinatesonly:
            newdatashape = list(vals[-1].shape)
            newdatashape[self.itargetdim] *= sourcecount
            del newdatashape[self.idimension]
            newdata = matplotlib.numerix.empty(newdatashape,matplotlib.numerix.typecode(vals[-1]))
            
        for i in range(0,targetcount):
            newtargetcoords[i*sourcecount:(i+1)*sourcecount] = vals[self.itargetdim][i]
            if not coordinatesonly:
                for j in range(0,sourcecount):
                    sourceindices = [slice(0,None,1) for k in range(vals[-1].ndim)]
                    sourceindices[self.itargetdim] = slice(i,i+1,1)
                    sourceindices[self.idimension] = slice(j,j+1,1)
                    targetindices = [slice(0,None,1) for k in range(newdata.ndim)]
                    targetindices[self.inewtargetdim] = slice(i*sourcecount+j,i*sourcecount+j+1,1)
                    newdata[tuple(targetindices)] = vals[-1][tuple(sourceindices)].copy()

        vals[self.itargetdim] = newtargetcoords
        del vals[self.idimension]
        if not coordinatesonly: vals[-1] = newdata
        return vals
        
class Figure(common.referencedobject):

    schemadirname = 'schemas/figure'
    @staticmethod
    def setRoot(rootpath):
        Figure.schemadirname = os.path.join(rootpath,'schemas/figure')

    def __init__(self,figure,defaultfont=None):
        common.referencedobject.__init__(self)
        
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
        self.defaultproperties.setProperty(['FontName'],defaultfont)
        self.defaultproperties.setProperty(['FontScaling'],100)

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
        self.properties.root.getLocation(['Data']).removeChildren('Series')

    def addVariable(self,varname,source=None):
        datanode = self.properties.root.getLocation(['Data'])
        varpath = '/'+varname
        if source!=None: varpath = source+varpath
        series = datanode.addChild('Series',id=varpath)
        self.update()

    def hasChanged(self):
        return self.haschanged

    def resetChanged(self):
        self.haschanged = False

    def update(self):
        if not self.updating:
            self.dirty = True
            return

        self.figure.clear()

        axes = self.figure.add_subplot(111)
        
        textscaling = self.properties.getProperty(['FontScaling'],usedefault=True)/100.
        
        # First scale the default font size; this takes care of all relative font sizes (e.g. "small")
        matplotlib.font_manager.fontManager.set_default_size(textscaling*matplotlib.rcParams['font.size'])
        
        # Now get some relevant font sizes.
        # Scale font sizes with text scaling parameter if they are absolute sizes.
        # (if they are strings, they are relative sizes already)
        fontfamily = self.properties.getProperty(['FontName'],usedefault=True)
        fontsizes = {
            'axes.titlesize' :10, #matplotlib.rcParams['axes.titlesize'],
            'axes.labelsize' :8, #matplotlib.rcParams['axes.labelsize'],
            'xtick.labelsize':8, #matplotlib.rcParams['xtick.labelsize'],
            'ytick.labelsize':8, #matplotlib.rcParams['ytick.labelsize']
        }
        for k,v in fontsizes.iteritems():
            if not isinstance(v,basestring): fontsizes[k]=v*textscaling

        # Get the default line proeprties
        deflinewidth = matplotlib.rcParams['lines.linewidth']
        deflinecolor = matplotlib.rcParams['lines.color']
        deflinecolor = matplotlib.colors.colorConverter.to_rgb(deflinecolor)
        deflinecolor = xmlstore.StoreColor.fromNormalized(*deflinecolor)
        defmarkersize = matplotlib.rcParams['lines.markersize']

        # Get forced axes boundaries (will be None if not set; then we autoscale)
        dim2data = {}
        defaultaxes = self.defaultproperties.root.getLocation(['Axes'])
        forcedaxes = self.properties.root.getLocation(['Axes'])
        for forcedaxis in forcedaxes.getLocationMultiple(['Axis']):
            istimeaxis = forcedaxis.getLocation(['IsTimeAxis']).getValueOrDefault()
            if istimeaxis:
                axmin = forcedaxis.getLocation(['MinimumTime']).getValue()
                axmax = forcedaxis.getLocation(['MaximumTime']).getValue()
            else:
                axmin = forcedaxis.getLocation(['Minimum']).getValue()
                axmax = forcedaxis.getLocation(['Maximum']).getValue()
            dim2data[forcedaxis.getSecondaryId()] = {'forcedrange':[axmin,axmax]}

        # Shortcuts to the nodes specifying the variables to plot.
        forceddatanode = self.properties.root.getLocation(['Data'])
        forcedseries = forceddatanode.getLocationMultiple(['Series'])

        # Shortcut to the node that will hold defaults for the plotted variables.
        defaultdatanode = self.defaultproperties.root.getLocation(['Data'])
        olddefaults = [node.getSecondaryId() for node in defaultdatanode.getLocationMultiple(['Series'])]

        # This variable will hold all long names of the plotted variables.
        # It will be used to create the plot title.
        longnames = []
        
        # No colorbar created (yet).
        cb = None

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
            
            # Create default series information
            defaultseriesnode = defaultdatanode.getChildById('Series',varpath,create=True)
            defaultseriesnode.getLocation(['PlotType2D']).setValue(0)
            defaultseriesnode.getLocation(['PlotType3D']).setValue(0)
            defaultseriesnode.getLocation(['LineWidth']).setValue(deflinewidth)
            defaultseriesnode.getLocation(['Color']).setValue(deflinecolor)
            defaultseriesnode.getLocation(['MarkerType']).setValue(2)
            defaultseriesnode.getLocation(['MarkerSize']).setValue(defmarkersize)
            defaultseriesnode.getLocation(['MarkerFaceColor']).setValue(deflinecolor)
            defaultseriesnode.getLocation(['LogScale']).setValue(False)
            
            # Old defaults will be removed after all series are plotted.
            # Register that the current variable is active, ensuring its default will remain.
            if varpath in olddefaults: olddefaults.remove(varpath)

            # Store the variable long name (to be used for building title)
            longnames.append(var.getLongName())

            # Build list of dimension boundaries for current variable.
            # For dimensions that have equal lower and upper bound, take a slice.
            dimbounds = []
            originaldims = var.getDimensions()
            for dimname in originaldims:
                if dimname in dim2data:
                    # We have boundaries set on the current dimension.
                    forcedrange = dim2data[dimname].get('forcedrange',(None,None))
                    if var.getDimensionInfo(dimname)['datatype']=='datetime':
                        if forcedrange[0]!=None: forcedrange[0] = common.date2num(forcedrange[0])
                        if forcedrange[1]!=None: forcedrange[1] = common.date2num(forcedrange[1])
                    if forcedrange[0]==forcedrange[1] and forcedrange[0]!=None:
                        # Equal upper and lower boundary: take a slice.
                        var = VariableSlice(var,dimname,forcedrange[0])
                    else:
                        dimbounds.append(forcedrange)
                else:
                    # No boundaries set.
                    dimbounds.append((None,None))
                    
            # Get final list of dimensions (after taking slices).
            dims = var.getDimensions()

            # Get the data (centered coordinates and values)
            data = var.getValues(dimbounds,staggered=False)
            
            # Skip this variable if no data are available.
            if data==None or 0 in data[-1].shape: continue

            # Now we are at the point where getting the data worked.
            # Register all used dimensions (even the "sliced out" ones)
            # as used, and get information on them.
            for dimname in originaldims:
                dimdata = dim2data.setdefault(dimname,{'forcedrange':[None,None]})
                dimdata['used'] = True
                diminfo = var.getDimensionInfo(dimname)
                dimdata.update(diminfo)

            # Add the variable itself to the dimension list.
            dimdata = dim2data.setdefault(varpath,{'forcedrange':[None,None]})
            dimdata.update({'label':var.getLongName(),'unit':var.getUnit(),'datatype':'float','logscale':False})
            
            # Find non-singleton dimensions (singleton dimension: dimension with length one)
            # Store singleton dimensions as fixed extra coordinates.
            gooddimindices = []
            gooddimnames = []
            fixedcoords = []
            for idim,dimname in enumerate(dims):
                if data[-1].shape[idim]>1:
                    # Normal dimension (more than one coordinate)
                    gooddimindices.append(idim)
                    gooddimnames.append(dimname)
                elif data[-1].shape[idim]==1:
                    # Singleton dimension
                    fixedcoords.append((dimname,data[idim][0]))
                    
            dims = gooddimnames
            values = data[-1].squeeze()

            # Get effective number of independent dimensions (singleton dimensions removed)
            dimcount = len(dims)
            defaultseriesnode.getLocation(['DimensionCount']).setValue(dimcount)

            # Get the plot type, based on the number of dimensions
            if dimcount==0:
                plottypenodename = 'PlotType2D'
            elif dimcount==1:
                plottypenodename = 'PlotType2D'
            elif dimcount==2:
                plottypenodename = 'PlotType3D'
            else:
                raise Exception('This variable has %i independent dimensions. Can only plot variables with 0, 1 or 2 independent dimensions.' % dimcount)
            plottype = seriesnode.getLocation([plottypenodename]).getValueOrDefault()

            # We use a staggered grid (coordinates at interfaces,
            # values at centers) for certain 3D plot types.
            staggered = (plottypenodename=='PlotType3D' and plottype==0)

            # Get coordinate data (now that we know whether to use a staggered grid)
            data = var.getValues(dimbounds,staggered=staggered,coordinatesonly=True)
            data = [data[idim].squeeze() for idim in gooddimindices] + [values]

            # Get the minimum and maximum values; store these as default.
            dim2data[varpath]['datarange'] = [data[-1].min(),data[-1].max()]

            # Mask values that are not within (minimum, maximum) range.
            #minimum = seriesnode.getLocation(['Minimum']).getValue()
            #maximum = seriesnode.getLocation(['Maximum']).getValue()
            #if minimum!=None and maximum!=None:
            #    data[-1] = matplotlib.numerix.ma.masked_array(data[-1],matplotlib.numerix.logical_or(data[-1]<minimum, data[-1]>maximum))
            #elif minimum!=None:
            #    data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<minimum)
            #elif maximum!=None:
            #    data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]>maximum)

            # Transform to log-scale if needed (first mask values <= zero)
            logscale = seriesnode.getLocation(['LogScale']).getValueOrDefault()
            if logscale:
                #data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<=0.)
                #data[-1] = matplotlib.numerix.ma.log10(data[-1])
                dim2data[varpath]['logscale'] = True

            # Get label
            #defaultlabel = '%s (%s)' % (var.getLongName(),var.getUnit())
            #if logscale: defaultlabel = 'log10 '+defaultlabel
            #defaultseriesnode.getLocation(['Label']).setValue(defaultlabel)
            #label = seriesnode.getLocation(['Label']).getValueOrDefault()

            # Enumerate over the dimension of the variable.
            for idim,dimname in enumerate(dims):
                # Get minimum and maximum coordinates.
                if data[idim].ndim==1:
                    # Coordinates provided as vector (1D) valid over whole domain.
                    datamin = data[idim][0]
                    datamax = data[idim][-1]
                else:
                    # Coordinates are provided as multidimensional array, with a value for every
                    # coordinate (data point) in the domain. We assume that for a given point
                    # in the space of the other coordinates, the current cordinate increases
                    # monotonously (i.e., position 0 holds the lowest value and position -1 the
                    # highest)
                    datamin = data[idim].take((0, ),idim).min()
                    datamax = data[idim].take((-1,),idim).max()

                # Update effective dimension bounds                    
                effrange = dim2data[dimname].setdefault('datarange',[None,None])
                if effrange[0]==None or datamin<effrange[0]: effrange[0] = datamin
                if effrange[1]==None or datamax>effrange[1]: effrange[1] = datamax
            
            # Plot the data series
            if len(dims)==0:
                # Zero-dimensional coordinate space (i.e., only a single data value is available)
                # No plotting of coordinate-less data (yet)
                pass
            if len(dims)==1:
                # One-dimensional coordinate space (x). Use x-axis for coordinates, unless the
                # dimension information states it is preferably uses the y-axis.
                xdim = 0
                datadim = 1
                if (dim2data[dims[0]]['preferredaxis']=='y'):
                    # Independent dimension prefers to take y-axis.
                    xdim = 1
                    datadim = 0
                style = ''
                if plottype!=0:
                    markertype = seriesnode.getLocation(['MarkerType']).getValueOrDefault()
                    markertypes = {0:'.',1:',',2:'o',3:'^',4:'s',5:'+',6:'x',7:'D'}
                    style+=markertypes[markertype]
                if plottype!=1: style+='-'
                linewidth = seriesnode.getLocation(['LineWidth']).getValueOrDefault()
                color = seriesnode.getLocation(['Color']).getValueOrDefault()
                markersize = seriesnode.getLocation(['MarkerSize']).getValueOrDefault()
                markerfacecolor = seriesnode.getLocation(['MarkerFaceColor']).getValueOrDefault()
                lines = axes.plot(data[xdim],data[datadim],style,linewidth=linewidth,color=color.getNormalized(),markersize=markersize,markerfacecolor=markerfacecolor.getNormalized())
                if xdim==0:
                    dim2data[dims[0]]['axis'] = 'x'
                    dim2data[varpath]['axis'] = 'y'
                else:
                    dim2data[varpath]['axis'] = 'x'
                    dim2data[dims[0]]['axis'] = 'y'
            elif len(dims)==2:
                # Two-dimensional coordinate space (x,y). Use x-axis for first coordinate dimension,
                # and y-axis for second coordinate dimension.
                xdim = 0
                ydim = 1
                prefaxis = [dim2data[dims[0]]['preferredaxis'],dim2data[dims[1]]['preferredaxis']]
                if (prefaxis[0]=='y' and prefaxis[1]!='y') or (prefaxis[1]=='x' and prefaxis[0]!='x'):
                    # One independent dimension prefers to switch axis and the
                    # other does not disagree.
                    xdim = 1
                    ydim = 0

                dim2data[dims[xdim]]['axis'] = 'x'
                dim2data[dims[ydim]]['axis'] = 'y'
                dim2data[varpath]   ['axis'] = 'colorbar'

                X = data[xdim]
                Y = data[ydim]
                Z = data[-1]
                
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

                if plottype==1:
                    loc = None
                    if logscale: loc = matplotlib.ticker.LogLocator()

                    cc = seriesnode.getLocation(['ContourCount']).getValue()
                    if cc!=None:
                        pc = axes.contourf(X,Y,Z,cc,norm=norm,locator=loc)
                    else:
                        pc = axes.contourf(X,Y,Z,norm=norm,locator=loc)
                    if cc==None:
                      defaultseriesnode.getLocation(['ContourCount']).setValue(len(pc.levels)-2)
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

            # Hold all plot properties so we can plot additional data series.
            axes.hold(True)

        # Remove unused default series
        # (remaining from previous plots that had these other data series)
        for oldname in olddefaults:
            defaultdatanode.removeChild('Series',oldname)

        # Create and store title
        self.defaultproperties.setProperty(['Title'],', '.join(longnames))
        title = self.properties.getProperty(['Title'],usedefault=True)
        assert title!=None, 'Title must be available, either explicitly set or as default.'
        if title!='': axes.set_title(title,size=fontsizes['axes.titlesize'],fontname=fontfamily)

        # Build table linking axis to data dimension.
        axis2dim = dict([(dat['axis'],dim) for dim,dat in dim2data.iteritems() if 'axis' in dat])

        # Get effective ranges for each dimension (based on forced limits and natural data ranges)
        for axisname in ('x','y','z','colorbar'):
            if axisname not in axis2dim: continue
            
            dim = axis2dim[axisname]
            dat = dim2data[dim]
            
            # Get the effective range of the current dimension
            valmin,valmax = dat['datarange'][:]
            forcedrange = dat.get('forcedrange',(None,None))
            if forcedrange[0]!=None: valmin = forcedrange[0]
            if forcedrange[1]!=None: valmax = forcedrange[1]
            
            # Get the explicitly set and the default properties.
            axisnode = forcedaxes.getChildById('Axis',dim,create=True)
            defaxisnode = defaultaxes.getChildById('Axis',dim,create=True)

            # Build default label for this axis
            deflab = dat['label']
            if dat['unit']!='': deflab += ' ('+dat['unit']+')'
            defaxisnode.getLocation(['Label']).setValue(deflab)
            defaxisnode.getLocation(['Unit']).setValue(dat['unit'])

            istimeaxis = dat['datatype']=='datetime'
            defaxisnode.getLocation(['IsTimeAxis']).setValue(istimeaxis)

            if istimeaxis:
                assert axisname!='colorbar', 'The color bar cannot be a time axis.'
                
                # Get the MatPlotLib axis object.
                if axisname=='x':
                    mplaxis = axes.get_xaxis()
                else:
                    mplaxis = axes.get_yaxis()
                    
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
                
                # Select tick type and spacing based on the time span to show.
                dayspan = (valmax-valmin)
                unitlengths = {0:365,1:30.5,2:1.,3:1/24.,4:1/1440.}
                if dayspan/365>=2:
                    location = 0
                    tickformat = 10
                elif dayspan>=61:
                    location = 1
                    tickformat = 4
                elif dayspan>=2:
                    location = 2
                    tickformat = 19
                elif 24*dayspan>=2:
                    location = 3
                    tickformat = 15
                else:
                    location = 4
                    tickformat = 15

                defaxisnode.getLocation(['TickLocationTime']).setValue(location)
                defaxisnode.getLocation(['TickFormatTime']).setValue(tickformat)
                location   = axisnode.getLocation(['TickLocationTime']).getValueOrDefault()
                tickformat = axisnode.getLocation(['TickFormatTime']).getValueOrDefault()

                # Calculate optimal interval between ticks, aiming for max. 8 ticks total.
                tickcount = dayspan/unitlengths[location]
                interval = math.ceil(tickcount/8.)
                if interval<1: interval = 1
                
                # Save default tick interval, then get effective tick interval.
                defaxisnode.getLocation(['TickIntervalTime']).setValue(interval)
                interval = axisnode.getLocation(['TickIntervalTime']).getValueOrDefault()

                # Make sure we do not plot more than 100 ticks: non-informative and very slow!
                tickcount = dayspan/unitlengths[location]
                if tickcount/interval>100: interval=math.ceil(tickcount/100.)

                if location==0:
                    mplaxis.set_major_locator(matplotlib.dates.YearLocator(base=interval))
                elif location==1:
                    mplaxis.set_major_locator(matplotlib.dates.MonthLocator(interval=interval))
                elif location==2:
                    mplaxis.set_major_locator(matplotlib.dates.DayLocator(interval=interval))
                elif location==3:
                    mplaxis.set_major_locator(matplotlib.dates.HourLocator(interval=interval))
                elif location==4:
                    mplaxis.set_major_locator(matplotlib.dates.MinuteLocator(interval=interval))
                else:
                    assert False, 'unknown tick location %i' % location

                # Add tick format
                assert tickformat in tickformats, 'Unknown tick format %i.' % tickformat
                mplaxis.set_major_formatter(CustomDateFormatter(tickformats[tickformat]))

                # Set the "natural" axis limits based on the data ranges.
                defaxisnode.getLocation(['MinimumTime']).setValue(common.num2date(dat['datarange'][0]))
                defaxisnode.getLocation(['MaximumTime']).setValue(common.num2date(dat['datarange'][1]))
            else:
                # Set the "natural" axis limits based on the data ranges.
                defaxisnode.getLocation(['Minimum']).setValue(dat['datarange'][0])
                defaxisnode.getLocation(['Maximum']).setValue(dat['datarange'][1])

            # Obtain label for axis.
            label = axisnode.getLocation(['Label']).getValueOrDefault()
            if label==None: label=''

            # Set axis labels and boundaries.
            if axisname=='x':
                if label!='': axes.set_xlabel(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
                axes.set_xlim(valmin,valmax)
                if dat['logscale']: axes.set_xscale('log')
            elif axisname=='y':
                if label!='': axes.set_ylabel(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
                axes.set_ylim(valmin,valmax)
                if dat['logscale']: axes.set_yscale('log')
            elif axisname=='colorbar':
                assert cb!=None, 'No colorbar has been created.'
                if label!='': cb.set_label(label,size=fontsizes['axes.labelsize'],fontname=fontfamily)
        
        # Scale the text labels
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
