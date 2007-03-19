import matplotlib
matplotlib.use('Qt4Agg')
matplotlib.rcParams['numerix'] = 'numpy'
import matplotlib.numerix,matplotlib.numerix.ma
import matplotlib.dates
import matplotlib.pylab
import matplotlib.backends.backend_agg

import common,xmlstore

class MonthFormatter(matplotlib.dates.DateFormatter):
    def __init__(self):
        matplotlib.dates.DateFormatter.__init__(self,'%b')

    def __call__(self, x, pos=None):
        return matplotlib.dates.DateFormatter.__call__(self,x,pos)[0]

class Figure:

    def __init__(self,figure,properties=None):
        self.figure = figure
        self.canvas = figure.canvas

        # Create store for the explicitly set properties
        self.properties = xmlstore.TypedStore('schemas/figure/gotmgui.xml',None)
        self.propertiesinterface = self.properties.getInterface()
        self.propertiesinterface.notifyOnDefaultChange = False
        self.propertiesinterface.addChangeHandler(self.onPropertyChanged)
        self.propertiesinterface.addStoreChangedHandler(self.onPropertyStoreChanged)
        
        # Create store for property defaults
        self.defaultproperties = xmlstore.TypedStore('schemas/figure/gotmgui.xml',properties)

        # Set some default properties.
        self.defaultproperties.setProperty(['TimeAxis',  'Label'],'time')
        self.defaultproperties.setProperty(['DepthAxis', 'Label'],'depth (m)')

        self.properties.setDefaultStore(self.defaultproperties)

        self.sources = {}
        self.defaultsource = None
        self.updating = True
        self.dirty = False
        self.haschanged = False
        
        self.callbacks = {'completeStateChange':[]}
        
    def registerCallback(self,eventname,callback):
        assert eventname in self.callbacks, 'Event "%s" is unknown.' % eventname
        self.callbacks[eventname].append(callback)

    def setUpdating(self,allowupdates):
        if self.updating != allowupdates:
            self.updating = allowupdates
            if allowupdates and self.dirty: self.update()

    def onPropertyChanged(self,node):
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
        series = datanode.addChild('Series')
        series.getLocation(['Variable']).setValue(varname)
        if source!=None:
            series.getLocation(['Source']).setValue(source)
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

        # Get forced axes boundaries (will be None if not set; then we autoscale)
        tmin = self.properties.getProperty(['TimeAxis','Minimum'])
        tmax = self.properties.getProperty(['TimeAxis','Maximum'])
        zmin = self.properties.getProperty(['DepthAxis','Minimum'])
        zmax = self.properties.getProperty(['DepthAxis','Maximum'])

        # Variables below will store the effective dimension boundaries
        tmin_eff = None
        tmax_eff = None
        zmin_eff = None
        zmax_eff = None

        # Link between dimension name (e.g., "time", "z") and axis (e.g., "x", "y")
        dim2axis = {}

        # Shortcuts to the nodes specifying the variables to plot.
        forceddatanode = self.properties.root.getLocation(['Data'])
        forcedseries = forceddatanode.getLocationMultiple(['Series'])

        # Shortcut to the node that will hold defaults for the plotted variables.
        defaultdatanode = self.defaultproperties.root.getLocation(['Data'])

        # This variable will hold all long names of the plotted variables; will be used to create plot title.
        longnames = []

        for (iseries,seriesnode) in enumerate(forcedseries):
            # Get the name and data source of the variable to plot.
            varname   = seriesnode.getLocation(['Variable']).getValue()
            varsource = seriesnode.getLocation(['Source']).getValue()
            if varsource==None:
                # No data source specified; take default.
                assert self.defaultsource!=None, 'No data source set for variable "%s", but no default source available either.' % varname
                varsource = self.defaultsource
                
            # Get variable object.
            var = self.sources[varsource].getVariable(varname)
            assert var!=None, 'Source "%s" does not contain variable with name "%s".' % (varsource,varname)

            # Copy series information
            defaultseriesnode = defaultdatanode.getNumberedChild('Series',iseries,create=True)
            defaultseriesnode.getLocation(['Variable']).setValue(varname)
            defaultseriesnode.getLocation(['Source']).setValue(varsource)
            defaultseriesnode.getLocation(['PlotType2D']).setValue(0)
            defaultseriesnode.getLocation(['PlotType3D']).setValue(0)
            defaultseriesnode.getLocation(['LineWidth']).setValue(0.5)
            defaultseriesnode.getLocation(['LogScale']).setValue(False)

            # Store the variable long name (to be used for building title)
            longnames.append(var.getLongName())

            # Get the (number of) independent dimensions of the current variable.
            dims = var.getDimensions()
            seriesnode.getLocation(['DimensionCount']).setValue(len(dims))

            # Get the plot type, based on the number of dimensions
            if len(dims)==1:
                plottypenodename = 'PlotType2D'
            elif len(dims)==2:
                plottypenodename = 'PlotType3D'
            else:
                raise Exception('This variable has %i independent dimensions. Can only plot variables with 2 or 3 independent dimensions.' % len(dims))
            plottype = seriesnode.getLocation([plottypenodename]).getValueOrDefault()

            # We use a staggered grid (coordinates at interfaces, values at centers) for certain 3D plot types.
            staggered = (plottypenodename=='PlotType3D' and plottype==0)

            # Set forced bounds (if any) for each dimension of the plotted variable.
            dimbounds = []
            for dimname in dims:
                if dimname=='time':
                    dimbounds.append((tmin,tmax))
                elif dimname=='z':
                    dimbounds.append((zmin,zmax))
                else:
                    raise Exception('Variable has unknown dimension "'+dimname+'".')

            # Get the data
            data = var.getValues(dimbounds,staggered=staggered)

            # Get the minimum and maximum values; store these as default.
            defaultseriesnode.getLocation(['Minimum']).setValue(data[-1].min())
            defaultseriesnode.getLocation(['Maximum']).setValue(data[-1].max())

            # Filter values that are not within (minimum, maximum) range.
            minimum = seriesnode.getLocation(['Minimum']).getValue()
            maximum = seriesnode.getLocation(['Maximum']).getValue()
            if minimum!=None and maximum!=None:
                data[-1] = matplotlib.numerix.ma.masked_array(data[-1],matplotlib.numerix.logical_or(data[-1]<minimum, data[-1]>maximum))
            elif minimum!=None:
                data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<minimum)
            elif maximum!=None:
                data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]>maximum)

            # Transform to log-scale if needed
            logscale = seriesnode.getLocation(['LogScale']).getValueOrDefault()
            if logscale:
                data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<=0)
                data[-1] = matplotlib.numerix.ma.log10(data[-1])

            # Get label
            defaultlabel = '%s (%s)' % (var.getLongName(),var.getUnit())
            if logscale: defaultlabel = 'log10 '+defaultlabel
            defaultseriesnode.getLocation(['Label']).setValue(defaultlabel)
            label = seriesnode.getLocation(['Label']).getValueOrDefault()

            # Enumerate over the dimension of the variable.
            for idim in range(len(dims)):
                # Get minimum and maximum coordinates.
                if len(data[idim].shape)==1:
                    datamin = data[idim][0]
                    datamax = data[idim][-1]
                else:
                    if idim==0:
                        datamin = min(data[idim][0,:])
                        datamax = max(data[idim][-1,:])
                    else:
                        datamin = min(data[idim][:,0])
                        datamax = max(data[idim][:,-1])

                if dims[idim]=='time':
                    # Update effective time bounds
                    if tmin_eff==None or datamin<tmin_eff: tmin_eff=datamin
                    if tmax_eff==None or datamax>tmax_eff: tmax_eff=datamax
                    
                    # Convert time (datetime objects) to time unit used by MatPlotLib
                    data[idim] = matplotlib.dates.date2num(data[idim])
                elif dims[idim]=='z':
                    # Update effective depth bounds
                    if zmin_eff==None or datamin<zmin_eff: zmin_eff=datamin
                    if zmax_eff==None or datamax>zmax_eff: zmax_eff=datamax

            # Plot the data series
            if len(dims)==1:
                # One-dimensional variable; currently this implies dependent on time only.
                xdim = 0

                linewidth = seriesnode.getLocation(['LineWidth']).getValueOrDefault()

                lines = axes.plot(data[xdim],data[-1],'-',linewidth=linewidth)
                dim2axis[dims[xdim]] = 'x'
                axes.set_ylabel(label)
            elif len(dims)==2:
                # Two-dimensional variable, i.e. dependent on time and depth.
                xdim = 0
                ydim = 1

                dim2axis[dims[xdim]] = 'x'
                dim2axis[dims[ydim]] = 'y'

                X = data[xdim]
                Y = data[ydim]
                Z = data[-1]

                # Get length of coordinate dimensions.
                if len(X.shape)==1:
                    xlength = X.shape[0]
                else:
                    xlength = X.shape[xdim]
                if len(Y.shape)==1:
                    ylength = Y.shape[0]
                else:
                    ylength = Y.shape[ydim]
                
                # Adjust X dimension.
                if len(X.shape)==1:
                    X = matplotlib.numerix.reshape(X,(1,-1))
                    X = matplotlib.numerix.repeat(X, ylength, 0)
                elif xdim<ydim:
                    X = matplotlib.numerix.transpose(X)
                    
                # Adjust Y dimension.
                if len(Y.shape)==1:
                    Y = matplotlib.numerix.reshape(Y,(-1,1))
                    Y = matplotlib.numerix.repeat(Y, xlength, 1)
                elif xdim<ydim:
                    Y = matplotlib.numerix.transpose(Y)
                    
                # Adjust Z dimension.
                if xdim<ydim:
                    # Note: using masked array transpose because values can be masked (e.g. after log-transform)
                    Z = matplotlib.numerix.ma.transpose(Z)

                if plottype==1:
                  pc = axes.contourf(X,Y,Z)
                else:
                  #pc = axes.pcolor(X,Y,Z,shading='flat', cmap=matplotlib.pylab.cm.jet)
                  pc = axes.pcolormesh(X,Y,Z,shading='flat', cmap=matplotlib.pylab.cm.jet)
                  #im.set_interpolation('bilinear')
                cb = self.figure.colorbar(mappable=pc)
                if label!='': cb.set_label(label)

            # Hold all plot properties so we can plot additional data series.
            axes.hold(True)

        # Remove unused series (remaining from previous plots that had more data series)
        defaultdatanode.removeChildren('Series',first=len(forcedseries))

        # Create and store title
        self.defaultproperties.setProperty(['Title'],', '.join(longnames))
        title = self.properties.getProperty(['Title'],usedefault=True)
        if title!='': axes.set_title(title)

        # Store current axes bounds
        self.defaultproperties.setProperty(['TimeAxis',  'Minimum'],tmin_eff)
        self.defaultproperties.setProperty(['TimeAxis',  'Maximum'],tmax_eff)
        self.defaultproperties.setProperty(['DepthAxis', 'Minimum'],zmin_eff)
        self.defaultproperties.setProperty(['DepthAxis', 'Maximum'],zmax_eff)
        if tmin==None: tmin = tmin_eff
        if tmax==None: tmax = tmax_eff
        if zmin==None: zmin = zmin_eff
        if zmax==None: zmax = zmax_eff

        # Configure time axis (if any).
        if 'time' in dim2axis:
            timeaxis = dim2axis['time']
            
            # Obtain label for time axis.
            tlabel = self.properties.getProperty(['TimeAxis','Label'],usedefault=True)

            # Configure limits and label of time axis.
            if timeaxis=='x':
                taxis = axes.xaxis
                if tlabel!='': axes.set_xlabel(tlabel)
                axes.set_xlim(matplotlib.dates.date2num(tmin),matplotlib.dates.date2num(tmax))
            elif timeaxis=='y':
                taxis = axes.yaxis
                if tlabel!='': axes.set_ylabel(tlabel)
                axes.set_ylim(matplotlib.dates.date2num(tmin),matplotlib.dates.date2num(tmax))

            # Select tick type and spacing based on the time span to show.
            dayspan = (tmax-tmin).days
            if dayspan/365>10:
              # more than 10 years
              taxis.set_major_locator(matplotlib.dates.YearLocator(base=5))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%Y'))
            elif dayspan/365>1:
              # less than 10 but more than 1 year
              taxis.set_major_locator(matplotlib.dates.YearLocator(base=1))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%Y'))
            elif dayspan>61:
              # less than 1 year but more than 2 months
              taxis.set_major_locator(matplotlib.dates.MonthLocator(interval=1))
              taxis.set_major_formatter(MonthFormatter())
            elif dayspan>7:
              # less than 2 months but more than 1 day
              taxis.set_major_locator(matplotlib.dates.DayLocator(interval=15))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%d %b'))
            elif dayspan>1:
              # less than 1 week but more than 1 day
              taxis.set_major_locator(matplotlib.dates.DayLocator(interval=1))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%d %b'))
            else:
              # less than 1 day
              taxis.set_major_locator(matplotlib.dates.HourLocator(interval=1))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%H:%M'))

        # Configure depth axis (y-axis), if any.
        if 'z' in dim2axis:
            zaxis = dim2axis['z']

            # Obtain label for depth axis.
            zlabel = self.properties.getProperty(['DepthAxis','Label'],usedefault=True)

            # Configure limits and label of depth axis.
            if zaxis=='x':
                axes.set_xlim(zmin,zmax)
                if zlabel!='': axes.set_xlabel(zlabel)
            elif zaxis=='y':
                axes.set_ylim(zmin,zmax)
                if zlabel!='': axes.set_ylabel(zlabel)
        self.properties.setProperty(['HasDepthAxis'],'z' in dim2axis)

        # Draw the plot to screen.            
        self.canvas.draw()
        
        for cb in self.callbacks['completeStateChange']: cb(len(forcedseries)>0)

        self.dirty = False
