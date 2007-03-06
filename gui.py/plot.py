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

        # Create empty set of properties (these will combine the 'forced' properties, and the automatically
        # chosen defaults for properties that were not explicitly set).
        self.properties = xmlstore.TypedStore('figuretemplate.xml',None)
        self.propertiesinterface = self.properties.getInterface()
        self.propertiesinterface.addBeforeChangeHandler(self.onBeforeMergedPropertyChange)
        
        # Create store for the explicitly set properties
        self.forcedproperties = xmlstore.TypedStore('figuretemplate.xml',properties)
        self.forcedpropertiesinterface = self.forcedproperties.getInterface()
        self.forcedpropertiesinterface.addChangeHandler(self.onExplicitPropertyChanged)

        self.sources = {}
        self.defaultsource = None
        self.updating = True
        self.dirty = False
        self.haschanged = False

        self.ignorechanges = False

    def setUpdating(self,allowupdates):
        if self.updating != allowupdates:
            self.updating = allowupdates
            if allowupdates and self.dirty: self.update()

    def onBeforeMergedPropertyChange(self,node,value):
        # Check if th user changed it (self.ignorechanges=False), or we are changing it (self.ignorechanges=True).
        # In the latter case no need to redirect the change: just approve.
        if self.ignorechanges: return True
        
        # The user tried to modify a figure property; redirect this to the store
        # for explicitly set properties.
        self.forcedproperties.setProperty(node.location,value)

        # Do not allow the change of the merged property store (the changed of
        # the explicit-property-store will force a refresh of the merged properties
        # indirectly).
        return False

    def onExplicitPropertyChanged(self,node):
        self.haschanged = True
        self.update()

    def clearSources(self):
        self.sources = {}
        self.defaultsource = None

    def addDataSource(self,name,obj):
        self.sources[name] = obj
        if self.defaultsource==None: self.defaultsource = name

    def clearProperties(self):
        self.forcedproperties.setStore(None)
        self.update()

    def setProperties(self,props):
        self.forcedproperties.setStore(props)
        self.update()

    def getPropertiesRoot(self):
        return self.forcedproperties.store.xmlroot

    def getPropertiesCopy(self):
        return common.copyNode(self.forcedproperties.store.xmlroot,None)

    def clearVariables(self):
        self.forcedproperties.root.getLocation(['Data']).removeChildren('Series')
        self.update()

    def addVariable(self,varname,source=None):
        datanode = self.forcedproperties.root.getLocation(['Data'])
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
        tmin = self.forcedproperties.getProperty(['TimeAxis','Minimum'])
        tmax = self.forcedproperties.getProperty(['TimeAxis','Maximum'])
        zmin = self.forcedproperties.getProperty(['DepthAxis','Minimum'])
        zmax = self.forcedproperties.getProperty(['DepthAxis','Maximum'])

        # Variables below will store the effective dimension boundaries
        tmin_eff = None
        tmax_eff = None
        zmin_eff = None
        zmax_eff = None

        # Link between dimension name (e.g., "time", "z") and axis (e.g., "x", "y")
        dim2axis = {}

        # We will now adjust the plot properties; disable use of property-change notifications,
        # as those would otherwise call plot.update again, leading to infinite recursion.
        self.ignorechanges = True

        # Shortcuts to the nodes specifying the variables to plot.
        forceddatanode = self.forcedproperties.root.getLocation(['Data'])
        forcedseries = forceddatanode.getLocationMultiple(['Series'])

        # Shortcut to the node that will hold the variables effectively plotted.
        datanode = self.properties.root.getLocation(['Data'])

        # This variable will hold all long names of the plotted variables; will be used to create plot title.
        longnames = []

        for (iseries,forcedseriesnode) in enumerate(forcedseries):
            # Get the name and data source of the variable to plot.
            varname   = forcedseriesnode.getLocation(['Variable']).getValue()
            varsource = forcedseriesnode.getLocation(['Source']).getValue()
            if varsource==None:
                # No data source specified; take default.
                assert self.defaultsource!=None, 'No data source set for variable "%s", but no default source available either.' % varname
                varsource = self.defaultsource
                
            # Get variable object.
            var = self.sources[varsource].getVariable(varname)
            assert var!=None, 'Source "%s" does not contain variable with name "%s".' % (varsource,varname)

            # Copy series information
            newseriesnode = datanode.getNumberedChild('Series',iseries)
            newseriesnode.getLocation(['Variable']).setValue(varname)
            if varsource!=None:
                newseriesnode.getLocation(['Source']).setValue(varsource)

            # Store the variable long name (to be used for building title)
            longnames.append(var.getLongName())

            # Get the (number of) independent dimensions of the current variable.
            dims = var.getDimensions()
            newseriesnode.getLocation(['DimensionCount']).setValue(len(dims))

            # Get the plot type, based on the number of dimensions
            if len(dims)==1:
                plottypenodename = 'PlotType2D'
            elif len(dims)==2:
                plottypenodename = 'PlotType3D'
            else:
                raise Exception('This variable has %i independent dimensions. Can only plot variables with 2 or 3 independent dimensions.' % len(dims))
            plottype = forcedseriesnode.getLocation([plottypenodename]).getValue()
            if plottype==None: plottype=0
            newseriesnode.getLocation([plottypenodename]).setValue(plottype)

            staggered = False
            if plottypenodename=='PlotType3D' and plottype==0: staggered = True

            # Set forced bounds for the different dimensions
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

            # Transform to log-scale if needed
            logscale = forcedseriesnode.getLocation(['LogScale']).getValue()
            if logscale==None: logscale = False
            newseriesnode.getLocation(['LogScale']).setValue(logscale)
            if logscale:
                data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<=0)
                data[-1] = matplotlib.numerix.ma.log10(data[-1])

            # get label
            label = forcedseriesnode.getLocation(['Label']).getValue()
            if label==None:
                label = var.getLongName()+' ('+var.getUnit()+')'
                if logscale: label = 'log10 '+label
            newseriesnode.getLocation(['Label']).setValue(label)

            for idim in range(len(dims)):
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

                #print dims[idim]+' '+str(data[idim])
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

                linewidth = forcedseriesnode.getLocation(['LineWidth']).getValue()
                if linewidth==None: linewidth = .5
                newseriesnode.getLocation(['LineWidth']).setValue(linewidth)

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

            iseries += 1

        # Remove unused series (remaining from previous plots that had more data series)
        datanode.removeChildren('Series',first=len(forcedseries))

        #axes.autoscale_view()

        # Create and store title
        title = self.forcedproperties.getProperty(['Title'])
        if title==None: title = ', '.join(longnames)
        if title!='': axes.set_title(title)
        self.properties.setProperty(['Title'],title)

        # Store current axes bounds
        if tmin==None: tmin = tmin_eff
        if tmax==None: tmax = tmax_eff
        if zmin==None: zmin = zmin_eff
        if zmax==None: zmax = zmax_eff
        self.properties.setProperty(['TimeAxis', 'Minimum'],tmin)
        self.properties.setProperty(['TimeAxis', 'Maximum'],tmax)
        self.properties.setProperty(['DepthAxis','Minimum'],zmin)
        self.properties.setProperty(['DepthAxis','Maximum'],zmax)

        # Configure time axis (x-axis), if any.
        if 'time' in dim2axis:
            timeaxis = dim2axis['time']
            
            # Obtain label for time axis.
            tlabel = self.forcedproperties.getProperty(['TimeAxis','Label'])
            if tlabel==None: tlabel = 'time'
            self.properties.setProperty(['TimeAxis', 'Label'],tlabel)

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
            zlabel = self.forcedproperties.getProperty(['DepthAxis','Label'])
            if zlabel==None: zlabel = 'depth (m)'
            self.properties.setProperty(['DepthAxis', 'Label'],zlabel)

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

        # Re-enable property-change notifications; we are done changing plot properties,
        # and want to be notified if anyone else changes them.
        self.ignorechanges = False
        
        self.dirty = False
