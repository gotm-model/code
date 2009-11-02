# Import modules from standard Python (>= 2.4) library
import datetime, os.path, sys

# Import third-party modules
from PyQt4 import QtGui,QtCore
import numpy
import matplotlib.figure
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg
from matplotlib.backends.backend_agg import FigureCanvasAgg

# Import our own custom modules
import xmlstore.xmlstore,xmlstore.util,xmlstore.gui_qt4
import plot,data,common

# ====================================================================================
# Utility functions
# ====================================================================================

def getIcon(name):
    """Returns an icon object (QtGui.QIcon) for the specified icon name.
    """
    path = os.path.join(common.getDataRoot(),'icons',name)
    return QtGui.QIcon(path)

def getFontSubstitute(fontname):
    """Returns the name of the actual font that is linked on OS level to the
    supplied font name. This is needed for MS Windows, which maintains a font
    substitution table.
    """
    assert isinstance(fontname,basestring), 'Supplied argument must be a string.'
    
    # Currently font substitution is only supported if the fontname is ASCII,
    # because QueryValueEx does not accept a unicode string as name of the subkey.
    # Not sure if this is only a _winreg issue, or a real Windows limitation...
    try:
        fontname = str(fontname)
    except UnicodeError:
        return fontname

    substitute = fontname

    if sys.platform=='win32':
        # Windows has a font substitution table in the registry, which links
        # "virtual" fonts (e.g. the "dialog box font") to their actual TrueType
        # name. Look up the supplied font name in this table, and return the
        # substitute if present.
        import _winreg
        hkey = None
        try:
            hkey = _winreg.OpenKey(_winreg.HKEY_LOCAL_MACHINE,r'SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes')
            value,datatype = _winreg.QueryValueEx(hkey,fontname)
            substitute = value
        except WindowsError:
            pass
        except EnvironmentError:
            pass
        if hkey is not None:
            _winreg.CloseKey(hkey)
        
    return substitute
    
# ====================================================================================
# Editors of custom data types that may be used in xmlstore.TypedStore objects
# ====================================================================================

class FontNameEditor(xmlstore.gui_qt4.SimpleSelectEditor):
    """Widget for choosing a font, suitable for use in MatPlotLib figures.
    Uses the font manager of MatPlotLib to obtain a list of available fonts.
    """
    def getOptionInfo(self):
        self.fontlist = []
        import matplotlib.font_manager
        fm = matplotlib.font_manager.fontManager
        if hasattr(fm,'ttflist'):
            fontnames = list(set([font.name for font in fm.ttflist]))
        else:
            fontnames = fm.ttfdict.keys()
        return sorted(fontnames,key=str.lower)

xmlstore.gui_qt4.registerEditor('fontname',FontNameEditor)

class MapProjectionEditor(xmlstore.gui_qt4.SimpleSelectEditor):
    """Widget for choosing a basemap map projection.
    Queries basemap to obtain a list of available projections.
    """
    def getOptionInfo(self):
        self.projlist = []
        try:
            import mpl_toolkits.basemap
            return mpl_toolkits.basemap._projnames
        except:
            return ()

xmlstore.gui_qt4.registerEditor('mapprojection',MapProjectionEditor)

class ColorMapEditor(QtGui.QComboBox,xmlstore.gui_qt4.AbstractPropertyEditor):
    """Widget for choosing a colormap, suitable for use in MatPlotLib figures.
    """
    cache = {}
    canvas,figure = None,None
    
    class Model(QtCore.QAbstractListModel):
        def __init__(self,items,parent):
            QtCore.QAbstractListModel.__init__(self,parent)
            self.items = items
            
        def rowCount(self,parent):
            if parent.isValid(): return 0
            return len(self.items)

        def data(self,index,role):
            irow = index.row()
            name = self.items[irow]
            if role==QtCore.Qt.DecorationRole:
                pixmap = ColorMapEditor.getPixMap(name,100.,10.)
                return QtCore.QVariant(QtGui.QIcon(pixmap))
            elif role==QtCore.Qt.DisplayRole:
                return QtCore.QVariant(name)
            else:
                return QtCore.QVariant()

    def __init__(self,parent,node,**kwargs):
        QtGui.QComboBox.__init__(self,parent)
        xmlstore.gui_qt4.AbstractPropertyEditor.__init__(self,parent,node)

        colormaps,self.items = plot.getColorMaps()
        self.model = ColorMapEditor.Model(self.items,self)
        self.setModel(self.model)
        self.view().setUniformItemSizes(True)
        self.connect(self, QtCore.SIGNAL('currentIndexChanged(int)'), self.editingFinished)
        self.setIconSize(QtCore.QSize(100.,10.))
                
    def value(self):
        return self.items[self.currentIndex()]
        
    def setValue(self,value):
        for i in range(self.count()):
            if self.items[i]==value:
                self.setCurrentIndex(i)
                break

    @staticmethod
    def getPixMap(value,width,height):
        qPixMap = ColorMapEditor.cache.get(value,None)
        if qPixMap is None or qPixMap.width()!=width or qPixMap.height()!=height:
            colormaps,cmlist = plot.getColorMaps()
            cm = colormaps[value]
            if ColorMapEditor.figure is None:
                ColorMapEditor.figure = matplotlib.figure.Figure(figsize=(width,height),dpi=1)
                ColorMapEditor.canvas = matplotlib.backends.backend_agg.FigureCanvasAgg(ColorMapEditor.figure)
                ColorMapEditor.figure.subplots_adjust(top=1.,bottom=0.,left=0.,right=1.)
                axes = ColorMapEditor.figure.add_subplot(111)
                axes.axis('off')
            else:
                ColorMapEditor.figure.set_size_inches(width,height)
                axes = ColorMapEditor.figure.gca()
            
            a = numpy.outer(numpy.ones(2),numpy.arange(0,1,1./width))
            axes.imshow(a,aspect='auto',cmap=cm,origin='lower')
            ColorMapEditor.canvas.draw()
            if QtCore.QSysInfo.ByteOrder == QtCore.QSysInfo.LittleEndian:
                stringBuffer = ColorMapEditor.canvas.get_renderer()._renderer.tostring_bgra()
            else:
                stringBuffer = ColorMapEditor.canvas.get_renderer()._renderer.tostring_argb()
            qImage = QtGui.QImage(stringBuffer, width, height, QtGui.QImage.Format_ARGB32)
            qPixMap = QtGui.QPixmap.fromImage(qImage)
            
            # Add border
            p = QtGui.QPainter(qPixMap)
            penwidth = p.pen().width()
            if penwidth==0: penwidth=1
            p.drawRect(QtCore.QRectF(0,0,width-penwidth,height-penwidth))
            
            # Store in cache
            ColorMapEditor.cache[value] = qPixMap
        return qPixMap

    @staticmethod
    def displayValue(delegate,painter,option,index):
        value = ColorMapEditor.convertFromQVariant(index.data(QtCore.Qt.EditRole))
        qPixMap = ColorMapEditor.getPixMap(value,100.,10.)
        option.decorationAlignment = QtCore.Qt.AlignLeft|QtCore.Qt.AlignVCenter
        delegate.drawBackground(painter,option,index)
        xOffset = QtGui.qApp.style().pixelMetric(QtGui.QStyle.PM_FocusFrameHMargin,option)
        delegate.drawDecoration(painter,option,option.rect.adjusted(xOffset,0,0,0),qPixMap)
        delegate.drawFocus(painter,option,option.rect)

    @staticmethod
    def convertFromQVariant(value):
        return xmlstore.gui_qt4.StringEditor.convertFromQVariant(value)

    @staticmethod
    def convertToQVariant(value):
        return xmlstore.gui_qt4.StringEditor.convertToQVariant(value)

xmlstore.gui_qt4.registerEditor('colormap',ColorMapEditor)

class LinkedFileEditor(QtGui.QWidget,xmlstore.gui_qt4.AbstractPropertyEditor):
    """Widget for "editing" a linked file. Currently just displays a button that,
    when clicked, displays a separate dialog.
    """
    def __init__(self,parent,node,fileprefix=None,datasourcedir=None,autoopen=False,**kwargs):
        QtGui.QWidget.__init__(self, parent)

        lo = QtGui.QHBoxLayout()
        
        self.title = node.getText(detail=1,capitalize=True)
        self.linkedfile = None
        self.datasourcedir = datasourcedir
        self.autoopen = autoopen
        
        if autoopen: return

        if fileprefix is None: fileprefix = self.title
        self.plotbutton = QtGui.QPushButton(fileprefix+'...',self)
        lo.addWidget(self.plotbutton)
        #lo.addStretch(1)

        self.setLayout(lo)

        self.connect(self.plotbutton, QtCore.SIGNAL('clicked()'), self.onPlot)
        
    def showEvent(self,ev):
        if self.autoopen: self.onPlot()

    def setValue(self,value):
        if self.linkedfile is not None: self.linkedfile.release()
        self.linkedfile = value.addref()
        
    def value(self):
        return self.linkedfile.addref()

    def onPlot(self):
        dialog = LinkedFileEditorDialog(self.linkedfile,self,title=self.title,datasourcedir=self.datasourcedir)
        ret = dialog.exec_()
        if ret == QtGui.QDialog.Accepted:
            self.linkedfile = dialog.linkedfile
            self.editingFinished(forceclose=True)
        dialog.destroy()
            
    def destroy(self):
        if self.linkedfile is not None: self.linkedfile.release()
        QtGui.QWidget.destroy(self)

xmlstore.gui_qt4.registerEditor('gotmdatafile',LinkedFileEditor)

# ====================================================================================
# Figure classes (toolbar, canvas, panel, dialog)
# ====================================================================================

class FigureToolbar(matplotlib.backend_bases.NavigationToolbar2):
    """Class derived from MatPlotLib NavigationToolbar2, used only for its
    zooming code: no toolbar its created/shown/used in any way!.
    
    Code for some methods (rectange drawing by draw_rubberband, cursor selection
    by set_cursor) has been taken from the NavigationToolbar2QT from backend_qt4.
    
    mouse_move has been copied from the base implementation (NavigationToolbar2),
    and changed such that selection rectangle cannot extent beyond the axes
    rectangle. Dangerous! Every time MatPlotLib changes this might require an
    update...
    
    The draw method that would normally force a canvas redraw has been
    reimplemented to call a callback specified at initialization. Thus the axes
    changes can be caught, and reflected in the XML-based plot properties.
    """
        
    def __init__( self, canvas, callback=None):
        matplotlib.backend_bases.NavigationToolbar2.__init__( self, canvas )
        self.callback = callback
        
    def _init_toolbar( self ):
        pass

    def dynamic_update( self ):
        self.canvas.draw()

    def set_cursor( self, cursor ):
        """Called by the base implementation to change the mouse cursor.
        The code has been taken from NavigationToolbar2QT.
        """
        cursord = {
            matplotlib.backend_bases.cursors.MOVE          : QtCore.Qt.PointingHandCursor,
            matplotlib.backend_bases.cursors.HAND          : QtCore.Qt.WaitCursor,
            matplotlib.backend_bases.cursors.POINTER       : QtCore.Qt.ArrowCursor,
            matplotlib.backend_bases.cursors.SELECT_REGION : QtCore.Qt.CrossCursor,
            }
        self.canvas.setCursor(QtGui.QCursor(cursord[cursor]))
                
    def draw_rubberband( self, event, x0, y0, x1, y1 ):
        """Called by the base implementation to draw the zooming rectangle.
        The code has been taken from NavigationToolbar2QT.
        """
        if callable(self.canvas.figure.bbox.height):
            # MatPlotLib <= 0.91.2
            height = self.canvas.figure.bbox.height()
        else:
            # MatPlotLib > 0.91.2
            height = self.canvas.figure.bbox.height
            
        y1 = height - y1
        y0 = height - y0
        
        w = abs(x1 - x0)
        h = abs(y1 - y0)

        rect = [ int(val)for val in min(x0,x1), min(y0, y1), w, h ]
        self.canvas.drawRectangle( rect )

    def draw(self):
        """Called by the base implementation (NavigationToolbar2) when axes
        boundaries change because of zooming/panning. This would force a
        canvas redraw in the base implementation, but has been reimplemented
        here to call a user defined callback instead. This allows the host
        to catch the changed figure boundaries and process it at a higher
        level, before returning control to MatPlotLib.
        """
        if self.callback is not None: self.callback()

    def mouse_move(self, event):
        """Called by the backend when the mouse cursor moves.
        The code has been taken from the base implementation (NavigationToolbar2),
        and adapted to respect the bounds of the axes rectangle.
        """

        if self._active=='ZOOM' and self._xypress:
            x, y = event.x, event.y
            lastx, lasty, a, ind, lim, trans= self._xypress[0]
            
            # The bit below is the only change from the base implementation.
            # it guarantees that the selection rectangle cannot extend outside
            # the axes rectangle.
            bb = a.bbox
            if not bb.contains(x,y):
                if callable(bb.xmin):
                    # MatPlotLib <= 0.91.2
                    xmin,xmax,ymin,ymax = bb.xmin(),bb.xmax(),bb.ymin(),bb.ymax()
                else:
                    # MatPlotLib > 0.91.2
                    xmin,xmax,ymin,ymax = bb.xmin,  bb.xmax,  bb.ymin,  bb.ymax
                if   x<xmin: x = xmin
                elif x>xmax: x = xmax
                if   y<ymin: y = ymin
                elif y>ymax: y = ymax
                event = matplotlib.backend_bases.MouseEvent(event.name, event.canvas, x, y,
                                   event.button, event.key,
                                   guiEvent=event.guiEvent)
        return matplotlib.backend_bases.NavigationToolbar2.mouse_move(self,event)

class FigureCanvas(FigureCanvasQTAgg):
    """Canvas for Qt4, inheriting from FigureCanvasQTAgg. This class exposes the
    canvas resize event to external subscribers. This is used to automatically
    update the figure size when the user resize (the container of) the canvas.
    """
    def __init__(self, figure):
        FigureCanvasQTAgg.__init__(self, figure)
        self.setAttribute(QtCore.Qt.WA_OpaquePaintEvent,False)

    def resizeEvent( self, e ):
        FigureCanvasQTAgg.resizeEvent( self, e )
        self.emit(QtCore.SIGNAL('afterResize()'))

    def draw(self):
        self.replot = True
        self.get_renderer().clear()
        self.update()

class FigurePanel(QtGui.QWidget):
    """This widget contains a MatPlotLib canvas that hosts a figure, plus a toolbar
    that allows for figure zooming, panning, printing, exporting, etc.
    """
    
    def __init__(self,parent,detachbutton=True):
        QtGui.QWidget.__init__(self,parent)

        # Create MatPlotLib figure with transparent background and no border.
        self.mplfigure = matplotlib.figure.Figure(facecolor='none',frameon=False,dpi=self.logicalDpiX())

        # Create MatPlotLib canvas (Qt-backend) attached to our MatPlotLib figure.
        self.canvas = FigureCanvas(self.mplfigure)
        self.canvas.setSizePolicy(QtGui.QSizePolicy.Expanding,QtGui.QSizePolicy.Expanding)
        self.canvas.setMinimumSize(300,250)
        self.connect(self.canvas, QtCore.SIGNAL('afterResize()'), self.afterCanvasResize)

        # Create our figure that encapsulates MatPlotLib figure.
        deffont = getFontSubstitute(unicode(self.fontInfo().family()))
        self.figure = plot.Figure(self.mplfigure,defaultfont=deffont)
        self.figure.registerCallback('completeStateChange',self.onFigureStateChanged)

        # Make sure we are notified when figure properties change
        # (used to detect when the "Reset view" button should be enabled)
        self.propertiesinterface = self.figure.properties.getInterface()
        self.propertiesinterface.connect('afterChange',     self.onFigurePropertyChanged)
        self.propertiesinterface.connect('afterStoreChange',self.onFigurePropertyStoreChanged)
        
        self.navtoolbar = FigureToolbar(self.canvas,self.updateAxesBounds)

        self.factory = xmlstore.gui_qt4.PropertyEditorFactory(self.figure.properties,live=True,allowhide=True)

        layout = QtGui.QVBoxLayout()
        try:
            # Try-except because versions of Qt4 < 4.3 did not support this attribute
            layout.setContentsMargins(0,0,0,0)
        except AttributeError:
            pass
        
        self.errortext = QtGui.QLabel(self)
        self.errortext.setVisible(False)
        self.errortext.setAlignment(QtCore.Qt.AlignTop)
        self.errortext.setWordWrap(True)
        
        self.toolbar = QtGui.QToolBar(self)
        self.toolbar.setToolButtonStyle(QtCore.Qt.ToolButtonTextBesideIcon)
        self.toolbar.addAction(getIcon('configure.png'),'Properties...',self.onAdvancedClicked)
        self.toolbar.addSeparator()
        self.actZoom = self.toolbar.addAction(getIcon('viewmag.png'),'Zoom',self.onZoomClicked)
        self.actPan = self.toolbar.addAction('Pan',self.onPanClicked)
        self.actResetView = self.toolbar.addAction(getIcon('viewmagfit.png'),'Reset view',self.onResetViewClicked)
        self.toolbar.addSeparator()
        self.toolbar.addAction(getIcon('filesaveas.png'),'Save as',self.onExport)
        self.toolbar.addAction(getIcon('fileprint.png'),'Print',self.onPrint)
        if detachbutton:
            self.toolbar.addSeparator()
            self.toolbar.addAction(getIcon('new_window.png'),'Detach figure',self.onDetach)

        self.actZoom.setCheckable(True)
        self.actPan.setCheckable(True)

        layout.addWidget(self.errortext)
        layout.addWidget(self.canvas)
        layout.addWidget(self.toolbar)

        self.setLayout(layout)

        self.dialogAdvanced = None

        # Initially disable all controls; we have no plot to configure yet...
        self.onFigureStateChanged(False)

        self.detachedfigures = []
        self.blockevents = False
        
    def hideEvent(self,event):
        if self.dialogAdvanced is not None: self.dialogAdvanced.hide()
        
    def afterCanvasResize(self):
        w,h = self.canvas.figure.get_size_inches()
        self.blockevents = True
        self.figure.defaultproperties['Width'].setValue(w*2.54)
        self.figure.defaultproperties['Height'].setValue(h*2.54)
        self.figure.onAspectChange()
        self.blockevents = False
        
    def onFigureStateChanged(self,complete):
        """Called when the figure state (figure shown/no figure shown) changes.
        """
        self.errortext.setVisible(not complete)
        if self.figure.errors:
            self.errortext.setText('\n'.join(self.figure.errors))
        else:
            self.errortext.setText('No data to plot.')
        self.canvas.setVisible(complete)
        self.toolbar.setVisible(complete)
        #self.setEnabled(complete)

    def onFigurePropertyStoreChanged(self):
        """Called when all customized figure properties change at once
        (the data store is changed). Currently used to enable/disable
        the "Reset view" button.
        """
        self.onAxesRangeChanged()
        self.updateWidthFromProperties()
        self.updateHeightFromProperties()
        
    def onFigurePropertyChanged(self,node,feature):
        """Called when one customized figure property changes.
        Currently used to enable/disable the "Reset view" button.
        """
        if self.blockevents: return
        self.blockevents = True
        if feature=='value':
            if node is self.figure['Width']:
                self.updateWidthFromProperties()
            elif node is self.figure['Height']:
                self.updateHeightFromProperties()
            self.onAxesRangeChanged()
        self.blockevents = False
        
    def onAxesRangeChanged(self):
        """Enables/disables the "Reset View" button, based on whether the
        current axes bounds have been customized by the user.
        """
        defaultrange = True
        axes = self.figure['Axes']
        if axes is not None:
            xaxis = axes.getChildById('Axis','x')
            yaxis = axes.getChildById('Axis','y')
            for axis in (xaxis,yaxis):
                if axis is None: continue
                if axis['IsTimeAxis'].getValue(usedefault=True):
                    defaultrange = (defaultrange and axis['MinimumTime'].hasDefaultValue() and axis['MaximumTime'].hasDefaultValue())
                else:
                    defaultrange = (defaultrange and axis['Minimum'].hasDefaultValue() and axis['Maximum'].hasDefaultValue())
        self.actResetView.setEnabled(not defaultrange)
        self.actPan.setEnabled(not defaultrange)

    def updateWidthFromProperties(self):
        """Adjusts the canvas/figure width on screen based on the width set in the figure properties store. 
        """
        w = self.figure['Width'].getValue()
        dpi = float(self.logicalDpiX())
        if w is not None:
            self.canvas.setMinimumWidth(w/2.54*dpi)
            self.canvas.setMaximumWidth(w/2.54*dpi)
        else:
            self.canvas.setMinimumWidth(300)
            self.canvas.setMaximumWidth(16777215)
            self.figure.defaultproperties['Width'].setValue(self.canvas.width()/dpi*2.54)

    def updateHeightFromProperties(self):
        """Adjusts the canvas/figure height on screen based on the height set in the figure properties store. 
        """
        h = self.figure['Height'].getValue()
        dpi = float(self.logicalDpiX())
        if h is not None:
            self.canvas.setMinimumHeight(h/2.54*dpi)
            self.canvas.setMaximumHeight(h/2.54*dpi)
        else:
            self.canvas.setMinimumHeight(250)
            self.canvas.setMaximumHeight(16777215)
            self.figure.defaultproperties['Height'].setValue(self.canvas.height()/dpi*2.54)

    def plot(self,varname,varstore=None):
        ownupdating = self.figure.updating
        if ownupdating: self.figure.setUpdating(False)
        self.figure.clearVariables()
        self.figure.clearProperties(deleteoptional=False)
        if isinstance(varstore,basestring) or varstore is None:
            self.figure.addVariable(varname,source=varstore)
        else:
            self.figure.clearSources()
            self.figure.addDataSource('main',varstore)
            self.figure.addVariable(varname)
        if ownupdating: self.figure.setUpdating(True)
        self.figure.resetChanged()

    def plotFromProperties(self,properties):
        self.figure.setProperties(properties)    

    def clear(self):
        """Clears the figure, by disconnecting its links to data stores
        (if any) and erasing any data series.
        """
        self.figure.clearProperties()
        self.figure.clearSources()

    def closeDetached(self):
        """Closes all detached figures.
        """
        for ch in self.detachedfigures:
            self.disconnect(ch, QtCore.SIGNAL('beforeDestroy'), self.beforeDetachedDestroy)
            ch.close()

    def onAdvancedClicked(self):
        """Called when the user clicks the "Properties..." button.
        Currently this shows the figure properties dialog box.
        """
        if self.dialogAdvanced is None:
            self.dialogAdvanced = xmlstore.gui_qt4.PropertyEditorDialog(self,self.figure.properties,title='Figure properties',loadsave=True,flags=QtCore.Qt.Tool,loadhook=self.loadProperties)
            self.dialogAdvanced.resize(350, 300)
            self.dialogAdvanced.resizeColumns()
        self.dialogAdvanced.show()
        self.dialogAdvanced.activateWindow()
        
    def loadProperties(self,path):
        oldroot = self.figure.properties.root
        data = plot.FigureProperties()
        data.load(path)
        newroot = data.root
        oldupdating = self.figure.setUpdating(False)
        for child in oldroot.children:
            id = child.getId()
            newchild = newroot[id]
            if id=='Data':
                for grandchild,newgrandchild in zip(child.children,newchild.children):
                    grandchild.copyFrom(newgrandchild)
            else:
                child.copyFrom(newchild)
        data.unlink()
        self.figure.setUpdating(oldupdating)
        
    def onZoomClicked(self,*args):
        """Called when the user clicks the "Zoom" button.
        """
        self.navtoolbar.zoom( self, *args )

    def onPanClicked(self,*args):
        """Called when the user clicks the "Zoom" button.
        """
        self.navtoolbar.pan( self, *args )

    def updateAxesBounds(self):
        """Called by the attached FigureToolbar object just after the
        bounds of the figure axes have been changed with the zoom
        functionality, but [supposedly] before the MatPlotLib figue has
        been redrawn.
        
        The new axes bounds are taken from the MatPltoLib figure, and
        used to change the explicit axes bounds in our attached Figure.
        This implicitly forces a redraw of the figure.
        """
        a = self.canvas.figure.gca()
        Xmin,Xmax=a.get_xlim()
        Ymin,Ymax=a.get_ylim()
        axes = self.figure['Axes']
        xaxis = axes.getChildById('Axis','x')
        yaxis = axes.getChildById('Axis','y')
        oldupdating = self.figure.setUpdating(False)
        for axis,minval,maxval in ((xaxis,Xmin,Xmax),(yaxis,Ymin,Ymax)):
            if axis['IsTimeAxis'].getValue(usedefault=True):
                axis['MinimumTime'].setValue(common.num2date(minval))
                axis['MaximumTime'].setValue(common.num2date(maxval))
            else:
                axis['Minimum'].setValue(minval)
                axis['Maximum'].setValue(maxval)
        self.figure.setUpdating(oldupdating)
        
        # We do not want the zoom function to stay active after the
        # zooming is done (although that is matPlotLib's default behavior)
        # Pretend the user clicks the zoom button again to disable zooming.
        if self.actZoom.isChecked(): self.actZoom.trigger()

    def onResetViewClicked(self,*args):
        """Called when the user clicks the "Reset view" button.
        """
        if self.actZoom.isChecked(): self.actZoom.trigger()
        if self.actPan.isChecked(): self.actPan.trigger()
        axes = self.figure['Axes']
        xaxis = axes.getChildByNumber('Axis',0)
        yaxis = axes.getChildByNumber('Axis',1)
        oldupdating = self.figure.setUpdating(False)
        for axis in (xaxis,yaxis):
            axis['MinimumTime'].clearValue()
            axis['MaximumTime'].clearValue()
            axis['Minimum'].clearValue()
            axis['Maximum'].clearValue()
        self.figure.setUpdating(oldupdating)

    def onExport(self):
        """Called when the user clicks the "Export to file..." button.
        """
        
        class ExportSettings(QtGui.QDialog):
            def __init__(self,parent=None):
                QtGui.QDialog.__init__(self,parent,QtCore.Qt.Dialog | QtCore.Qt.MSWindowsFixedSizeDialogHint | QtCore.Qt.WindowTitleHint | QtCore.Qt.WindowSystemMenuHint)
                
                layout = QtGui.QGridLayout()
                
                labWidth = QtGui.QLabel('Width:',self)
                labHeight = QtGui.QLabel('Height:',self)
                labResolution = QtGui.QLabel('Resolution:',self)
                layout.addWidget(labWidth,     0,0)
                layout.addWidget(labHeight,    1,0)
                layout.addWidget(labResolution,2,0)

                self.editWidth = xmlstore.gui_qt4.ScientificDoubleEditor(self)
                self.editHeight = xmlstore.gui_qt4.ScientificDoubleEditor(self)
                self.editResolution = xmlstore.gui_qt4.ScientificDoubleEditor(self)
                layout.addWidget(self.editWidth,     0,1)
                layout.addWidget(self.editHeight,    1,1)
                layout.addWidget(self.editResolution,2,1)
                
                labWidthUnit = QtGui.QLabel('cm',self)
                labHeightUnit = QtGui.QLabel('cm',self)
                labResolutionUnit = QtGui.QLabel('dpi',self)
                layout.addWidget(labWidthUnit,     0,2)
                layout.addWidget(labHeightUnit,    1,2)
                layout.addWidget(labResolutionUnit,2,2)
                
                layout.setColumnStretch(3,1)

                layoutButtons = QtGui.QHBoxLayout()

                # Add "OK" button
                self.bnOk = QtGui.QPushButton('&OK',self)
                self.connect(self.bnOk, QtCore.SIGNAL('clicked()'), self.accept)
                layoutButtons.addWidget(self.bnOk)

                # Add "Cancel" button
                self.bnCancel = QtGui.QPushButton('&Cancel',self)
                self.connect(self.bnCancel, QtCore.SIGNAL('clicked()'), self.reject)
                layoutButtons.addWidget(self.bnCancel)
                
                layout.addLayout(layoutButtons,3,0,1,4)

                self.setLayout(layout)
                
                self.setWindowIcon(getIcon('filesaveas.png'))
                self.setWindowTitle('Figure export settings')

        dialog = ExportSettings(self)
        oldwidth = self.canvas.figure.get_figwidth()
        oldheight = self.canvas.figure.get_figheight()
        olddpi = self.canvas.figure.get_dpi()
        dialog.editWidth.setValue (self.figure['Width' ].getValue(usedefault=True),'%.1f')
        dialog.editHeight.setValue(self.figure['Height'].getValue(usedefault=True),'%.1f')
        dialog.editResolution.setValue(olddpi,'%.0f')
        if dialog.exec_()!=QtGui.QDialog.Accepted: return

        # Routine to get list of possible file types and extensions
        # taken from NavigationToolbar2QT.save_figure (backend_qt4.py)
        filetypes = self.canvas.get_supported_filetypes_grouped()
        sorted_filetypes = filetypes.items()
        sorted_filetypes.sort()
        default_filetype = 'png'
        filters = []
        selectedFilter = None
        for name, exts in sorted_filetypes:
            exts_list = " ".join(['*.%s' % ext for ext in exts])
            filter = '%s (%s)' % (name, exts_list)
            if default_filetype in exts:
                selectedFilter = filter
            filters.append(filter)
        filters = ';;'.join(filters)

        fname = QtGui.QFileDialog.getSaveFileName(self,'Choose location to save plot to','',filters,selectedFilter)
        if fname:
            # Calculate desired width and height in inches
            width = dialog.editWidth.value()/2.54
            height = dialog.editHeight.value()/2.54
            QtGui.qApp.setOverrideCursor(QtCore.Qt.WaitCursor)
            try:
                agg = self.canvas.switch_backends(FigureCanvasAgg)
                self.canvas.figure.set_figwidth(width)
                self.canvas.figure.set_figheight(height)
                agg.print_figure(unicode(fname),dpi=dialog.editResolution.value(), facecolor='w', edgecolor='w', orientation='portrait')
                self.canvas.figure.set_figwidth(oldwidth)
                self.canvas.figure.set_figheight(oldheight)
                self.canvas.figure.set_canvas(self.canvas)
            finally:
                QtGui.qApp.restoreOverrideCursor()
        
    def onPrint(self):
        """Called when the user clicks the "Print..." button.
        """
        printer = QtGui.QPrinter(QtGui.QPrinter.HighResolution)
        printDialog = QtGui.QPrintDialog(printer, self)
        if printDialog.exec_()!=QtGui.QDialog.Accepted: return
        
        QtGui.qApp.setOverrideCursor(QtCore.Qt.WaitCursor)

        canvas = self.canvas.switch_backends(FigureCanvasAgg)

        # Store current DPI and colors.
        origDPI       = canvas.figure.get_dpi()
        origfacecolor = canvas.figure.get_facecolor()
        origedgecolor = canvas.figure.get_edgecolor()

        try:
            # Set the document name
            printer.setDocName(self.figure['Title'].getValue(usedefault=True))
        
            # Get the printer's resolution
            res = printer.resolution()

            # Adjust figure/canvas DPI and colors for printer.
            canvas.figure.set_dpi(res)
            canvas.figure.set_facecolor('w')
            canvas.figure.set_edgecolor('w')

            # Draw the plot (in memory)
            canvas.draw()

            # matplotlib is in rgba byte order.
            # qImage wants to put the bytes into argb format and
            # is in a 4 byte unsigned int.  little endian system is LSB first
            # and expects the bytes in reverse order (bgra).
            if (QtCore.QSysInfo.ByteOrder == QtCore.QSysInfo.LittleEndian):
                stringBuffer = canvas.renderer._renderer.tostring_bgra()
            else:
                stringBuffer = canvas.renderer._renderer.tostring_argb()
            qImage = QtGui.QImage(stringBuffer, canvas.renderer.width, canvas.renderer.height, QtGui.QImage.Format_ARGB32)

            # Find the position where to start drawing (in order to center the figure on page)
            pagerect = printer.pageRect()
            paperrect = printer.paperRect()
            top  = (paperrect.height()-qImage.height())/2. - pagerect.top()
            left = (paperrect.width() -qImage.width() )/2. - pagerect.left()

            # Draw the image with the printer's painter
            p = QtGui.QPainter(printer)
            try:
                p.drawImage(QtCore.QPoint(left, top), qImage)
            finally:
                p.end()
                
        finally:
            # Restore original DPI and colors.
            canvas.figure.set_dpi(origDPI)
            canvas.figure.set_facecolor(origfacecolor)
            canvas.figure.set_edgecolor(origedgecolor)

            # Restore original canvas.
            self.figure.figure.set_canvas(self.canvas)

            QtGui.qApp.restoreOverrideCursor()

    def onDetach(self):
        """Called when the user clicks the "Detach" button. This opens
        a new dialog with the currently shown figure, with all figure
        settings copied form the existing one.
        """
        fd = FigureDialog(self,sourcefigure=self.figure)
        fd.show()
        self.detachedfigures.append(fd)
        self.connect(fd, QtCore.SIGNAL('beforeDestroy'), self.beforeDetachedDestroy)
        
    def beforeDetachedDestroy(self,dialog):
        """Called just before a detached figure is destroyed (e.g., when it
        is closed by the user. This is used to remove all references to the
        figure about to be destroyed, so we will not try to close/destroy it
        ourselves later.
        """
        self.detachedfigures.remove(dialog)
        
    def destroy(self,destroyWindow=True,destroySubWindows=True):
        """This must be called by the parent object (i.e., the widget hosting
        the FigurePanel), to ensure that the figure + its settings are cleaned
        up nicely, and any child dialogs (detached figures, figre properties)
        are closed.
        """
        self.closeDetached()
        if self.dialogAdvanced is not None: self.dialogAdvanced.close()
        if self.figure is not None:
            self.figure.release()
            self.figure = None
        QtGui.QWidget.destroy(self,destroyWindow,destroySubWindows)

class FigureDialog(QtGui.QDialog):
    """Dialog that contains a single figure panel.
    """
    
    def __init__(self,parent=None,varstore=None,varname=None,sourcefigure=None,figureproperties=None,quitonclose=False,closebutton=None,destroyonclose=True):
        QtGui.QDialog.__init__(self,parent,QtCore.Qt.Window | QtCore.Qt.WindowMaximizeButtonHint | QtCore.Qt.WindowSystemMenuHint )

        if closebutton is None: closebutton = xmlstore.gui_qt4.needCloseButton()
        
        self.setSizeGripEnabled(True)
        layout = QtGui.QVBoxLayout(self)
        self.panel = FigurePanel(self,detachbutton=False)
        layout.addWidget(self.panel)

        self.panel.figure.setUpdating(False)
        if sourcefigure is not None:
            # A figure to copy settings from is provided.
            self.panel.figure.copyFrom(sourcefigure)
        elif figureproperties is not None:
            # An XML DOM tree with figure settings is provided
            assert varstore is not None,'If figure properties are specified, the variable store must be given as well.'
            self.panel.figure.addDataSource('main',varstore)
            self.panel.plotFromProperties(figureproperties)
        elif varstore is not None and varname is not None:
            # A figure store and variable name are provided.
            self.panel.figure.addDataSource('main',varstore)
            self.panel.figure.addVariable(varname)
        else:
            # Nothing provided; figure will be empty.
            assert varstore is None and varname is None,'If a variable is to be plotted, both the variable store and the variable name must be provided.'
        self.panel.figure.setUpdating(True)
        
        if closebutton: self.panel.toolbar.addAction(getIcon('exit.png'),'Close',self.accept)

        title = self.panel.figure['Title'].getValue(usedefault=True)
        if title is None: title = 'Figure'
        self.setWindowTitle(title)

        # Prevent this window from keeping the application alive after the main window was closed.
        self.setAttribute(QtCore.Qt.WA_QuitOnClose,quitonclose)
        
        self.destroyonclose = destroyonclose

        self.resize(500, 500)
        
    def getFigure(self):
        return self.panel.figure
        
    def closeEvent(self,event):
        QtGui.QDialog.closeEvent(self,event)
        if self.destroyonclose: self.destroy()
        
    def destroy(self,destroyWindow = True, destroySubWindows = True):
        self.emit(QtCore.SIGNAL('beforeDestroy'),self)
        assert self.panel is not None, 'FigurePanel is None. This means FigureDialog.destroy() is now called for the second time.'
        self.panel.destroy()
        self.panel = None
        QtGui.QDialog.destroy(self,destroyWindow,destroySubWindows)

# =======================================================================
# Classes for editing a GOTM data file
# =======================================================================

class LinkedFileEditorDialog(QtGui.QDialog):

    def __init__(self,linkedfile,parent=None,title=None,datasourcedir=None):
        QtGui.QDialog.__init__(self,parent,QtCore.Qt.Dialog)

        self.privatestore = common.VariableStore()

        self.linkedfile = linkedfile.copy()  # Copies only the metadata, not the actual data!
        self.linkedfile.setDataFile(linkedfile.getDataFile())

        self.datasourcedir = datasourcedir
        
        #self.dlgEditFunction = None
        
        loRight = QtGui.QVBoxLayout()

        # Right panel: list of variables and plot panel.
        #lolist = QtGui.QHBoxLayout()
        #self.label = QtGui.QLabel('Variables:',self)
        #lolist.addWidget(self.label)
        #self.list = QtGui.QComboBox(self)
        #namedict = self.linkedfile.getVariableLongNames()
        #for name in self.linkedfile.keys():
        #    self.list.addItem(namedict[name],QtCore.QVariant(name))
        #self.list.setEnabled(self.list.count()>0)
        #lolist.addWidget(self.list,1)
        #loRight.addLayout(lolist)
        
        self.panels,self.dataeditors = [],[]
        def createPanel():
            widget = QtGui.QWidget(self)
            panel = FigurePanel(widget)
            tw = QtGui.QTabWidget(widget)
            de = FunctionVariableEditor(tw)
            tw.addTab(de,'Provide data')
            
            l = QtGui.QHBoxLayout()
            l.addWidget(tw)
            l.addWidget(panel)
            widget.setLayout(l)
            
            self.connect(de.bnApply, QtCore.SIGNAL('clicked()'), self.onApplyFunction)
            
            self.panels.append(panel)
            self.dataeditors.append(de)
            
            # Hide experimental function editor for now
            tw.hide()
            
            return panel,widget
        
        namedict = self.linkedfile.getVariableLongNames()
        if len(namedict)==1:
            panel,widget = createPanel()
            loRight.addWidget(widget)
        else:
            self.tabs = QtGui.QTabWidget(self)
            for name in self.linkedfile.getVariableNames():
                panel,widget = createPanel()
                self.tabs.addTab(widget,namedict[name])
            loRight.addWidget(self.tabs)
            self.connect(self.tabs, QtCore.SIGNAL('currentChanged(int)'), self.onTabChanged)

        for panel in self.panels:
            firstaction = panel.toolbar.actions()[0]
            #self.insertAction(panel,firstaction,'Specify function',self.onEditFunction)
            #self.actExport = self.insertAction(firstaction,'Export data...',self.onExport)
            #self.insertAction(firstaction,'Edit data...',self.onEditData)

        # Bottom panel: OK and Cancel buttons
        lobuttons = QtGui.QHBoxLayout()
        lobuttons.addStretch(1)

        self.buttonImport = QtGui.QPushButton('Import data...',self)
        self.buttonExport = QtGui.QPushButton('Export data...',self)
        self.buttonEdit   = QtGui.QPushButton('Edit data...',self)
        self.buttonOk = QtGui.QPushButton('OK',self)
        self.buttonCancel = QtGui.QPushButton('Cancel',self)
        
        self.buttonOk.setDefault(True)
        self.buttonOk.setFocus()
        
        lobuttons.addWidget(self.buttonImport)
        lobuttons.addWidget(self.buttonExport)
        lobuttons.addWidget(self.buttonEdit)
        lobuttons.addWidget(self.buttonOk)
        lobuttons.addWidget(self.buttonCancel)
        
        loRight.addLayout(lobuttons)

        self.setLayout(loRight)

        self.connect(self.buttonImport, QtCore.SIGNAL('clicked()'), self.onImport)
        self.connect(self.buttonExport, QtCore.SIGNAL('clicked()'), self.onExport)
        self.connect(self.buttonEdit,   QtCore.SIGNAL('clicked()'), self.onEditData)
        self.connect(self.buttonOk,     QtCore.SIGNAL('clicked()'), self.accept)
        self.connect(self.buttonCancel, QtCore.SIGNAL('clicked()'), self.reject)

        self.resize(750, 450)

        self.first = True
        
        self.progressdialog = QtGui.QProgressDialog('',QtCore.QString(),0,0,self,QtCore.Qt.Dialog|QtCore.Qt.WindowTitleHint)
        self.progressdialog.setModal(True)
        self.progressdialog.setMinimumDuration(0)
        self.progressdialog.setAutoReset(False)
        self.progressdialog.setWindowTitle('Parsing data file...')

        if title is not None: self.setWindowTitle(title)
        
    def insertAction(self,panel,before,string,icon=None,target=None):
        if target is None:
            target = icon
            icon = None
        act = QtGui.QAction(string,panel.toolbar)
        self.connect(act,QtCore.SIGNAL('triggered()'),target)
        panel.toolbar.insertAction(before,act)
        return act
        
    def onTabChanged(self,newtab):
        pass
        #if self.dlgEditFunction is not None: self.dlgEditFunction.hide()
        
    def onEditData(self):
        dialog = LinkedFileDataEditor(self.linkedfile,self,title='Edit %s' % unicode(self.windowTitle()).lower())
        if dialog.exec_()!=QtGui.QDialog.Accepted: return
        self.setData()
        
    def getCurrentVariable(self):
        """Returns the currently selected (visible) variable.
        """
        i = 0
        varnames = self.linkedfile.getVariableNames()
        if len(varnames)>1: i = self.tabs.currentIndex()
        return self.privatestore.getVariable(varnames[i])
        
    def onEditFunction(self):
        if self.dlgEditFunction is None:
            self.dlgEditFunction = FunctionVariableEditor(self,QtCore.Qt.Tool)
            self.connect(self.dlgEditFunction.bnApply, QtCore.SIGNAL('clicked()'), self.onApplyFunction)
        variable = self.getCurrentVariable()
        if not isinstance(variable,common.FunctionVariable): variable = None
        self.dlgEditFunction.setVariable(variable)
        self.dlgEditFunction.show()
        self.dlgEditFunction.activateWindow()
        
    def onApplyFunction(self):
        variable = self.getCurrentVariable()
        if not isinstance(variable,common.FunctionVariable):
            newvariable = common.FunctionVariable(variable,resolution=500)
            for d in variable.getDimensions():
                minv,maxv = self.linkedfile.getDimensionRange(d)
                if isinstance(minv,datetime.datetime):
                    minv,maxv = common.date2num(minv),common.date2num(maxv)
                    newvariable.addDimensionTransform(d,offset=minv)
                newvariable.setDimensionBounds(d,minv,maxv)
            variable = newvariable
            self.privatestore.addChild(variable)

        i = 0
        if len(self.panels)>1: i = self.tabs.currentIndex()
        #self.dlgEditFunction.apply(variable)
        self.dataeditors[i].apply(variable)
        self.panels[i].figure.update()
        
    def showEvent(self,ev):
        """Called when the window is shown. This allows us to perform
        lengthy initialization with a progress indicator as child of this window.
        """
        if self.first:
            self.setData()
            self.first = False
            for name,panel in zip(self.linkedfile.getVariableNames(),self.panels):
                panel.plot(name,self.privatestore)

    def setData(self,datafile=None):
        """This function loads a new data file and parses it to check for errors.
        When the dialog is shown for the very first time, this function is called
        without the datafile argument. At that point, no new data file is set.
        Instead, only the currently loaded datafile is parsed.
        """

        # Close any detached figures
        for panel in self.panels: panel.closeDetached()
        
        # If a new datafile is provided, load it as new current data file.
        # (but backup the old data file before doing so)
        if datafile is not None:
            oldlinkedfile = self.linkedfile
            self.linkedfile = self.linkedfile.copy()  # Copies only the metadata, not the actual data!
            self.linkedfile.setDataFile(datafile)

        # Try to parse the current data file.
        try:
            try:
                self.linkedfile.getData(callback=self.onParseProgress)
                
                # Release the old datafile that we have in backup.
                if datafile is not None: oldlinkedfile.release()
            finally:
                self.progressdialog.reset()
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Invalid data file', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            if datafile is None:
                # No backup file available (this is the initialization call with the original data file)
                # Create a new empty data file by copying only the metadata from the original file.
                # Thus we do have something to work with in the editor.
                oldlinkedfile = self.linkedfile
                self.linkedfile = oldlinkedfile.copy()  # Copies only the metadata, not the actual data!
                oldlinkedfile.release()
            else:
                # Release the partially loaded data file, and revert to the backup file.
                self.linkedfile.release()
                self.linkedfile = oldlinkedfile
                return
            
        # The data may be None, meaning that the data file is empty.
        # In that case, explicitly create an empty table.
        if self.linkedfile.data is None:
            self.linkedfile.clear()
        
        # Add copies of the individual variables to our private store.
        for varname,panel,dataeditor in zip(self.linkedfile.getVariableNames(),self.panels,self.dataeditors):
            v = self.linkedfile.getVariable(varname)
            self.privatestore.addChild(v.copy())
            panel.figure.update()
            if not isinstance(v,common.FunctionVariable): v = None
            dataeditor.setVariable(v)
        
        # Enable the "Export" button if the data file is valid.
        self.buttonExport.setEnabled(self.linkedfile.validate(None))

    def onParseProgress(self,progress,status):
        if self.progressdialog.isHidden(): self.progressdialog.show()
        if progress is not None:
            if self.progressdialog.maximum()==0: self.progressdialog.setMaximum(100)
            self.progressdialog.setValue(int(100*progress))
        elif self.progressdialog.maximum()!=0:
            self.progressdialog.setMaximum(0)
            self.progressdialog.setValue(0)
            
        self.progressdialog.setLabelText(status)
        QtGui.qApp.processEvents()

    def onImport(self):
        dir = ''
        if self.datasourcedir is not None: dir = self.datasourcedir.get('')
        path = unicode(QtGui.QFileDialog.getOpenFileName(self,'',dir,''))

        # If the browse dialog was cancelled, just return.
        if path=='': return
        
        # Store the data source directory
        if self.datasourcedir is not None:
            self.datasourcedir.set(os.path.dirname(path))

        # Create data file for file-on-disk, copy it to memory,
        # and release the data file. We do not want to lock the
        # file on disk while working with the scenario.
        df = xmlstore.datatypes.DataContainerDirectory.DataFileFile(path)
        memdf = xmlstore.datatypes.DataFileMemory.fromDataFile(df)
        df.release()

        # Use the in-memory data file.
        self.setData(memdf)
        memdf.release()

    def onExport(self):
        path = unicode(QtGui.QFileDialog.getSaveFileName(self,'','',''))
        
        # If the browse dialog was cancelled, just return.
        if path=='': return

        # Save data file.
        self.linkedfile.saveToFile(path)
            
    def destroy(self):
        for panel in self.panels: panel.destroy()
        QtGui.QDialog.destroy(self)

class FunctionVariableEditor(QtGui.QWidget):
    def __init__(self,parent=None,flags=QtCore.Qt.Widget):
        QtGui.QWidget.__init__(self,parent,flags)
        self.variable = None
        
        self.label = QtGui.QLabel('Expression:',self)
        self.edit = QtGui.QLineEdit(self)
        self.checkVectorized = QtGui.QCheckBox('Supports vectorized evaluation',self)
        
        self.bnApply = QtGui.QPushButton('Apply',self)
        layoutButtons = QtGui.QHBoxLayout()
        layoutButtons.addWidget(self.bnApply)
        layoutButtons.addStretch(1)
        
        layout = QtGui.QGridLayout()
        layout.addWidget(self.label,0,0)
        layout.addWidget(self.edit,0,1)
        layout.addWidget(self.checkVectorized,1,0,1,2)
        layout.addLayout(layoutButtons,2,0)
        layout.setRowStretch(3,1)
        
        self.setLayout(layout)
        
        self.setWindowTitle('Specify function')
        
    def setVariable(self,variable):
        if variable is None:
            self.checkVectorized.setChecked(True)
            self.edit.setText('')
        else:
            self.checkVectorized.setChecked(variable.vectorized)
            if variable.functions:
                self.edit.setText(variable.functions[0][1])
            
    def apply(self,target):
        target.clearFunctions()
        target.addFunction(unicode(self.edit.text()))
        target.setVectorized(bool(self.checkVectorized.isChecked()))

class LinkedFileDataEditor(QtGui.QDialog):

    class LinkedDataModel(QtCore.QAbstractItemModel):
        def __init__(self,datastore,type=0,autoload=True):
            QtCore.QAbstractItemModel.__init__(self)
            self.datastore = datastore
            self.type = type
            self.pos = 0
            
            self.datamatrix = None
            self.rowlabels = None
            self.datelabels = True
            
            if autoload: self.loadData()

        def loadData(self):
            rawdata = self.datastore.getData()
            self.rowlabels = None
            self.datamatrix = None
            if isinstance(self.datastore,data.LinkedMatrix):
                self.datamatrix = rawdata[-1]
                if len(self.datastore.dimensions)==1:
                    self.rowlabels = rawdata[0]
                    dimname = self.datastore.getDimensionNames()[0]
                    self.datelabels = (self.datastore.getDimensionInfo_raw(dimname)['datatype']=='datetime')
            elif isinstance(self.datastore,data.LinkedProfilesInTime):
                if self.type==0:
                    if self.pos<len(rawdata[1]) and self.pos>=0:
                        self.rowlabels = rawdata[1][self.pos]
                        self.datamatrix = rawdata[2][self.pos]
                        self.datelabels = False
                else:
                    self.rowlabels = rawdata[0]
                    self.datelabels = True
            else:
                assert False, 'Unknown data file type "%s".' % self.datastore.type
                
        def saveData(self):
            """Send changed data arrays back to the original data store.
            This is needed if the data objects have been replaced by new ones,
            not if the data are modified in place.
            """
            rawdata = self.datastore.getData()
            if isinstance(self.datastore,data.LinkedMatrix):
                if len(self.datastore.dimensions)==1:
                    rawdata[0] = self.rowlabels
                rawdata[-1] = self.datamatrix
            elif isinstance(self.datastore,data.LinkedProfilesInTime):
                if self.type==0:
                    assert self.rowlabels is not None, 'Table for profiles must have row labels.'
                    if self.pos>=0 and self.pos<len(rawdata[1]):
                        rawdata[1][self.pos] = self.rowlabels
                        rawdata[2][self.pos] = self.datamatrix
                else:
                    rawdata[0] = self.rowlabels
            else:
                assert False, 'Unknown data file type "%s".' % self.datastore.type
            self.datastore.dataChanged()
            
        def reset(self):
            self.loadData()
            QtCore.QAbstractItemModel.reset(self)
            
        def index(self,irow,icolumn,parent=None):
            if parent is None: parent=QtCore.QModelIndex()
            assert not parent.isValid(), 'Only the root can have child nodes.'
            return self.createIndex(irow,icolumn)

        def parent(self,index):
            return QtCore.QModelIndex()
            
        def rowCount(self,parent=None):
            if parent is None: parent=QtCore.QModelIndex()
            if parent.isValid(): return 0
            if self.rowlabels is not None:
                return self.rowlabels.shape[0]
            elif self.datamatrix is not None:
                return self.datamatrix.shape[0]
            else:
                return 0

        def columnCount(self,parent=None):
            if parent is None: parent=QtCore.QModelIndex()
            colcount = 0
            if self.rowlabels is not None:
                colcount += 1
            if self.datamatrix is not None:
                colcount += self.datamatrix.shape[1]
            return colcount

        def data(self,index,role=QtCore.Qt.DisplayRole):
            if role==QtCore.Qt.DisplayRole or role==QtCore.Qt.EditRole:
                rowindex = index.row()
                colindex = index.column()
                if self.rowlabels is not None: colindex -= 1
                if colindex==-1:
                    if self.datelabels:
                        val = common.num2date(self.rowlabels[rowindex])
                    else:
                        val = self.rowlabels[rowindex]
                else:
                    val = self.datamatrix[rowindex,colindex]

                if role==QtCore.Qt.DisplayRole:
                    if isinstance(val,datetime.datetime):
                        return QtCore.QVariant(xmlstore.util.formatDateTime(val))
                    else:
                        return QtCore.QVariant('%.6g' % float(val))
                        
                if isinstance(val,datetime.datetime):
                    val = xmlstore.gui_qt4.datetime2qtdatetime(val)
                else:
                    val = float(val)
                    
                return QtCore.QVariant(val)
                
            return QtCore.QVariant()
            
        def setData(self,index,value,role=QtCore.Qt.EditRole):
            # Only do something if we are editing the data.
            if role!=QtCore.Qt.EditRole: return True
            
            # Convert the new value from variant to native data type.
            if value.canConvert(QtCore.QVariant.DateTime):
                value = value.toDateTime()
                value.setTimeSpec(QtCore.Qt.UTC)
                value = common.date2num(xmlstore.gui_qt4.qtdatetime2datetime(value))
            elif value.canConvert(QtCore.QVariant.Double):
                (value,ok) = value.toDouble()
            else:
                assert False, 'Do not know variant type %s.' % val.type()
            
            # Find the row and column index of the edited variable, and
            # adjust the column index to skip row labels (if any).    
            rowindex = index.row()
            colindex = index.column()
            if self.rowlabels is not None: colindex -= 1
            
            if colindex==-1:
                # We have edited a row label. The table is kept sorted according
                # to row labels, so this means the position of the current row
                # may have change.
                if self.rowlabels[rowindex]==value: return True
                newrowindex = self.rowlabels.searchsorted(value)
                self.rowlabels[rowindex] = value
                if newrowindex!=rowindex and newrowindex!=rowindex+1:
                    # Row position should change
                    
                    # Buffer the label and data of the row that changes position.
                    buflab = self.rowlabels[rowindex]
                    if self.datamatrix is not None: bufdata = self.datamatrix[rowindex,:].copy()
                    
                    if newrowindex>rowindex+1:
                        # Row moves down
                        self.rowlabels[rowindex:newrowindex-1] = self.rowlabels[rowindex+1:newrowindex]
                        self.rowlabels[newrowindex-1] = buflab
                        if self.datamatrix is not None:
                            self.datamatrix[rowindex:newrowindex-1,:] = self.datamatrix[rowindex+1:newrowindex,:]
                            self.datamatrix[newrowindex-1,:] = bufdata
                        if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type!=0:
                            # We have edited the time of a profile - move the depth and values along as well.
                            self.datastore.data[1].insert(newrowindex-1,self.datastore.data[1].pop(rowindex))
                            self.datastore.data[2].insert(newrowindex-1,self.datastore.data[2].pop(rowindex))
                        self.emitRowsChanged(rowindex,newrowindex-1)
                    elif newrowindex<rowindex:
                        # Row moves up
                        self.rowlabels[newrowindex+1:rowindex+1] = self.rowlabels[newrowindex:rowindex]
                        self.rowlabels[newrowindex] = buflab
                        if self.datamatrix is not None:
                            self.datamatrix[newrowindex+1:rowindex+1,:] = self.datamatrix[newrowindex:rowindex,:]
                            self.datamatrix[newrowindex,:] = bufdata
                        if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type!=0:
                            # We have edited the time of a profile - move the depth and values along as well.
                            self.datastore.data[1].insert(newrowindex,self.datastore.data[1].pop(rowindex))
                            self.datastore.data[2].insert(newrowindex,self.datastore.data[2].pop(rowindex))
                        self.emitRowsChanged(newrowindex,rowindex)
            else:
                if self.datamatrix[rowindex,colindex]==value: return True
                self.datamatrix[rowindex,colindex] = value
                
            # Make sure that the data store knows that its contents have changed.
            self.datastore.dataChanged()
            
            # Notify any GUI elements attache dto the model that data have changed.
            self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex &,const QModelIndex &)'),index,index)
            
            return True
            
        def addRow(self,newrowindex=None,nrows=1):
            """Adds one or more rows to the current data object.
            """
            # By default: add the new row to the end of the list.
            if newrowindex is None: newrowindex = self.datamatrix.shape[0]
            
            self.beginInsertRows(QtCore.QModelIndex(),newrowindex,newrowindex+nrows-1)
            
            if self.datamatrix is not None:
                # Add a new row to the data matrix.
                newrows = numpy.zeros((nrows,self.datamatrix.shape[1]),self.datamatrix.dtype)
                self.datamatrix = numpy.concatenate((self.datamatrix[:newrowindex,:],newrows,self.datamatrix[newrowindex:,:]),axis=0)
                
            if self.rowlabels is not None:
                delta = 1.
                if len(self.rowlabels)==0:
                    # Creating the first row(s).
                    start = 0.
                    if self.datelabels: start = common.date2num(datetime.datetime.today())
                elif newrowindex>0 and newrowindex<len(self.rowlabels):
                    # Inserting in the middle.
                    above,below = self.rowlabels[newrowindex-1],self.rowlabels[newrowindex]
                    delta = (below-above)/(nrows+1)
                    start = above + delta
                elif newrowindex==0:
                    # Inserting at the top.
                    below = self.rowlabels[newrowindex]
                    if len(self.rowlabels)>1:
                        delta = self.rowlabels[newrowindex+1] - below
                    start = below-nrows*delta
                else:
                    # Appending to the bottom.
                    above = self.rowlabels[newrowindex-1]
                    if len(self.rowlabels)>1:
                        delta = above - self.rowlabels[newrowindex-2]
                    start = above+delta
                
                # Add a new row to the list of row labels.
                values = numpy.linspace(start,start+(nrows-1)*delta,nrows)
                self.rowlabels = numpy.concatenate((self.rowlabels[:newrowindex],values,self.rowlabels[newrowindex:]))

                if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type!=0:
                    # We have added one or more profiles - add empty arrays for the associated depths and values.
                    dimdatatype = self.datastore.mpldatatypes[self.datastore.dimensions[self.datastore.dimensionorder[1]]['datatype']]
                    valdatatype = self.datastore.mpldatatypes[self.datastore.datatype]
                    nvar = len(self.datastore.vardata)
                    for i in range(nrows):
                        self.datastore.data[1].insert(newrowindex,numpy.empty((0,),    dimdatatype))
                        self.datastore.data[2].insert(newrowindex,numpy.empty((0,nvar),valdatatype))
            
            # Replace data in the store by our new objects.    
            self.saveData()
            
            self.endInsertRows()
            
        def removeRow(self,start,stop=None):
            """Remove a contiguous set of one or more rows from the current data object.
            """
        
            # If no last row is provided, just remove the one row (i.e., the start row)
            if stop is None: stop = start
            
            self.beginRemoveRows(QtCore.QModelIndex(),start,stop)
            
            if self.datamatrix is not None:
                # Remove row from data matrix.
                self.datamatrix = numpy.concatenate((self.datamatrix[0:start,:],self.datamatrix[stop+1:,:]))
                
            if self.rowlabels is not None:
                # Remove row from list of row labels.
                self.rowlabels = numpy.concatenate((self.rowlabels[0:start],self.rowlabels[stop+1:]))

                if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type!=0:
                    # We have removed one or more profiles - remove arrays for the associated depths and values.
                    for i in range(stop-start+1):
                        del self.datastore.data[1][start]
                        del self.datastore.data[2][start]
                
            # Replace data in the store by our new objects.    
            self.saveData()
            
            self.endRemoveRows()
            
        def emitRowsChanged(self,start,stop):
            """Shortcut routine for internal use that tells any GUI elements attached
            to the model that all data in the specified rows have changed.
            """
            startindex = self.index(start,0)
            stopindex = self.index(stop,self.columnCount()-1)
            self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex &,const QModelIndex &)'),startindex,stopindex)
            
        def flags(self,index):
            return QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable | QtCore.Qt.ItemIsEditable
            
        def headerData(self,section, orientation, role=QtCore.Qt.DisplayRole):
            # Return no header if the orientation is not horizontal, or the role is not display or tooltip.
            if orientation!=QtCore.Qt.Horizontal or role not in (QtCore.Qt.DisplayRole,QtCore.Qt.ToolTipRole):  return QtCore.QVariant()
            
            if isinstance(self.datastore,(data.LinkedMatrix,data.LinkedProfilesInTime)):
                if len(self.datastore.dimensions)>0: section -= 1
                if section==-1:
                    # Header requested for coordinate variable
                    i = 0
                    if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type==0: i += 1
                    val = self.datastore.getDimensionNames()[i]
                    info = self.datastore.getDimensionInfo(val)
                    longname = info.get('label',val)
                    unit = info.get('unit','')
                else:
                    # Header requested for value variable
                    val = self.datastore.keys()[section]
                    var = self.datastore[val]
                    longname = var.getLongName()
                    unit = var.getUnit()
                        
                # If the long name is small, it will be shown directly, making the tooltip superfluous.
                if role==QtCore.Qt.ToolTipRole and len(longname)<10: return QtCore.QVariant()
                
                if role==QtCore.Qt.ToolTipRole or len(longname)<10: val = longname
                if unit: val += ' (%s)' % unit
                return QtCore.QVariant(val)
                
            # No tooltip
            return QtCore.QVariant()
    
    class LinkedFileDelegate(QtGui.QItemDelegate):

        def __init__(self,parent=None):
            QtGui.QItemDelegate.__init__(self,parent)

        # createEditor (inherited from QtGui.QItemDelegate)
        #   Creates the editor widget for the model item at the given index
        def createEditor(self, parent, option, index):
            val = index.data(QtCore.Qt.EditRole)
            type = val.type()
            if type==QtCore.QVariant.Double:
                editor = xmlstore.gui_qt4.ScientificDoubleEditor(parent)
                self.currenteditor = editor
            elif type==QtCore.QVariant.DateTime:
                editor = QtGui.QDateTimeEdit(parent)

            # Install event filter that captures key events for view from the editor (e.g. return press).
            editor.installEventFilter(self)
            
            return editor
            
        # setEditorData (inherited from QtGui.QItemDelegate)
        #   Sets value in the editor widget, for the model item at the given index
        def setEditorData(self, editor,index):
            value = index.data(QtCore.Qt.EditRole)
            if not value.isValid(): return
            type = value.type()
            if type==QtCore.QVariant.Double:
                value,ret = value.toDouble()
                editor.setValue(value)
            elif type==QtCore.QVariant.DateTime:
                value = value.toDateTime()
                editor.setDateTime(value)

        # setModelData (inherited from QtGui.QItemDelegate)
        #   Obtains the value from the editor widget, and set it for the model item at the given index
        def setModelData(self, editor, model, index):
            if isinstance(editor,xmlstore.gui_qt4.ScientificDoubleEditor):
                editor.interpretText()
                if not editor.hasAcceptableInput(): return
                model.setData(index, QtCore.QVariant(editor.value()))
            elif isinstance(editor,QtGui.QDateTimeEdit):
                model.setData(index, QtCore.QVariant(editor.dateTime()))

    class CustomListView(QtGui.QListView):
        def keyPressEvent(self,event):
            if event.key()==QtCore.Qt.Key_Delete:
                self.emit(QtCore.SIGNAL('deletePressed()'))
                event.accept()
                return
            QtGui.QListView.keyPressEvent(self,event)

    class CustomTableView(QtGui.QTableView):
        def keyPressEvent(self,event):
            if event.key()==QtCore.Qt.Key_Delete:
                self.emit(QtCore.SIGNAL('deletePressed()'))
                event.accept()
                return
            QtGui.QTableView.keyPressEvent(self,event)
                
    def __init__(self,linkedfile,parent=None,title=None):
        QtGui.QDialog.__init__(self,parent)

        self.linkedfile = linkedfile

        lo = QtGui.QGridLayout()

        # Left panel: data editor
        if isinstance(self.linkedfile,data.LinkedProfilesInTime):
            self.labelTimes = QtGui.QLabel('Time:',self)
            lo.addWidget(self.labelTimes,0,0)
            self.labelProfile = QtGui.QLabel('Profile:',self)
            lo.addWidget(self.labelProfile,0,1)
            self.listTimes = self.CustomListView(self)
            self.listmodel = LinkedFileDataEditor.LinkedDataModel(self.linkedfile,type=1)
            self.listTimes.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
            self.listTimes.setModel(self.listmodel)
            self.listTimes.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
            lo.addWidget(self.listTimes,1,0)
            self.connect(self.listTimes, QtCore.SIGNAL('customContextMenuRequested(const QPoint &)'), self.onTableContextMenu)
            self.connect(self.listTimes.selectionModel(), QtCore.SIGNAL('currentChanged(const QModelIndex &,const QModelIndex &)'), self.onTimeChanged)
            self.connect(self.listTimes, QtCore.SIGNAL('deletePressed()'), self.onDelete)
        self.tableData = self.CustomTableView(self)
        self.tablemodel = LinkedFileDataEditor.LinkedDataModel(self.linkedfile)
        self.tableData.verticalHeader().hide()
        self.tableData.verticalHeader().setDefaultSectionSize(20)
        self.tableData.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        self.tableData.setModel(self.tablemodel)
        self.tabledelegate = self.LinkedFileDelegate()
        self.tableData.setItemDelegate(self.tabledelegate)
        self.tableData.setContextMenuPolicy(QtCore.Qt.CustomContextMenu)
        self.connect(self.tableData, QtCore.SIGNAL('customContextMenuRequested(const QPoint &)'), self.onTableContextMenu)
        self.connect(self.tableData, QtCore.SIGNAL('deletePressed()'), self.onDelete)
        
        lo.addWidget(self.tableData,max(0,lo.rowCount()-1),max(0,lo.columnCount()-1))

        if isinstance(self.linkedfile,data.LinkedProfilesInTime):
            self.listTimes.setCurrentIndex(self.listmodel.index(0,0))

        # Bottom panel: OK and Cancel button
        lobuttons = QtGui.QHBoxLayout()
        self.buttonOk     = QtGui.QPushButton('OK',    self)
        self.buttonCancel = QtGui.QPushButton('Cancel',self)
        lobuttons.addStretch(1)
        lobuttons.addWidget(self.buttonOk)
        lobuttons.addWidget(self.buttonCancel)
        
        lo.addLayout(lobuttons,lo.rowCount(),0,1,lo.columnCount())

        self.setLayout(lo)

        self.connect(self.buttonOk,     QtCore.SIGNAL('clicked()'), self.accept)
        self.connect(self.buttonCancel, QtCore.SIGNAL('clicked()'), self.reject)

        self.buttonOk.setDefault(True)
        self.buttonOk.setFocus()

        self.resize(750, 450)

        if title is not None: self.setWindowTitle(title)
        
    def addRow(self,view=None,newindex=None,nrows=1):
        """Event handler: called when user clicks the "Add row" button.
        """
        if view is None: view = self.tableData
        model = view.model()
        if newindex is None: newindex = model.rowCount()
        model.addRow(newindex,nrows)
        
        # Select newly added rows
        selectmodel = view.selectionModel()
        selectmodel.clear()
        selectmodel.select(QtGui.QItemSelection(model.index(newindex,0),model.index(newindex+nrows-1,0)),QtGui.QItemSelectionModel.ClearAndSelect|QtGui.QItemSelectionModel.Rows)
        selectmodel.setCurrentIndex(model.index(newindex,0),QtGui.QItemSelectionModel.NoUpdate)
        
    def onDelete(self):
        self.removeRow(self.sender())
        
    def removeRow(self,view=None):
        """Event handler: called when user clicks the "Remove row" button.
        Removes all selected rows.
        """
        if view is None: view = self.tableData
        model = view.model()
        
        # Get selected rows; do nothing if no rows are selected.
        rows = view.selectionModel().selectedRows()
        if len(rows)==0: return
        
        # Get indices of selected rows and sort them in increasing order.
        rows = [r.row() for r in rows]
        rows.sort(reverse=True)
        
        # Iterate over selected rows, deleting contiguous blocks one by one.
        bottom = rows[0]
        top = bottom
        for currow in rows[1:]:
            if currow!=top-1:
                # New row is not attached to previous selection.
                # Therefore, first remove previous selection.
                model.removeRow(top,bottom)
                bottom = currow
            top = currow
        model.removeRow(top,bottom)
        
    def onTableContextMenu(self,pos):
        table = self.sender()
    
        # Get list of selected rows.
        selectedrows = sorted([r.row() for r in table.selectionModel().selectedRows()])

        # Build context menu
        menu = QtGui.QMenu(self)
        
        actRemoveRows, actInsertRowsAbove, actInsertRowsBelow, actNewRow = None, None, None, None
        if selectedrows:
            rowtext = 'row'
            if len(selectedrows)>1: rowtext = '%i rows' % len(selectedrows)
            submenu = menu.addMenu(getIcon('insert_table_row.png'),'Insert %s' % rowtext)
            actInsertRowsAbove = submenu.addAction('above selection')
            actInsertRowsBelow = submenu.addAction('below selection')
            actRemoveRows = menu.addAction(getIcon('delete_table_row.png'),'Delete selected %s' % rowtext)
        elif table.model().datamatrix is not None or table.model().rowlabels is not None:
            actNewRow = menu.addAction('Add row')

        if menu.isEmpty(): return

        # Show context menu and take action.
        actChosen = menu.exec_(table.mapToGlobal(pos))
        if not actChosen: return
        
        if actChosen==actRemoveRows:
            self.removeRow(table)
        elif actChosen==actInsertRowsAbove:
            self.addRow(table,selectedrows[0],len(selectedrows))
        elif actChosen==actInsertRowsBelow:
            self.addRow(table,selectedrows[-1]+1,len(selectedrows))
        elif actChosen==actNewRow:
            self.addRow(table,0,1)
    
    def onTimeChanged(self,current,previous):
        """Event handler: called when the user selects another date (only used for profiles in time).
        """
        self.tablemodel.pos = current.row()
        self.tablemodel.reset()
        self.labelProfile.setText('Profile for %s' % self.listmodel.data(current).toString())
        self.tableData.horizontalScrollBar().setValue(0)
        self.tableData.verticalScrollBar().setValue(0)
