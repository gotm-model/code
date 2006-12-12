#!/usr/bin/python

#$Id: visualizer.py,v 1.3 2006-12-12 08:19:32 jorn Exp $

from PyQt4 import QtGui,QtCore

import common,commonqt

import matplotlib
matplotlib.use('Qt4Agg')
matplotlib.rcParams['numerix'] = 'numeric'

import sys,datetime
import xml.sax
import os.path

import matplotlib.figure
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_agg import FigureCanvasAgg

def loadResult(path):
    result = common.Result()

    try:
        if path.endswith('.gotmresult'):
            result.load(path)
        elif path.endswith('.nc'):
            result.attach(path)
        else:
            # We do not recognize this file type; try both GOTM result and NetCDF
            done = True
            try:
                result.attach(path)
            except Exception,e:
                done = False
            if not done:
                done = True
                try:
                    result.load(path)
                except Exception,e:
                    done = False
            if (not done):
                raise Exception('The file "'+path+'" is probably not a GOTM result or a NetCDF file.')
    except:
        result.unlink()
        result = None
        raise

    return result

class OpenWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)

        self.pathOpen = commonqt.PathEditor(self,header='File to open: ')
        self.pathOpen.filter = 'GOTM result files (*.gotmresult);;NetCDF files (*.nc);;All files (*.*)'

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.pathOpen)
        self.setLayout(layout)
        self.connect(self.pathOpen, QtCore.SIGNAL("onChanged()"), self.completeStateChanged)

    def completeStateChanged(self):
        self.emit(QtCore.SIGNAL('onCompleteStateChanged()'))

    def isComplete(self):
        return self.pathOpen.hasPath()

    def getResult(self):
        return loadResult(self.pathOpen.path())

class PageOpen(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('Specify the location of the result you want to view.',self)
        self.openwidget = OpenWidget(self)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addWidget(self.openwidget)
        layout.addStretch()
        self.setLayout(layout)
        self.connect(self.openwidget, QtCore.SIGNAL('onCompleteStateChanged()'), self.completeStateChanged)

    def isComplete(self):
        return self.openwidget.isComplete()

    def saveData(self,mustbevalid):
        if not mustbevalid: return True
        try:
            result = self.openwidget.getResult()
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            return False
        self.parent().shared['result'] = result
        self.parent().shared['scenario'] = result.scenario
        return True

class PageSave(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.result = parent.shared['result']

        self.label = QtGui.QLabel('Do you want to save the result of your simulation?',self)
        self.bngroup     = QtGui.QButtonGroup()
        self.radioNoSave = QtGui.QRadioButton('No, I do not want to save the result.', parent)
        self.radioSave   = QtGui.QRadioButton('Yes, I want to save the result to file.', parent)

        self.pathSave = commonqt.PathEditor(self,header='File to save to: ',save=True)
        self.pathSave.filter = 'GOTM result files (*.gotmresult);;All files (*.*)'
        self.pathSave.forcedextension = '.gotmresult'

        self.bngroup.addButton(self.radioNoSave, 0)
        self.bngroup.addButton(self.radioSave,   1)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addWidget(self.radioNoSave)
        layout.addWidget(self.radioSave)
        layout.addWidget(self.pathSave)
        layout.addStretch()
        self.setLayout(layout)

        self.connect(self.bngroup,  QtCore.SIGNAL("buttonClicked(int)"), self.onSourceChange)
        self.connect(self.pathSave, QtCore.SIGNAL("onChanged()"),        self.completeStateChanged)

        self.radioSave.setChecked(True)
        self.onSourceChange()

    def onSourceChange(self):
        checkedid = self.bngroup.checkedId()
        self.pathSave.setVisible(checkedid==1)
        self.completeStateChanged()

    def isComplete(self):
        checkedid = self.bngroup.checkedId()
        if   checkedid==0:
            return True
        elif checkedid==1:
            return self.pathSave.hasPath()

    def saveData(self,mustbevalid):
        if not mustbevalid: return True
        checkedid = self.bngroup.checkedId()
        if checkedid==1:
            try:
                self.result.save(self.pathSave.path())
            except Exception,e:
                print e
                QtGui.QMessageBox.critical(self, 'Unable to save result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False
        return True

class PageFinal(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('You are now done.',self)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addStretch()
        self.setLayout(layout)

    def isComplete(self):
        return True

class FigurePanel(QtGui.QWidget):
    
    def __init__(self,parent):
        QtGui.QWidget.__init__(self,parent)

        bgcolor = self.palette().window().color()
        mplfigure = matplotlib.figure.Figure(facecolor=(bgcolor.red()/255., bgcolor.green()/255., bgcolor.blue()/255.),edgecolor=(bgcolor.red()/255., bgcolor.green()/255., bgcolor.blue()/255.))
        self.figure = common.Figure(mplfigure)

        self.canvas = FigureCanvas(mplfigure)
        self.canvas.setSizePolicy(QtGui.QSizePolicy.Expanding,QtGui.QSizePolicy.Expanding)
        self.canvas.setMinimumSize(300,250)
        self.figure.canvas = self.canvas

        self.factory = commonqt.PropertyEditorFactory(self.figure.properties,live=True)

        # Controls for time range.
        self.dateeditStart = self.factory.createEditor(['TimeAxis','Minimum'],self)
        self.dateeditStop  = self.factory.createEditor(['TimeAxis','Maximum'],self)

        # Controls for depth range.
        self.lineeditStartDepth = self.factory.createEditor(['DepthAxis','Minimum'],self)
        self.lineeditStopDepth = self.factory.createEditor(['DepthAxis','Maximum'],self)

        layout = QtGui.QVBoxLayout()
        
        layoutOptions = QtGui.QGridLayout()

        layoutTime = QtGui.QHBoxLayout()
        layoutTime.addWidget(self.dateeditStart.editor)
        self.labelDateTo = QtGui.QLabel(self.tr(' to '),self)
        layoutTime.addWidget(self.labelDateTo)
        layoutTime.addWidget(self.dateeditStop.editor)
        self.labelDateRange = QtGui.QLabel(self.tr('Time range:'),self)
        layoutOptions.addWidget(self.labelDateRange,1,0)
        layoutOptions.addLayout(layoutTime,1,1)
        
        layoutDepth = QtGui.QHBoxLayout()
        layoutDepth.addWidget(self.lineeditStartDepth.editor)
        self.labelDepthTo = QtGui.QLabel(self.tr(' to '),self)
        layoutDepth.addWidget(self.labelDepthTo)
        layoutDepth.addWidget(self.lineeditStopDepth.editor)
        self.labelDepthRange = QtGui.QLabel(self.tr('Depth range:'),self)
        layoutOptions.addWidget(self.labelDepthRange,2,0)
        layoutOptions.addLayout(layoutDepth,2,1)

        self.layoutOptions = layoutOptions

        self.buttonAdvanced = QtGui.QPushButton(self.tr('Advanced...'),self)
        self.buttonAdvanced.setAutoDefault(False)
        self.buttonAdvanced.setDefault(False)
        self.connect(self.buttonAdvanced, QtCore.SIGNAL('clicked()'), self.onAdvancedClicked)
        layoutOptions.addWidget(self.buttonAdvanced,3,0,1,2)

        self.layoutButtons = QtGui.QHBoxLayout()

        # Button for showing/hiding properties
        self.buttonProperties = QtGui.QPushButton(self.tr('Edit plot &properties'),self)
        self.buttonProperties.setAutoDefault(False)
        self.buttonProperties.setDefault(False)
        self.connect(self.buttonProperties, QtCore.SIGNAL('clicked()'), self.onPropertiesClicked)
        self.layoutButtons.addWidget(self.buttonProperties)

        # Button for exporting to file
        self.buttonExport = QtGui.QPushButton(self.tr('&Export to file'),self)
        self.buttonExport.setAutoDefault(False)
        self.buttonExport.setDefault(False)
        self.connect(self.buttonExport, QtCore.SIGNAL('clicked()'), self.onExport)
        self.layoutButtons.addWidget(self.buttonExport)

        # Button for printing
        self.buttonPrint = QtGui.QPushButton(self.tr('&Print'),self)
        self.buttonPrint.setAutoDefault(False)
        self.buttonPrint.setDefault(False)
        self.connect(self.buttonPrint, QtCore.SIGNAL('clicked()'), self.onPrint)
        self.layoutButtons.addWidget(self.buttonPrint)

        self.widgetProperties = QtGui.QWidget(self)
        self.widgetProperties.setLayout(layoutOptions)
        self.widgetProperties.setVisible(False)

        layout.addWidget(self.canvas)
        layout.addLayout(self.layoutButtons)
        layout.addWidget(self.widgetProperties)

        self.setLayout(layout)

        self.dialogAdvanced = None

        # Initially disable all controls; we have no plot to configure yet...
        self.setEnabled(False)

    def plot(self,varstore,varname):
        self.figure.setUpdating(False)
        self.figure.clearSources()
        self.figure.clearProperties()
        self.figure.addDataSource('main',varstore)
        self.figure.addVariable(varname)
        self.figure.setUpdating(True)
        self.setEnabled(True)

    def plotFromProperties(self,properties):
        self.figure.setProperties(properties)    
        self.setEnabled(True)

    def onAdvancedClicked(self):
        if self.dialogAdvanced==None:
            self.dialogAdvanced = commonqt.PropertyEditorDialog(self,self.figure.properties,title='Figure properties')
        self.dialogAdvanced.show()
        self.dialogAdvanced.activateWindow()

    def onPropertiesClicked(self):
        window = commonqt.getTopLevelWidget(self)
        window.setUpdatesEnabled(False)
        sz = window.size()
        if not self.widgetProperties.isVisible():
            self.widgetProperties.setVisible(True)
            self.buttonProperties.setText('Hide plot &properties')
            self.propheight = self.widgetProperties.height()+self.layout().spacing()
            sz.setHeight(sz.height() + self.propheight)
        else:
            self.widgetProperties.setVisible(False)
            self.buttonProperties.setText('Edit plot &properties')
            sz.setHeight(sz.height() - self.propheight)
        window.resize(sz)
        window.setUpdatesEnabled(True)

    def onExport(self):
        fname = QtGui.QFileDialog.getSaveFileName(self,'Choose location to save plot to','','Portable Network Graphics (*.png);;Encapsulated PostScript (*.eps);;Scalable Vector Graphics (*.svg);;Bitmap (*.bmp)')
        if fname:
            QtGui.qApp.setOverrideCursor(QtCore.Qt.WaitCursor)
            agg = self.canvas.switch_backends(FigureCanvasAgg)
            agg.print_figure(str(fname.toLatin1()),dpi=150, facecolor='w', edgecolor='w', orientation='portrait')
            self.canvas.figure.set_canvas(self.canvas)
            QtGui.qApp.restoreOverrideCursor()

            #self.canvas.print_figure( str(fname.toLatin1()) )
        

    def onPrint(self):
        printer = QtGui.QPrinter(QtGui.QPrinter.HighResolution)
        printDialog = QtGui.QPrintDialog(printer, self)
        if printDialog.exec_() == QtGui.QDialog.Accepted:
            print 'Printing to '+printer.printerName()
            p = QtGui.QPainter(printer)
            
            canvas = self.canvas.switch_backends(FigureCanvasAgg)

            # Store current DPI and colors.
            origDPI       = canvas.figure.dpi.get()
            origfacecolor = canvas.figure.get_facecolor()
            origedgecolor = canvas.figure.get_edgecolor()

            res = printer.resolution()
            if res>600: res=600
            print 'Using resolution of '+ str(res) + ' dpi.'

            # Adjust DPI and colors for printer.
            canvas.figure.dpi.set(res)
            canvas.figure.set_facecolor('w')
            canvas.figure.set_edgecolor('w')


            # Draw the plot (in memory)
            print 'MatPlotLib: creating Agg in-memory bitmap.'
            canvas.draw()

            # matplotlib is in rgba byte order.
            # qImage wants to put the bytes into argb format and
            # is in a 4 byte unsigned int.  little endian system is LSB first
            # and expects the bytes in reverse order (bgra).
            print 'Converting in-memory bitmap to format suitable for Qt.'
            if (QtCore.QSysInfo.ByteOrder == QtCore.QSysInfo.LittleEndian):
                stringBuffer = canvas.renderer._renderer.tostring_bgra()
            else:
                stringBuffer = canvas.renderer._renderer.tostring_argb()
                
            print 'Creating QImage object from in-memory data. Using width = ' + str(canvas.renderer.width) + ', height = '+ str(canvas.renderer.height)
            qImage = QtGui.QImage(stringBuffer, canvas.renderer.width, canvas.renderer.height, QtGui.QImage.Format_ARGB32)
            #pixmap = QtGui.QPixmap.fromImage(qImage)
            #print 'Drawing pixmap to QPainter object connected to printer.'
            #p.drawPixmap(QtCore.QPoint(0, 0), pixmap )
            print 'Drawing bitmap to QPainter object connected to printer.'
            p.drawImage(QtCore.QPoint(0, 0), qImage)
            #p.drawImage(QtCore.QPoint(0, 0), qImage, QtCore.QRect(0,0,1000,1000))
            print 'Done drawing; closing painter.'
            p.end()

            print 'printing done; restoring original figure resolution and color.'

            # Restore original DPI and colors.
            canvas.figure.dpi.set(origDPI)
            canvas.figure.set_facecolor(origfacecolor)
            canvas.figure.set_edgecolor(origedgecolor)

            # Restore original canvas.
            self.figure.figure.set_canvas(self.canvas)

class FigureDialog(QtGui.QDialog):
    
    def __init__(self,parent,sourcefigure):
        QtGui.QDialog.__init__(self,None,QtCore.Qt.Window | QtCore.Qt.WindowMaximizeButtonHint | QtCore.Qt.WindowSystemMenuHint )

        self.setSizeGripEnabled(True)
        layout = QtGui.QVBoxLayout(self)
        self.panel = FigurePanel(self)
        layout.addWidget(self.panel)

        properties = sourcefigure.getPropertiesCopy()
        self.panel.figure.sources = sourcefigure.sources
        self.panel.figure.defaultsource = sourcefigure.defaultsource
        self.panel.plotFromProperties(properties)

        title = self.panel.figure.properties.getProperty(['Title'])
        self.setWindowTitle(title)
        
        self.resize(500, 500)

class PageVisualize(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.result = parent.shared['result']

        self.treewidgetVars = QtGui.QTreeWidget(self)
        self.treewidgetVars.setColumnCount(1)
        self.treewidgetVars.header().hide()
        self.connect(self.treewidgetVars, QtCore.SIGNAL("itemSelectionChanged()"), self.OnVarSelected)
        self.treewidgetVars.setSizePolicy(QtGui.QSizePolicy.Minimum,QtGui.QSizePolicy.Expanding)
        self.treewidgetVars.setMaximumWidth(250)

        self.figurepanel = FigurePanel(self)

        self.buttonTakeOut = QtGui.QPushButton('&Detach figure',self.figurepanel)
        self.connect(self.buttonTakeOut, QtCore.SIGNAL('clicked()'), self.OnDetachFigure)
        self.figurepanel.layoutButtons.addWidget(self.buttonTakeOut)

        layout = QtGui.QHBoxLayout()
        layout.addWidget(self.treewidgetVars)
        layout.addWidget(self.figurepanel)
        self.setLayout(layout)

        self.BuildVariableTree()

        self.figurechildren = []

    def closeEvent(self,ev):
        for ch in self.figurechildren: ch.close()
        QtGui.QWidget.closeEvent(self,ev)

    def OnVarSelected(self):
        if len(self.treewidgetVars.selectedItems())==0: return
        it = self.treewidgetVars.selectedItems()[0]
        varid = it.data(0,QtCore.Qt.UserRole)
        if varid.isNull(): return
        varname = varid.toString()
        self.figurepanel.plot(self.result,varname)

    def OnDetachFigure(self):
        if len(self.treewidgetVars.selectedItems())==0: return
        it = self.treewidgetVars.selectedItems()[0]
        varid = it.data(0,QtCore.Qt.UserRole)
        if varid.isNull(): return
        fd = FigureDialog(self,sourcefigure=self.figurepanel.figure)
        fd.show()
        self.figurechildren.append(fd)

    def BuildVariableTree(self):
        self.treewidgetVars.clear()
        handler = self.TreeContentHandler(self.treewidgetVars,self.result)
        f = file(os.path.join(os.path.dirname(__file__),'varstructure.xml'), 'r')
        xml.sax.parse(f,handler)

    class TreeContentHandler(xml.sax.handler.ContentHandler):

        def __init__(self,treewidget,result):
            xml.sax.handler.ContentHandler.__init__(self)
            self.treewidget = treewidget
            self.dict = result.getVariableLongNames()
            self.curitem = self.treewidget
            self.dictFound = {}

        def startElement(self,name,attrs):
            if name=='variable':
                varid = attrs.getValue('id')
                if self.dict.has_key(varid):
                    varnode = QtGui.QTreeWidgetItem(self.curitem,[self.dict[varid]])
                    varnode.setData(0,QtCore.Qt.UserRole,QtCore.QVariant(varid))
                    self.dictFound[varid] = True
                else:
                    print 'Skipped variable '+varid+' because it is not present in result file.'
            elif name=='folder':
                self.curitem = QtGui.QTreeWidgetItem(self.curitem,[attrs.getValue('name')])
                self.curitem.setFlags(self.curitem.flags() & (~QtCore.Qt.ItemIsSelectable))

        def endElement(self,name):
            if name=='folder':
                self.curitem = self.curitem.parent()
                if self.curitem==None: self.curitem = self.treewidget

        def endDocument(self):
            # Find all variables that have not been added to the tree yet.
            # (because they were not known when the tree file was made)
            add = [];
            for varid in self.dict.keys():
                if not varid in self.dictFound: add.append(varid)

            # Add any left variables to the tree, below the node 'Other'
            if len(add)>0:
                # Add 'other' folder
                folderOther = QtGui.QTreeWidgetItem(self.treewidget,['Other'])
                folderOther.setFlags(folderOther.flags() & (~QtCore.Qt.ItemIsSelectable))

                # Sort variables by long name, case-insensitive.
                add.sort(lambda x,y: cmp(self.dict[x].lower(),self.dict[y].lower()))

                # Add remaining variables.
                for varid in add:
                    varnode = QtGui.QTreeWidgetItem(folderOther,[self.dict[varid]])
                    varnode.setData(0,QtCore.Qt.UserRole,QtCore.QVariant(varid))

    def isComplete(self):
        return True

def main():
    # Debug info
    print 'Python version: '+str(sys.version_info)
    print 'PyQt4 version: '+QtCore.PYQT_VERSION_STR
    print 'Qt version: '+QtCore.qVersion()
    print 'xml version: '+xml.__version__

    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([' '])
    else:
        app = QtGui.qApp

    # Create wizard dialog
    wiz = commonqt.Wizard()
    wiz.setWindowTitle('Result visualizer')
    wiz.resize(800, 600)

    seq = [PageOpen,PageVisualize,PageSave,PageFinal]

    # Get NetCDF file to open from command line or from FileOpen dialog.
    if len(sys.argv)>1:
        result = None
        try:
            result = loadResult(sys.argv[1])
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
        if result!=None:
            seq.pop(0)
            wiz.shared['result'] = result
            wiz.shared['scenario'] = result.scenario

    seq = commonqt.WizardSequence(seq)
    wiz.setSequence(seq)
    wiz.show()

    ret = app.exec_()
    page = None

    wiz.unlink()

    sys.exit(ret)

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
