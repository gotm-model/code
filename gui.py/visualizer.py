#!/usr/bin/python

#$Id: visualizer.py,v 1.36 2008-04-30 09:46:22 jorn Exp $

from PyQt4 import QtGui,QtCore

import xmlstore.gui_qt4, xmlplot.gui_qt4
import core.common, core.result, core.report, commonqt

import sys,datetime
import xml.sax
import os.path

def loadResult(path):
    res = core.result.Result()

    try:
        if path.endswith('.gotmresult'):
            res.load(path)
        elif path.endswith('.nc'):
            res.attach(path)
        else:
            # We do not recognize this file type; try both GOTM result and NetCDF
            done = True
            try:
                res.attach(path)
            except Exception,e:
                done = False
            if not done:
                done = True
                try:
                    res.load(path)
                except Exception,e:
                    done = False
            if (not done):
                raise Exception('The file "%s" is not a GOTM result or a NetCDF file.' % path)
    except:
        res.release()
        res = None
        raise

    return res

class OpenWidget(QtGui.QWidget):
    def __init__(self,parent=None,mrupaths=[]):
        QtGui.QWidget.__init__(self,parent)

        self.pathOpen = commonqt.PathEditor(self,header='File to open: ',mrupaths=mrupaths)
        self.pathOpen.filter = 'GOTM result files (*.gotmresult);;NetCDF files (*.nc);;All files (*.*)'

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.pathOpen)
        layout.setMargin(0)
        self.setLayout(layout)
        self.connect(self.pathOpen, QtCore.SIGNAL('onChanged()'), self.completeStateChanged)
        
    def setPath(self,path):
        self.pathOpen.setPath(path)
        
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
            res = self.openwidget.getResult()
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            return False
        self.owner.setProperty('result',res)
        self.owner.setProperty('scenario',res.scenario.addref())
        return True

class PageVisualize(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.varpath = None
        self.varname = None

        self.result = parent.getProperty('result')
        
        self.treestore = self.result.getVariableTree(os.path.join(core.common.getDataRoot(),'schemas/outputtree.xml'))
        self.model = xmlstore.gui_qt4.TypedStoreModel(self.treestore,nohide=False,novalues=True)

        self.treeVariables = xmlstore.gui_qt4.ExtendedTreeView(self)
        self.treeVariables.header().hide()
        self.treeVariables.setSizePolicy(QtGui.QSizePolicy.Minimum,QtGui.QSizePolicy.Expanding)
        self.treeVariables.setMaximumWidth(250)
        self.treeVariables.setModel(self.model)
        self.connect(self.treeVariables.selectionModel(), QtCore.SIGNAL('selectionChanged(const QItemSelection &, const QItemSelection &)'), self.OnVarSelected)

        self.figurepanel = xmlplot.gui_qt4.FigurePanel(self)

        self.label = QtGui.QLabel('Here you can view the results of the simulation. Please choose a variable to be plotted from the menu.',self)

        layout = QtGui.QGridLayout()
        layout.addWidget(self.label,0,0,1,2)
        layout.addWidget(self.treeVariables,1,0)
        layout.addWidget(self.figurepanel,1,1)
        self.setLayout(layout)
        
        self.defaultfigures = parent.settings.root['FigureSettings']

        self.figurepanel.figure.addDataSource('result',self.result)

    def OnVarSelected(self,*args):
        selected = self.treeVariables.selectionModel().selectedIndexes()
        if len(selected)==0: return
        node = selected[0].internalPointer()
        if node.hasChildren(): return
        
        # Show wait cursor
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        
        # Save settings for currently shown figure
        self.saveFigureSettings()

        # Get name and path of variable about to be shown.
        self.varname = node.getId()
        self.varpath = '/'.join(node.location)
        
        # Disable figure updating while we make changes.
        self.figurepanel.figure.setUpdating(False)
        
        # Plot; first try stored figures, otherwise plot anew.
        props = self.figurepanel.figure.properties
        if not self.result.getFigure('result/'+self.varpath,props):
            self.figurepanel.plot(self.varname,'result')
        
        # Re-enable figure updating (this will force a redraw because things changed)
        self.figurepanel.figure.setUpdating(True)

        # Restore original cursor
        QtGui.QApplication.restoreOverrideCursor()

    def isComplete(self):
        return True

    def saveData(self,mustbevalid):
        self.saveFigureSettings()
        return True

    def destroy(self,destroyWindow = True,destroySubWindows = True):
        self.figurepanel.destroy()
        self.figurepanel = None
        self.treestore.release()
        self.treestore = None
        commonqt.WizardPage.destroy(self,destroyWindow,destroySubWindows)

    def saveFigureSettings(self):
        if self.varpath!=None and self.figurepanel.figure.hasChanged():
            self.result.setFigure('result/'+self.varpath,self.figurepanel.figure.properties)
            
    def onSaveAsDefault(self):
        pass

class ConfigureReportWidget(QtGui.QWidget):
    def __init__(self,parent,result,rep):
        QtGui.QWidget.__init__(self,parent)
        
        self.result = result
        self.report = rep

        self.factory = xmlstore.gui_qt4.PropertyEditorFactory(self.report.store)

        reportname2path = core.report.Report.getTemplates()

        self.labTemplates = QtGui.QLabel('Report template:',self)
        self.comboTemplates = QtGui.QComboBox(parent)
        for (name,path) in reportname2path.items():
            self.comboTemplates.addItem(name,QtCore.QVariant(path))
        
        self.labOutput = QtGui.QLabel('Directory to save to:',self)
        self.pathOutput = commonqt.PathEditor(self,getdirectory=True)
        
        # Default report directory: result or scenario directory
        if self.result.path!=None:
            self.pathOutput.defaultpath = os.path.dirname(self.result.path)
        elif self.result.scenario!=None and self.result.scenario.path!=None:
            self.pathOutput.defaultpath = os.path.dirname(self.result.scenario.path)

        self.labVariables = QtGui.QLabel('Included variables:',self)
        self.treestore = self.result.getVariableTree('schemas/outputtree.xml')
        
        # Prepare selection based on report settings
        selroot = self.report.store['Figures/Selection']
        for node in selroot.children:
            targetnode = self.treestore[node.getValue()]
            if targetnode!=None: targetnode.setValue(True)
        
        self.model = xmlstore.gui_qt4.TypedStoreModel(self.treestore,nohide=False,novalues=True,checkboxes=True)
        self.treeVariables = xmlstore.gui_qt4.ExtendedTreeView(self)
        self.treeVariables.header().hide()
        self.treeVariables.setModel(self.model)
        self.treeVariables.setSizePolicy(QtGui.QSizePolicy.Expanding,QtGui.QSizePolicy.Expanding)

        # Create labels+editors for figure settings
        editWidth = self.factory.createEditor('Figures/Width',self)
        editHeight = self.factory.createEditor('Figures/Height',self)
        editDpi = self.factory.createEditor('Figures/Resolution',self)
        editFontScaling = self.factory.createEditor('Figures/FontScaling',self)
        
        layout = QtGui.QGridLayout()
        layout.addWidget(self.labOutput,     0,0)
        layout.addWidget(self.pathOutput,    0,1)
        layout.addWidget(self.labTemplates,  1,0)
        layout.addWidget(self.comboTemplates,1,1)
        layout.addWidget(self.labVariables,  2,0,QtCore.Qt.AlignTop)
        layout.addWidget(self.treeVariables, 2,1)

        self.figbox = QtGui.QGroupBox('Figure settings',self)
        figlayout = QtGui.QGridLayout()
        editWidth.addToGridLayout(figlayout,0,0)
        editHeight.addToGridLayout(figlayout)
        editDpi.addToGridLayout(figlayout)
        editFontScaling.addToGridLayout(figlayout)
        figlayout.setColumnStretch(3,1)
        self.figbox.setLayout(figlayout)
        layout.addWidget(self.figbox,3,0,1,2)
        
        layout.setMargin(0)
        self.setLayout(layout)

        self.connect(self.pathOutput, QtCore.SIGNAL('onChanged()'), self.completeStateChanged)

    def completeStateChanged(self):
        self.emit(QtCore.SIGNAL('onCompleteStateChanged()'))

    def isComplete(self):
        return self.pathOutput.hasPath()

    def generate(self):
        # Get path of target directory and template.
        templateindex = self.comboTemplates.currentIndex()
        templatepath = unicode(self.comboTemplates.itemData(templateindex).toString())
        outputpath = self.pathOutput.path()

        # Warn if the target directory is not empty.
        if os.path.isdir(outputpath) and len(os.listdir(outputpath))>0:
            ret = QtGui.QMessageBox.warning(self,'Directory is not empty','The specified target directory ("%s") contains one or more files, which may be overwritten. Do you want to continue?' % outputpath,QtGui.QMessageBox.Yes,QtGui.QMessageBox.No)
            if ret==QtGui.QMessageBox.No: return False

        # Update the list of selected variables.
        selroot = self.report.store['Figures/Selection']
        selroot.removeAllChildren()
        for node in self.model.getCheckedNodes():
            if node.canHaveValue():
                ch = selroot.addChild('VariablePath')
                ch.setValue('/'.join(node.location))

        # Make changed report settings persistent
        self.factory.updateStore()

        # Generate the report and display the wait cursor while doing so.
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        self.report.generate(self.result,outputpath,templatepath,callback=self.onReportProgressed)
        QtGui.QApplication.restoreOverrideCursor()

        return True

    def onReportProgressed(self,progressed,status):
        self.emit(QtCore.SIGNAL('onReportProgressed'),progressed,status)

    def destroy(self,destroyWindow = True,destroySubWindows = True):
        self.factory.unlink()
        self.treeVariables.setModel(None)
        self.model.unlink()
        self.treestore.release()
        QtGui.QWidget.destroy(self,destroyWindow,destroySubWindows)
        
class PageReportGenerator(commonqt.WizardPage):
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.result = parent.getProperty('result')
        deffont = xmlplot.gui_qt4.getFontSubstitute(unicode(self.fontInfo().family()))
        self.report = core.report.Report(defaultfont = deffont)
        
        # Copy report settings from result.
        self.report.store.root.copyFrom(self.result.store['ReportSettings'],replace=True)

        self.label = QtGui.QLabel('You can generate a report that describes the scenario and the simulation results. A report consists of an HTML file, associated files (CSS, javascript) and image files for all figures.',self)
        self.label.setWordWrap(True)
        self.checkReport = QtGui.QCheckBox('Yes, I want to generate a report.', parent)
        self.reportwidget = ConfigureReportWidget(self,self.result,self.report)

        self.progressbar = QtGui.QProgressBar(self)
        self.progressbar.setRange(0,100)
        self.labStatus = QtGui.QLabel(self)
        self.progressbar.hide()
        self.labStatus.hide()

        layout = QtGui.QGridLayout()
        layout.addWidget(self.label,0,0,1,2)
        layout.addWidget(self.checkReport,1,0,1,2)
        layout.addWidget(self.reportwidget,2,1,1,1)

        layout.addWidget(self.progressbar,3,0,1,2)
        layout.addWidget(self.labStatus,4,0,1,2)

        layout.setRowStretch(5,1)
        layout.setColumnStretch(1,1)
        layout.setColumnMinimumWidth(0,commonqt.getRadioWidth())

        self.setLayout(layout)

        self.connect(self.checkReport, QtCore.SIGNAL('stateChanged(int)'),        self.onCheckChange)
        self.connect(self.reportwidget,QtCore.SIGNAL('onCompleteStateChanged()'), self.completeStateChanged)
        self.connect(self.reportwidget,QtCore.SIGNAL('onReportProgressed'),       self.reportProgressed)
        self.onCheckChange()

    def onCheckChange(self):
        self.reportwidget.setVisible(self.checkReport.isChecked())
        self.completeStateChanged()

    def isComplete(self):
        if not self.checkReport.isChecked(): return True
        return self.reportwidget.isComplete()

    def saveData(self,mustbevalid):
        if mustbevalid and self.checkReport.isChecked():
            ret = self.reportwidget.generate()
            if ret:
                self.result.store['ReportSettings'].copyFrom(self.report.store.root,replace=True)
            return ret
        return True

    def reportProgressed(self,progressed,status):
        if self.progressbar.isHidden():
            self.label.setText('Please wait while the report is created...')
            self.checkReport.hide()
            self.reportwidget.hide()
            self.progressbar.show()
            self.labStatus.show()
            self.repaint()
            
        self.progressbar.setValue(round(progressed*100))
        self.labStatus.setText(status)
        QtGui.qApp.processEvents()

    def destroy(self,destroyWindow = True,destroySubWindows = True):
        self.report.release()
        self.reportwidget.destroy(destroyWindow,destroySubWindows)
        commonqt.WizardPage.destroy(self,destroyWindow,destroySubWindows)

class PageSave(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.result = parent.getProperty('result')

        self.label = QtGui.QLabel('Do you want to save the result of your simulation?',self)
        self.bngroup     = QtGui.QButtonGroup()
        self.radioNoSave = QtGui.QRadioButton('No, I do not want to save the result.', parent)
        self.radioSave   = QtGui.QRadioButton('Yes, I want to save the result to file.', parent)

        self.pathSave = commonqt.PathEditor(self,header='File to save to: ',save=True)
        self.pathSave.filter = 'GOTM result files (*.gotmresult);;All files (*.*)'
        if self.result.path!=None: self.pathSave.setPath(self.result.path)

        self.checkboxAddFigures = QtGui.QCheckBox('Also save my figure settings.',self)
        self.checkboxAddFigures.setChecked(True)

        self.bngroup.addButton(self.radioNoSave, 0)
        self.bngroup.addButton(self.radioSave,   1)

        layout = QtGui.QGridLayout()
        layout.addWidget(self.label,      0,0,1,2)
        layout.addWidget(self.radioNoSave,1,0,1,2)
        layout.addWidget(self.radioSave,  2,0,1,2)
        layout.addWidget(self.pathSave,   3,1,1,1)
        layout.addWidget(self.checkboxAddFigures,4,1,1,1)

        layout.setRowStretch(5,1)
        layout.setColumnStretch(1,1)
        layout.setColumnMinimumWidth(0,commonqt.getRadioWidth())

        self.setLayout(layout)

        self.connect(self.bngroup,  QtCore.SIGNAL('buttonClicked(int)'), self.onSourceChange)
        self.connect(self.pathSave, QtCore.SIGNAL('onChanged()'),        self.completeStateChanged)

        self.radioSave.setChecked(True)
        self.onSourceChange()

    def onSourceChange(self):
        checkedid = self.bngroup.checkedId()
        self.pathSave.setVisible(checkedid==1)
        self.checkboxAddFigures.setVisible(checkedid==1)
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
            targetpath = self.pathSave.path()
            if os.path.isfile(targetpath):
                ret = QtGui.QMessageBox.warning(self, 'Overwrite existing file?', 'There already exists a file at the specified location. Overwrite it?', QtGui.QMessageBox.Yes, QtGui.QMessageBox.No)
                if ret==QtGui.QMessageBox.No:
                    return False
            try:
                self.result.save(targetpath,addfiguresettings=self.checkboxAddFigures.isChecked())
                self.owner.settings.addUniqueValue('Paths/RecentResults','Path',targetpath)
            except Exception,e:
                print e
                QtGui.QMessageBox.critical(self, 'Unable to save result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False
        return True

    def doNotShow(self):
        return (not self.result.hasChanged())

class PageFinal(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('You are now done. Click the "Home" button below to work with another scenario or result.',self)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addStretch()
        self.setLayout(layout)

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

    seq = [PageOpen,PageChooseAction,PageVisualize,PageReportGenerator,PageSave,PageFinal]

    # Get NetCDF file to open from command line or from FileOpen dialog.
    if len(sys.argv)>1:
        res = None
        try:
            res = loadResult(sys.argv[1])
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to load result', unicode(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
        if res!=None:
            seq.pop(0)
            wiz.setProperty('result',res)
            wiz.setProperty('scenario',res.scenario.addref())

    seq = commonqt.WizardSequence(seq)
    wiz.setSequence(seq)
    wiz.show()

    ret = app.exec_()
    page = None

    wiz.unlink()

    sys.exit(ret)

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
