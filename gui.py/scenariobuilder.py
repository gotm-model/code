#!/usr/bin/python

#$Id: scenariobuilder.py,v 1.2 2006-12-04 08:03:10 jorn Exp $

from PyQt4 import QtGui,QtCore

import common,commonqt
import sys,xml

class ScenarioWidget(QtGui.QWidget):

    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)

        self.bngroup     = QtGui.QButtonGroup()
        self.radioNew    = QtGui.QRadioButton('Create a new scenario based on a template.', parent)
        self.radioOpen   = QtGui.QRadioButton('Open an existing scenario.', parent)
        self.radioImport1 = QtGui.QRadioButton('Import a GOTM scenario from an existing directory.', parent)
        self.radioImport2 = QtGui.QRadioButton('Import a GOTM scenario from a tar/gz archive.', parent)

        self.pathOpen    = commonqt.PathEditor(self,header='File to open: ')
        self.pathImport1 = commonqt.PathEditor(self,header='Directory to import: ',getdirectory=True)
        self.pathImport2 = commonqt.PathEditor(self,header='Archive to import: ')

        self.pathOpen.filter    = 'GOTM scenario files (*.gotmscenario);;GOTM result files (*.gotmresult);;All files (*.*)'
        self.pathImport2.filter = 'tar/gz files (*.tar.gz);;All files (*.*)'
        
        self.bngroup.addButton(self.radioNew,    0)
        self.bngroup.addButton(self.radioOpen,   1)
        self.bngroup.addButton(self.radioImport1,2)
        self.bngroup.addButton(self.radioImport2,3)

        layout = QtGui.QGridLayout()
        layout.addWidget(self.radioNew,    0,0)
        layout.addWidget(self.radioOpen,   1,0)
        layout.addWidget(self.pathOpen,    2,0)
        layout.addWidget(self.radioImport1,3,0)
        layout.addWidget(self.pathImport1, 4,0)
        layout.addWidget(self.radioImport2,5,0)
        layout.addWidget(self.pathImport2, 6,0)
        self.setLayout(layout)

        self.connect(self.bngroup,     QtCore.SIGNAL("buttonClicked(int)"), self.onSourceChange)
        self.connect(self.pathOpen,    QtCore.SIGNAL("onChanged()"),        self.completeStateChanged)
        self.connect(self.pathImport1, QtCore.SIGNAL("onChanged()"),        self.completeStateChanged)
        self.connect(self.pathImport2, QtCore.SIGNAL("onChanged()"),        self.completeStateChanged)

        self.radioNew.setChecked(True)
        self.onSourceChange()

    def onSourceChange(self):
        checkedid = self.bngroup.checkedId()
        self.pathOpen.setVisible(checkedid==1)
        self.pathImport1.setVisible(checkedid==2)
        self.pathImport2.setVisible(checkedid==3)
        self.completeStateChanged()

    def isComplete(self):
        checkedid = self.bngroup.checkedId()
        if   checkedid==0:
            return True
        elif checkedid==1:
            return self.pathOpen.hasPath()
        elif checkedid==2:
            return self.pathImport1.hasPath()
        elif checkedid==3:
            return self.pathImport2.hasPath()

    def getScenario(self):
        if not self.isComplete(): return None
        checkedid = self.bngroup.checkedId()
        if   checkedid==0:
            scenario = common.Scenario(templatename=common.guiscenarioversion)
        elif checkedid==1:
            path = self.pathOpen.path()
            if path.endswith('.gotmresult'):
                try:
                    result = common.Result()
                    result.load(path)
                except Exception,e:
                    raise Exception('An error occurred while loading the result: '+str(e))
                scenario = result.scenario
                result.unlink()
            else:
                try:
                    scenario = common.Scenario(templatename=common.guiscenarioversion)
                    scenario.loadAll(path)
                except Exception,e:
                    raise Exception('An error occurred while loading the scenario: '+str(e))
        elif checkedid==2:
            try:
                scenario = common.Scenario.fromNamelists(self.pathImport1.path())
            except Exception,e:
                raise Exception('Cannot parse namelist files. Error: '+str(e))
        elif checkedid==3:
            try:
                scenario = common.Scenario.fromNamelists(self.pathImport2.path())
            except Exception,e:
                raise Exception('Cannot parse namelist files. Error: '+str(e))
        return scenario

    def completeStateChanged(self):
        self.emit(QtCore.SIGNAL('onCompleteStateChanged()'))

class PageOpen(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('How do you want to obtain a scenario?',self)
        self.scenariowidget = ScenarioWidget(self)
        self.connect(self.scenariowidget, QtCore.SIGNAL("onCompleteStateChanged()"),self.completeStateChanged)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addWidget(self.scenariowidget)
        layout.addStretch()
        self.setLayout(layout)

    def isComplete(self):
        return self.scenariowidget.isComplete()

    def saveData(self,mustbevalid):
        try:
            newscen = self.scenariowidget.getScenario()
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to obtain scenario', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            return False
        if 'result' in self.parent().shared:
            result = self.parent().shared.pop('result')
            result.unlink()
        if 'scenario' in self.parent().shared:
            oldscen = self.parent().shared['scenario']
            if oldscen!=None: oldscen.unlink()
        self.parent().shared['scenario'] = newscen
        return True

class PageLocation(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.shared['scenario']
        if self.scenario==None: raise Exception('No scenario available; this page should not have been available.')

        groupbox1 = QtGui.QGroupBox('Geographic location',self)

        self.factory = commonqt.PropertyEditorFactory(self.scenario)

        loclayout = QtGui.QGridLayout()
        self.labName       = QtGui.QLabel('Name: ',self)
        self.labLongitude  = QtGui.QLabel('Longitude: ',self)
        self.labLatitude   = QtGui.QLabel('Latitude: ',self)
        self.labDepth      = QtGui.QLabel('Water depth: ',self)
        self.lineName      = self.factory.createEditor(['station','name'],self)
        self.lineLongitude = self.factory.createEditor(['station','longitude'],self)
        self.lineLatitude  = self.factory.createEditor(['station','latitude' ],self)
        self.lineDepth     = self.factory.createEditor(['station','depth'    ],self)
        loclayout.addWidget(self.labName, 0,0)
        loclayout.addWidget(self.lineName.editor,0,1)
        loclayout.addWidget(self.labLongitude, 1,0)
        loclayout.addWidget(self.lineLongitude.editor,1,1)
        loclayout.addWidget(self.labLatitude, 2,0)
        loclayout.addWidget(self.lineLatitude.editor,2,1)
        loclayout.addWidget(self.labDepth, 3,0)
        loclayout.addWidget(self.lineDepth.editor,3,1)
        groupbox1.setLayout(loclayout)

        groupbox2 = QtGui.QGroupBox('Simulated period',self)

        periodlayout = QtGui.QGridLayout()
        self.labStart  = QtGui.QLabel('Start date: ',self)
        self.labStop   = QtGui.QLabel('Stop date: ',self)
        self.lineStart = self.factory.createEditor(['time','start'],self)
        self.lineStop  = self.factory.createEditor(['time','stop'] ,self)
        periodlayout.addWidget(self.labStart, 0,0)
        periodlayout.addWidget(self.lineStart.editor,0,1)
        periodlayout.addWidget(self.labStop, 1,0)
        periodlayout.addWidget(self.lineStop.editor,1,1)
        groupbox2.setLayout(periodlayout)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(groupbox1)
        layout.addWidget(groupbox2)
        layout.addStretch()
        self.setLayout(layout)

    def saveData(self,mustbevalid):
        if not self.factory.hasChanged(): return True
        
        if not mustbevalid:
            res = QtGui.QMessageBox.question(self,'Your scenario has changed','Do you want to preserve your changes?',QtGui.QMessageBox.Yes,QtGui.QMessageBox.No,QtGui.QMessageBox.NoButton)
            if res==QtGui.QMessageBox.No: return True

        self.factory.updateStore()
        return True

    def isComplete(self):
        return True

class PageAdvanced(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.shared['scenario']
        if self.scenario==None: raise Exception('No scenario available; this page should not have been available.')
        
        self.model = commonqt.PropertyStoreModel(self.scenario,nohide=False)

        self.tree = commonqt.ExtendedTreeView(self)
        self.delegate = commonqt.PropertyDelegate()
        self.tree.setItemDelegate(self.delegate)
        self.tree.setModel(self.model)
        self.tree.setExpandedAll(maxdepth=1)

        lab = QtGui.QLabel('Here you can change all properties of the GOTM scenario. Currently this still uses the GOTM namelist structure. This will change in the future; we will move to a more intuitive structure.',self)
        lab.setWordWrap(True)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(lab)
        layout.addWidget(self.tree)
        self.setLayout(layout)

        self.connect(self.model, QtCore.SIGNAL("dataChanged(const QModelIndex&,const QModelIndex&)"),self.completeStateChanged)

    def showEvent(self,event):
        self.tree.header().resizeSection(0,.65*self.tree.width())

    def isComplete(self):
        return True
    
class PageSave(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.shared['scenario']

        self.label = QtGui.QLabel('The scenario has been modified. Do you want to save it?',self)
        self.bngroup     = QtGui.QButtonGroup()
        self.radioNoSave = QtGui.QRadioButton('No, I do not want to save the modified scenario.', parent)
        self.radioSave   = QtGui.QRadioButton('Yes, I want to save the scenario to file.', parent)

        self.pathSave = commonqt.PathEditor(self,header='File to save to: ',save=True)
        self.pathSave.filter = 'GOTM scenario files (*.gotmscenario);;All files (*.*)'
        self.pathSave.forcedextension = '.gotmscenario'

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
                self.scenario.saveAll(self.pathSave.path())
            except Exception,e:
                QtGui.QMessageBox.critical(self, 'Unable to save scenario', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False
        return True

    def doNotShow(self):
        return (not self.scenario.hasChanged())

class PageFinal(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('Your scenario is now complete.',self)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addStretch()
        self.setLayout(layout)

    def isComplete(self):
        return True

class SequenceEditScenario(commonqt.WizardSequence):
    def __init__(self):
        commonqt.WizardSequence.__init__(self,[PageLocation,PageAdvanced,PageSave])

def main():
    # Debug info
    print 'Python version: '+str(sys.version_info)
    print 'PyQt4 version: '+QtCore.PYQT_VERSION_STR
    print 'Qt version: '+QtCore.qVersion()
    print 'xml version: '+xml.__version__

    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([" "])
    else:
        app = QtGui.qApp

    # Create wizard dialog
    wiz = commonqt.Wizard()
    wiz.setWindowTitle('Scenario builder')
    wiz.resize(800, 600)

    seq = commonqt.WizardSequence([PageOpen,SequenceEditScenario(),PageFinal])
    wiz.setSequence(seq)
    wiz.show()

    ret = app.exec_()
    page = None

    wiz.unlink()

    sys.exit(ret)

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
