#!/usr/bin/python

#$Id: scenariobuilder.py,v 1.17 2007-07-06 13:48:29 jorn Exp $

from PyQt4 import QtGui,QtCore

import scenario, data, commonqt
import sys,xml, os.path

class ScenarioWidget(QtGui.QWidget):

    def __init__(self,parent=None,mrupaths=[]):
        QtGui.QWidget.__init__(self,parent)

        self.bngroup      = QtGui.QButtonGroup()
        self.radioNew     = QtGui.QRadioButton('Create a new scenario from a template.',self)
        self.radioOpen    = QtGui.QRadioButton('Open an existing scenario.',self)
        self.radioImport1 = QtGui.QRadioButton('Import a namelist-based scenario from an existing directory.',self)
        self.radioImport2 = QtGui.QRadioButton('Import a namelist-based scenario from a tar/gz archive.',self)

        self.labTemplate = QtGui.QLabel('Template:',self)
        default2path = scenario.Scenario.defaultname2path()
        self.comboTemplates = QtGui.QComboBox(parent)
        for (name,path) in default2path.items():
            self.comboTemplates.addItem(name,QtCore.QVariant(name))
        #self.comboTemplates.setSizePolicy(QtGui.QSizePolicy.Fixed,QtGui.QSizePolicy.Fixed)
        self.templatelayout = QtGui.QHBoxLayout()
        self.templatelayout.addWidget(self.labTemplate)
        self.templatelayout.addWidget(self.comboTemplates,1)
        self.templatelayout.addStretch()

        self.pathOpen    = commonqt.PathEditor(self,header='File to open: ',mrupaths=mrupaths)
        self.pathImport1 = commonqt.PathEditor(self,header='Directory to import: ',getdirectory=True)
        self.pathImport2 = commonqt.PathEditor(self,header='Archive to import: ')

        self.pathOpen.filter    = 'GOTM scenario files (*.gotmscenario);;GOTM result files (*.gotmresult);;dataless GOTM scenario files (*.xml);;All files (*.*)'
        self.pathImport2.filter = 'tar/gz files (*.tar.gz);;All files (*.*)'
        
        self.bngroup.addButton(self.radioNew,    0)
        self.bngroup.addButton(self.radioOpen,   1)
        self.bngroup.addButton(self.radioImport1,2)
        self.bngroup.addButton(self.radioImport2,3)

        layout = QtGui.QGridLayout()
        layout.addWidget(self.radioNew,       0,0,1,2)
        layout.addLayout(self.templatelayout, 1,1)
        layout.addWidget(self.radioOpen,      2,0,1,2)
        layout.addWidget(self.pathOpen,       3,1)
        layout.addWidget(self.radioImport1,   4,0,1,2)
        layout.addWidget(self.pathImport1,    5,1)
        layout.addWidget(self.radioImport2,   6,0,1,2)
        layout.addWidget(self.pathImport2,    7,1)

        layout.setColumnStretch(1,1)

        layout.setColumnMinimumWidth(0,commonqt.getRadioWidth())

        layout.setMargin(0)
        
        self.setLayout(layout)

        self.connect(self.bngroup,     QtCore.SIGNAL('buttonClicked(int)'), self.onSourceChange)
        self.connect(self.pathOpen,    QtCore.SIGNAL('onChanged()'),        self.completeStateChanged)
        self.connect(self.pathImport1, QtCore.SIGNAL('onChanged()'),        self.completeStateChanged)
        self.connect(self.pathImport2, QtCore.SIGNAL('onChanged()'),        self.completeStateChanged)

        self.radioNew.setChecked(True)
        self.onSourceChange()
        
    def setPath(self,path):
        self.radioOpen.click()
        self.pathOpen.setPath(path)

    def onSourceChange(self):
        self.setUpdatesEnabled(False)
        checkedid = self.bngroup.checkedId()
        self.labTemplate.setVisible(checkedid==0)
        self.comboTemplates.setVisible(checkedid==0)
        self.pathOpen.setVisible(checkedid==1)
        self.pathImport1.setVisible(checkedid==2)
        self.pathImport2.setVisible(checkedid==3)
        self.completeStateChanged()
        self.setUpdatesEnabled(True)

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
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        try:
            checkedid = self.bngroup.checkedId()
            if   checkedid==0:
                index = self.comboTemplates.currentIndex()
                defname = unicode(self.comboTemplates.itemData(index).toString())
                defscenario = scenario.Scenario.getDefault(defname,scenario.guiscenarioversion)
                xmldom = defscenario.toXmlDom()
                scen = scenario.Scenario.fromSchemaName(scenario.guiscenarioversion)
                scen.setStore(xmldom)
            elif checkedid==1:
                path = self.pathOpen.path()
                if path.endswith('.gotmresult'):
                    try:
                        result = data.Result()
                        result.load(path)
                    except Exception,e:
                        raise Exception('An error occurred while loading the result: '+str(e))
                    scen = result.scenario.addref()  # Note: the scenario version will be guiscenarioversion, set by Result
                    result.release()
                elif path.endswith('.xml'):
                    try:
                        scen = scenario.Scenario.fromSchemaName(scenario.guiscenarioversion)
                        scen.load(path)
                    except Exception,e:
                        raise Exception('An error occurred while loading the scenario: '+str(e))
                else:
                    try:
                        scen = scenario.Scenario.fromSchemaName(scenario.guiscenarioversion)
                        scen.loadAll(path)
                    except Exception,e:
                        raise Exception('An error occurred while loading the scenario: '+str(e))
            elif checkedid==2:
                try:
                    scen = scenario.Scenario.fromNamelists(self.pathImport1.path(),strict = False)
                except Exception,e:
                    raise Exception('Cannot parse namelist files. Error: '+str(e))
            elif checkedid==3:
                try:
                    scen = scenario.Scenario.fromNamelists(self.pathImport2.path(),strict = False)
                except Exception,e:
                    raise Exception('Cannot parse namelist files. Error: '+str(e))

        finally:
            QtGui.QApplication.restoreOverrideCursor()

        if checkedid!=0:
            # We have loaded a scenario from file. Look for empty nodes and reset these to their defaults.
            #emptynodes = scen.root.getEmptyNodes()
            emptynodes = [n for n in scen.root.getEmptyNodes() if not n.isHidden()]
            for node in emptynodes:
                assert node.getDefaultValue()!=None, 'No value set for "%s", but no default value is available.' % node
            emptycount = len(emptynodes)
            if emptycount>0:
                QtGui.QMessageBox.information(self,'Scenario is incomplete','In this scenario %i variables do not have a value. These will be set to their default value.' % emptycount,QtGui.QMessageBox.Ok)
                scen.changed = True
            
        return scen

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
        self.owner.setProperty('result',None)
        self.owner.setProperty('scenario',newscen)
        return True

class ScenarioPage(commonqt.WizardPage):
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.getProperty('scenario')
        if self.scenario==None: raise Exception('No scenario available; this page should not have been available.')

        self.factory = commonqt.PropertyEditorFactory(self.scenario,live=True,allowhide=True)

    def saveData(self,mustbevalid):
        if self.factory.hasChanged():
            if not mustbevalid:
                res = QtGui.QMessageBox.question(self,'Your scenario has changed','Do you want to preserve your changes?',QtGui.QMessageBox.Yes,QtGui.QMessageBox.No,QtGui.QMessageBox.NoButton)
                if res==QtGui.QMessageBox.No: return True

            self.factory.updateStore()

        self.factory.unlink()
        
        return True

    def isComplete(self):
        return True

class PageLocation(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        titlelayout = QtGui.QHBoxLayout()
        self.lineTitle = self.factory.createEditor(['title'],self)
        self.labTitle = self.lineTitle.createLabel()
        titlelayout.addWidget(self.labTitle)
        titlelayout.addWidget(self.lineTitle.editor)

        groupboxLocation = QtGui.QGroupBox('Geographic location',self)

        loclayout = QtGui.QGridLayout()
        self.lineName      = self.factory.createEditor(['station','name'],     self)
        self.lineLongitude = self.factory.createEditor(['station','longitude'],self)
        self.lineLatitude  = self.factory.createEditor(['station','latitude'], self)
        self.lineDepth     = self.factory.createEditor(['station','depth'],    self)
        self.lineName.addToGridLayout(loclayout)
        self.lineLongitude.addToGridLayout(loclayout)
        self.lineLatitude.addToGridLayout(loclayout)
        self.lineDepth.addToGridLayout(loclayout)
        loclayout.setColumnStretch(3,1)
        groupboxLocation.setLayout(loclayout)

        groupboxPeriod = QtGui.QGroupBox('Simulated period',self)

        periodlayout = QtGui.QGridLayout()
        self.lineStart = self.factory.createEditor(['time','start'],self)
        self.lineStop  = self.factory.createEditor(['time','stop'] ,self)
        self.lineStart.addToGridLayout(periodlayout)
        self.lineStop.addToGridLayout(periodlayout)
        periodlayout.setColumnStretch(3,1)
        groupboxPeriod.setLayout(periodlayout)

        layout = QtGui.QVBoxLayout()
        layout.setSpacing(25)
        layout.addLayout(titlelayout)
        layout.addWidget(groupboxLocation)
        layout.addWidget(groupboxPeriod)
        layout.addStretch()
        self.setLayout(layout)

class PageDiscretization(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        groupboxGrid = QtGui.QGroupBox('Column structure',self)

        gridlayout = QtGui.QGridLayout()

        # Create controls for grid layout.
        self.bngroup = self.factory.createEditor(['grid','grid_method'],self,selectwithradio=True).editor
        self.editZoomSurface = self.factory.createEditor(['grid','ddu'],self)      
        self.editZoomBottom  = self.factory.createEditor(['grid','ddl'],self)
        self.editGridFile = self.factory.createEditor(['grid','grid_file'],self)
        
        # Add controls for grid layout to the widget.
        gridlayout.addWidget(self.bngroup.button(0), 0,0,1,4)
        self.editZoomSurface.addToGridLayout(gridlayout,1,1)
        self.editZoomBottom.addToGridLayout(gridlayout,2,1)
        gridlayout.addWidget(self.bngroup.button(1),3,0,1,4)
        gridlayout.addWidget(self.bngroup.button(2), 4,0,1,4)
        gridlayout.addWidget(self.editGridFile.editor, 5,1)
        groupboxGrid.setLayout(gridlayout)

        gridlayout.setColumnStretch(3,1)
        gridlayout.setColumnMinimumWidth(0,commonqt.getRadioWidth())

        groupboxTime = QtGui.QGroupBox('Time discretization',self)
        timelayout = QtGui.QGridLayout()
        self.editTimeStep = self.factory.createEditor(['timeintegration','dt'],self)
        self.editTimeStep.addToGridLayout(timelayout)
        groupboxTime.setLayout(timelayout)

        layout = QtGui.QVBoxLayout()
        layout.setSpacing(25)
        layout.addWidget(groupboxGrid)
        layout.addWidget(groupboxTime)
        layout.addStretch()
        self.setLayout(layout)

class PageSalinity(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        layout = QtGui.QGridLayout()
        self.editSalinity = self.factory.createEditor(['obs','sprofile'],self,selectwithradio=True)
        self.bngrpSalinity = self.editSalinity.editor
        self.labelSalinity = QtGui.QLabel(self.editSalinity.node.getText(detail=2),self)
        self.labelSalinity.setWordWrap(True)

        self.editSalinityConstant       = self.factory.createEditor(['obs','sprofile','s_const'],self)
        self.editSalinityUpperThickness = self.factory.createEditor(['obs','sprofile','z_s1'],self)
        self.editSalinityUpper          = self.factory.createEditor(['obs','sprofile','s_1'],self)
        self.editSalinityLowerThickness = self.factory.createEditor(['obs','sprofile','z_s2'],self)
        self.editSalinityLower          = self.factory.createEditor(['obs','sprofile','s_2'],self)
        self.editSalinitySurface        = self.factory.createEditor(['obs','sprofile','s_surf'],self)
        self.editSalinityNSquare        = self.factory.createEditor(['obs','sprofile','s_obs_NN'],self)
        self.editSalinityFile           = self.factory.createEditor(['obs','sprofile','s_prof_file'],self)
        
        self.editSalinityRelaxation     = self.factory.createEditor(['obs','sprofile','SRelax'],self,boolwithcheckbox=True)
        self.editSalinityBulk           = self.factory.createEditor(['obs','sprofile','SRelax','SRelaxTauM'],self)
        
        layout.addWidget(self.labelSalinity,               0,0,1,5)
        layout.addWidget(self.bngrpSalinity.button(0),     1,0,1,5)
        
        layout.addWidget(self.bngrpSalinity.button(11),    2,0,1,5)

        constlayout = QtGui.QGridLayout()
        self.editSalinityConstant.addToGridLayout(constlayout)
        constlayout.setColumnStretch(3,1)
        layout.addLayout(constlayout,3,1)
        
        layout.addWidget(self.bngrpSalinity.button(12),        4,0,1,5)
        
        layerlayout = QtGui.QGridLayout()
        self.editSalinityUpperThickness.addToGridLayout(layerlayout)
        self.editSalinityUpper.addToGridLayout(layerlayout)
        self.editSalinityLowerThickness.addToGridLayout(layerlayout)
        self.editSalinityLower.addToGridLayout(layerlayout)
        layerlayout.setColumnStretch(3,1)
        layout.addLayout(layerlayout,5,1)
        
        layout.addWidget(self.bngrpSalinity.button(13),    6,0,1,5)

        nsquarelayout = QtGui.QGridLayout()
        self.editSalinitySurface.addToGridLayout(nsquarelayout)
        self.editSalinityNSquare.addToGridLayout(nsquarelayout)
        nsquarelayout.setColumnStretch(3,1)
        layout.addLayout(nsquarelayout,7,1)
        
        layout.addWidget(self.bngrpSalinity.button(2),     8,0,1,5)
        
        filelayout = QtGui.QHBoxLayout()
        filelayout.addWidget(self.editSalinityFile.editor)
        filelayout.addStretch()
        layout.addLayout(filelayout,9,1)

        layout.setRowMinimumHeight(10,25)

        layout.addWidget(self.editSalinityRelaxation.editor,11,0,1,5)

        relaxationlayout = QtGui.QGridLayout()
        self.editSalinityBulk.addToGridLayout(relaxationlayout)
        layout.addLayout(relaxationlayout,12,1)

        layout.setColumnStretch(2,1)
        layout.setRowStretch(13,1)
        layout.setColumnMinimumWidth(0,commonqt.getRadioWidth())
        
        self.setLayout(layout)

class PageAdvanced(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.getProperty('scenario')
        if self.scenario==None: raise Exception('No scenario available; this page should not have been available.')
        
        self.model = commonqt.PropertyStoreModel(self.scenario,nohide=False)

        self.tree = commonqt.ExtendedTreeView(self)
        #self.tree.header().hide()
        self.delegate = commonqt.PropertyDelegate()
        self.tree.setItemDelegate(self.delegate)
        self.tree.setModel(self.model)
        self.tree.setExpandedAll(maxdepth=1)
        self.tree.expandNonDefaults()

        lab = QtGui.QLabel('Here you can change all properties of your scenario. Values that differ from the default are shown in bold; they can be reset to their default through the context menu that opens after clicking the right mousebutton.',self)
        lab.setWordWrap(True)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(lab)
        layout.addWidget(self.tree)
        self.setLayout(layout)

        self.connect(self.model, QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),self.completeStateChanged)

    def showEvent(self,event):
        self.tree.header().resizeSection(0,.65*self.tree.width())

    def isComplete(self):
        return True

    def saveData(self,mustbevalid):
        if mustbevalid:
            # Find used file nodes that have not been supplied with data.
            filenodes = []
            for fn in self.scenario.root.getNodesByType('file'):
                if fn.isHidden(): continue
                value = fn.getValueOrDefault()
                if value==None or not value.isValid(): filenodes.append(fn)
            if len(filenodes)>0:
                vartext = '\n'.join([fn.getText(2) for fn in filenodes])
                QtGui.QMessageBox.critical(self,'Scenario is incomplete','The following variables will be used in the simulation, but have not been set:\n\n%s\n\nEither configure the scenario to not use these variables, or supply them with data.' % vartext,QtGui.QMessageBox.Ok,QtGui.QMessageBox.NoButton)
                return False

            # Find used nodes that have not been set, and lack a default value.
            errornodes = []
            for node in self.scenario.root.getEmptyNodes():
                if node.isHidden(): continue
                if node.getDefaultValue()==None: errornodes.append(node)
            if len(errornodes)>0:
                vartext = '\n'.join([node.getText(2) for node in errornodes])
                QtGui.QMessageBox.critical(self,'Scenario is incomplete','The following variables will be used in the simulation, but have not been set to a value:\n\n%s\n\nPlease set these variables to a value first.' % vartext,QtGui.QMessageBox.Ok,QtGui.QMessageBox.NoButton)
                return False
        return True
    
class PageSave(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.getProperty('scenario')

        self.label = QtGui.QLabel('The scenario has been modified. Do you want to save it?',self)
        self.bngroup     = QtGui.QButtonGroup()
        self.radioNoSave = QtGui.QRadioButton('No, I do not want to save the modified scenario.', parent)
        self.radioSave   = QtGui.QRadioButton('Yes, I want to save the scenario to file.', parent)

        self.pathSave = commonqt.PathEditor(self,header='File to save to: ',save=True)
        self.pathSave.filter = 'GOTM scenario files (*.gotmscenario);;All files (*.*)'

        if self.scenario.path!=None:
            self.pathSave.setPath(self.scenario.path)

        self.bngroup.addButton(self.radioNoSave, 0)
        self.bngroup.addButton(self.radioSave,   1)

        layout = QtGui.QGridLayout()
        layout.addWidget(self.label,0,0,1,2)
        layout.addWidget(self.radioNoSave,1,0,1,2)
        layout.addWidget(self.radioSave,2,0,1,2)
        layout.addWidget(self.pathSave,3,1,1,1)

        layout.setRowStretch(4,1)
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
                self.scenario.saveAll(targetpath)
            except Exception,e:
                QtGui.QMessageBox.critical(self, 'Unable to save scenario', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False
            self.owner.settings.addUniqueValue(('Paths','RecentScenarios'),'Path',targetpath)
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
        commonqt.WizardSequence.__init__(self,[PageLocation,PageDiscretization,PageSalinity,PageAdvanced,PageSave])

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
