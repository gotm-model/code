#!/usr/bin/python

#$Id: scenariobuilder.py,v 1.50 2009-08-06 13:22:48 jorn Exp $

from PyQt4 import QtGui,QtCore

# Import modules from standard Python library
import sys,xml, os.path

import core.scenario, core.result, xmlstore.util, xmlstore.gui_qt4, commonqt

class ScenarioWidget(QtGui.QWidget):

    def __init__(self,parent=None,mrupaths=[]):
        QtGui.QWidget.__init__(self,parent)

        self.bngroup      = QtGui.QButtonGroup()
        self.radioNew     = QtGui.QRadioButton('Create a new scenario from a template.',self)
        self.radioOpen    = QtGui.QRadioButton('Open an existing scenario.',self)
        self.radioImport1 = QtGui.QRadioButton('Import a namelist-based scenario from an existing directory.',self)
        self.radioImport2 = QtGui.QRadioButton('Import a namelist-based scenario from an archive.',self)

        self.labTemplate = QtGui.QLabel('Template:',self)
        default2path = core.scenario.Scenario.getDefaultValues()
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
        self.pathImport2.filter = 'tar/gz files (*.tar.gz);;zip files (*.zip);;All files (*.*)'
        
        self.bngroup.addButton(self.radioNew,    0)
        self.bngroup.addButton(self.radioOpen,   1)
        self.bngroup.addButton(self.radioImport1,2)
        self.bngroup.addButton(self.radioImport2,3)

        layout = QtGui.QGridLayout()
        layout.addWidget(self.radioNew,       0,0,1,3)
        layout.addLayout(self.templatelayout, 1,1)
        layout.addWidget(self.radioOpen,      2,0,1,3)
        layout.addWidget(self.pathOpen,       3,1,1,2)
        layout.addWidget(self.radioImport1,   4,0,1,3)
        layout.addWidget(self.pathImport1,    5,1,1,2)
        layout.addWidget(self.radioImport2,   6,0,1,3)
        layout.addWidget(self.pathImport2,    7,1,1,2)

        self.checkSkipToSimulation = QtGui.QCheckBox('Simulate the scenario without further configuration.',self)
        layout.addWidget(self.checkSkipToSimulation,8,0,1,3)

        layout.setColumnStretch(2,1)

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
        self.checkSkipToSimulation.setVisible(checkedid!=0)
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

    def getScenario(self,callback=None,completecallback=None):
        if not self.isComplete(): return None
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
        try:
            checkedid = self.bngroup.checkedId()
            if   checkedid==0:
                index = self.comboTemplates.currentIndex()
                defname = unicode(self.comboTemplates.itemData(index).toString())
                defscenario = core.scenario.Scenario.getDefault(defname,core.scenario.guiscenarioversion)
                xmldom = defscenario.toXmlDom()
                scen = core.scenario.Scenario.fromSchemaName(core.scenario.guiscenarioversion)
                scen.setStore(xmldom)
            elif checkedid==1:
                path = self.pathOpen.path()
                if path.endswith('.gotmresult'):
                    try:
                        res = core.result.Result()
                        res.load(path)
                    except Exception,e:
                        raise Exception('An error occurred while loading the result: '+str(e))
                    scen = res.scenario.addref()  # Note: the scenario version will be guiscenarioversion, set by Result
                    res.release()
                elif path.endswith('.xml'):
                    try:
                        scen = core.scenario.Scenario.fromSchemaName(core.scenario.guiscenarioversion)
                        scen.load(path)
                    except Exception,e:
                        raise Exception('An error occurred while loading the scenario: '+str(e))
                else:
                    try:
                        scen = core.scenario.Scenario.fromSchemaName(core.scenario.guiscenarioversion)
                        scen.loadAll(path,callback=callback)
                    except Exception,e:
                        raise Exception('An error occurred while loading the scenario: '+str(e))
            elif checkedid==2:
                try:
                    scen = core.scenario.Scenario.fromNamelists(self.pathImport1.path(),strict = False,targetversion=core.scenario.guiscenarioversion,requireplatform='gotm')
                except Exception,e:
                    raise Exception('Cannot parse namelist files. Error: '+str(e))
            elif checkedid==3:
                try:
                    scen = core.scenario.Scenario.fromNamelists(self.pathImport2.path(),strict = False,targetversion=core.scenario.guiscenarioversion,requireplatform='gotm')
                except Exception,e:
                    raise Exception('Cannot parse namelist files. Error: '+str(e))

        finally:
            QtGui.QApplication.restoreOverrideCursor()
            
        if checkedid!=0:
            # We have loaded a scenario from file. Look for empty nodes and reset these to their defaults.
            #emptynodes = scen.root.getEmptyNodes()
            emptynodes = [n for n in scen.root.getEmptyNodes() if not n.isHidden()]
            for node in emptynodes:
                defval = node.getDefaultValue()
                assert defval is not None, 'No value set for "%s", but no default value is available.' % node
                if isinstance(defval,xmlstore.util.referencedobject): defval.release()
            emptycount = len(emptynodes)
            if emptycount>0:
                if completecallback is not None: completecallback()
                QtGui.QMessageBox.information(self,'Scenario is incomplete','In this scenario the following %i variables do not have a value:\n\n%s\n\nThese variables will be set to their default value.' % (emptycount,'\n'.join(['/'.join(n.location) for n in emptynodes])),QtGui.QMessageBox.Ok)
                scen.changed = True
            
        return scen
        
    def setSkipToSimulation(self,value):
        self.checkSkipToSimulation.setChecked(value)

    def skipToSimulation(self):
        return self.bngroup.checkedId()!=0 and self.checkSkipToSimulation.isChecked()

    def completeStateChanged(self):
        self.emit(QtCore.SIGNAL('onCompleteStateChanged()'))
        
class PageOpen(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('How do you want to obtain a scenario?',self)
        self.scenariowidget = ScenarioWidget(self)
        self.connect(self.scenariowidget, QtCore.SIGNAL('onCompleteStateChanged()'),self.completeStateChanged)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addWidget(self.scenariowidget)
        layout.addStretch()
        self.setLayout(layout)

    def isComplete(self):
        return self.scenariowidget.isComplete()

    def saveData(self,mustbevalid):
        dialog = commonqt.ProgressDialog(self,title='Please wait...',suppressstatus=True)
        try:
            newscen = self.scenariowidget.getScenario(callback=dialog.onProgressed,completecallback=dialog.close)
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to obtain scenario', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            dialog.close()
            return False
        self.owner.setProperty('result',None)
        self.owner.setProperty('scenario',newscen)
        dialog.close()
        return True

class ScenarioPage(commonqt.WizardPage):
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.getProperty('scenario')
        if self.scenario is None: raise Exception('No scenario available; this page should not have been available.')

        self.factory = xmlstore.gui_qt4.PropertyEditorFactory(self.scenario,live=True,allowhide=True,datasourcedir=parent.getProperty('datasourcedir'))

    def saveData(self,mustbevalid):
        
        editednodes = self.factory.getEditedNodes()
        if mustbevalid:
            progressdialog = commonqt.ProgressDialog(self,title='Validating settings...')
            try:
                errors = self.scenario.validate(editednodes,callback=progressdialog.onProgressed,repair=1)
            finally:
                progressdialog.close()
            if len(errors)>0:
                QtGui.QMessageBox.critical(self,'Scenario has not been configured correctly','The following problems remain:\n\n%s' % '\n'.join(errors),QtGui.QMessageBox.Ok,QtGui.QMessageBox.NoButton)
                return False
        else:
            self.scenario.clearValidationHistory(editednodes)

        return True

    def isComplete(self):
        return True
        
    def destroy(self,destroyWindow = True,destroySubWindows = True):
        self.factory.unlink()
        commonqt.WizardPage.destroy(self,destroyWindow,destroySubWindows)
        
class PageLocation(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)
        
        layout = QtGui.QVBoxLayout()
        layout.setSpacing(25)

        self.title = self.createHeader('Simulated location and period','Here you specify the geographic location and time period that you want to simulate. Optionally you can provide a title for the simulation and location.')
        layout.addWidget(self.title)

        # Editor for title

        editGeneral = self.factory.createEditor('title',self,groupbox=True)
        groupboxGeneral = editGeneral.editor
        layoutTitle = QtGui.QHBoxLayout()
        editTitle = self.factory.createEditor('title',self)
        editTitle.addToBoxLayout(layoutTitle,addstretch=False,unit=False)
        groupboxGeneral.setLayout(layoutTitle)
        layout.addWidget(groupboxGeneral)

        # Editors for geographic location

        editStation = self.factory.createEditor('station',self,groupbox=True)
        groupboxLocation = editStation.editor

        layoutLocation = QtGui.QGridLayout()
        editName      = self.factory.createEditor('station/name',     self)
        editLongitude = self.factory.createEditor('station/longitude',self)
        editLatitude  = self.factory.createEditor('station/latitude', self)
        editDepth     = self.factory.createEditor('station/depth',    self)
        
        editName.addToGridLayout(layoutLocation,unit=False,colspan=3)
        editLongitude.addToGridLayout(layoutLocation)
        editLatitude.addToGridLayout(layoutLocation)
        editDepth.addToGridLayout(layoutLocation)
        layoutLocation.setColumnStretch(3,1)
        groupboxLocation.setLayout(layoutLocation)
        layout.addWidget(groupboxLocation)

        # Editors for simulated period

        editPeriod = self.factory.createEditor('time',self,groupbox=True)
        groupboxPeriod = editPeriod.editor

        layoutPeriod = QtGui.QGridLayout()
        editStart = self.factory.createEditor('time/start',self)
        editStop  = self.factory.createEditor('time/stop' ,self)
        editStart.addToGridLayout(layoutPeriod)
        editStop.addToGridLayout(layoutPeriod)
        layoutPeriod.setColumnStretch(3,1)
        groupboxPeriod.setLayout(layoutPeriod)
        layout.addWidget(groupboxPeriod)
        
        # Build final layout

        layout.addStretch()
        self.setLayout(layout)
        
    def saveData(self,mustbevalid):
        if self.scenario.hasChanged() and not mustbevalid:
            res = QtGui.QMessageBox.warning(self,'Leaving scenario editor','All changes to your scenario will be lost. Do you want to continue?',QtGui.QMessageBox.Yes,QtGui.QMessageBox.No,QtGui.QMessageBox.NoButton)
            if res==QtGui.QMessageBox.No: return False

        return ScenarioPage.saveData(self,mustbevalid)

class PageDiscretization(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        layout = QtGui.QVBoxLayout()
        layout.setSpacing(25)

        self.title = self.createHeader('Discretization of space and time','Here you specify how the water column and the time period are discretized. Generally this involves a balance between accuracy and simulation time.')
        layout.addWidget(self.title)

        editColumn = self.factory.createEditor('grid',self,groupbox=True)

        layoutGrid = QtGui.QGridLayout()

        # Create controls for grid layout.
        editGridMethod = self.factory.createEditor('grid/grid_method',self,selectwithradio=True)
        self.bngroup = editGridMethod.editor
        editLevelCount  = self.factory.createEditor('grid/nlev',self)      
        editZoomSurface = self.factory.createEditor('grid/ddu', self)      
        editZoomBottom  = self.factory.createEditor('grid/ddl', self)
        editGridFile = self.factory.createEditor('grid/grid_file',self)
        
        # Add controls for grid layout to the widget.
        layoutGrid.addWidget(self.bngroup.buttonFromValue(0),    0,0,1,4)
        editLevelCount.addToGridLayout (layoutGrid,1,1)
        editZoomSurface.addToGridLayout(layoutGrid,2,1)
        editZoomBottom.addToGridLayout (layoutGrid,3,1)
        layoutGrid.addWidget(self.bngroup.buttonFromValue(1),    4,0,1,4)
        layoutGrid.addWidget(self.bngroup.buttonFromValue(2),    5,0,1,4)
        editGridFile.addToGridLayout(layoutGrid,   6,1,label=False,unit=False)
                
        editColumn.editor.setLayout(layoutGrid)

        layoutGrid.setColumnStretch(3,1)
        layoutGrid.setColumnMinimumWidth(0,commonqt.getRadioWidth())


        editTime = self.factory.createEditor('timeintegration',self,groupbox=True)
        
        layoutTime = QtGui.QGridLayout()
        editTimeStep = self.factory.createEditor('timeintegration/dt',self)
        editOutputStep = self.factory.createEditor('output/dtsave',self)
        editTimeStep.addToGridLayout(layoutTime)
        editOutputStep.addToGridLayout(layoutTime)
        layoutTime.setColumnStretch(3,1)
        editTime.editor.setLayout(layoutTime)

        layout.addWidget(editColumn.editor)
        layout.addWidget(editTime.editor)
        layout.addStretch()
        self.setLayout(layout)

class PageSalinity(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        layout = QtGui.QVBoxLayout()

        self.title = self.createHeader('Salinity observations','Here you can provide observations on vertical salinity profiles. These are used to initialize the column, and optionally to relax model results to.')
        layout.addWidget(self.title)
        layout.addSpacing(20)

        # Create main layout
        layoutSalinity = QtGui.QGridLayout()
        
        # Create button group for the salinity option, and an explanatory label.
        editSalinity = self.factory.createEditor('obs/sprofile',self,selectwithradio=True)

        # Create editors for all different salinity configurations.
        editSalinityConstant       = self.factory.createEditor('obs/sprofile/s_const',self)
        editSalinityUpperThickness = self.factory.createEditor('obs/sprofile/z_s1',self)
        editSalinityUpper          = self.factory.createEditor('obs/sprofile/s_1',self)
        editSalinityLowerThickness = self.factory.createEditor('obs/sprofile/z_s2',self)
        editSalinityLower          = self.factory.createEditor('obs/sprofile/s_2',self)
        editSalinitySurface        = self.factory.createEditor('obs/sprofile/s_surf',self)
        editSalinityNSquare        = self.factory.createEditor('obs/sprofile/s_obs_NN',self)
        editSalinityFile           = self.factory.createEditor('obs/sprofile/s_prof_file',self)
        
        # Create editors for relaxation.
        editSalinityRelaxation     = self.factory.createEditor('obs/sprofile/SRelax',self,boolwithcheckbox=True)
        editSalinityBulk           = self.factory.createEditor('obs/sprofile/SRelax/SRelaxTauM',self)
        
        # Add explanatory label
        layoutSalinity.addWidget(editSalinity.createLabel(),       0,0,1,2)
        
        # Add button for no salinity
        layoutSalinity.addWidget(editSalinity.editor.buttonFromValue(0),     1,0,1,2)
        
        # Add button for constant salinity.
        layoutSalinity.addWidget(editSalinity.editor.buttonFromValue(11),    2,0,1,2)

        # Add controls to edit constant salinity.
        layoutConstant = QtGui.QGridLayout()
        editSalinityConstant.addToGridLayout(layoutConstant)
        layoutConstant.setColumnStretch(3,1)
        layoutSalinity.addLayout(layoutConstant,3,1)
        
        # Add button for two-layer salinity.
        layoutSalinity.addWidget(editSalinity.editor.buttonFromValue(12),    4,0,1,2)
        
        # Add controls to edit two-layer salinity.
        layoutLayer = QtGui.QGridLayout()
        editSalinityUpperThickness.addToGridLayout(layoutLayer)
        editSalinityUpper.addToGridLayout(layoutLayer)
        editSalinityLowerThickness.addToGridLayout(layoutLayer)
        editSalinityLower.addToGridLayout(layoutLayer)
        layoutLayer.setColumnStretch(3,1)
        layoutSalinity.addLayout(layoutLayer,5,1)
        
        # Add button for stably stratified salinity.
        layoutSalinity.addWidget(editSalinity.editor.buttonFromValue(13),    6,0,1,2)

        # Add controls to edit stably stratified salinity.
        layoutNSquare = QtGui.QGridLayout()
        editSalinitySurface.addToGridLayout(layoutNSquare)
        editSalinityNSquare.addToGridLayout(layoutNSquare)
        layoutNSquare.setColumnStretch(3,1)
        layoutSalinity.addLayout(layoutNSquare,7,1)
        
        # Add button for custom salinities.
        layoutSalinity.addWidget(editSalinity.editor.buttonFromValue(2),     8,0,1,2)
        
        # Add control to choose custom salinities.
        layoutFile = QtGui.QHBoxLayout()
        editSalinityFile.addToBoxLayout(layoutFile,label=False,unit=False)
        layoutFile.addStretch()
        layoutSalinity.addLayout(layoutFile,9,1)

        layoutSalinity.setColumnMinimumWidth(0,commonqt.getRadioWidth())
        layout.addLayout(layoutSalinity)

        # Add control to configure bulk relaxation.
        layout.addSpacing(20)
        layoutRelaxation = QtGui.QGridLayout()
        editSalinityRelaxation.addToGridLayout(layoutRelaxation,0,0,1,5,label=False,unit=False)
        editSalinityBulk.addToGridLayout(layoutRelaxation,1,1)
        layoutRelaxation.setColumnMinimumWidth(0,commonqt.getRadioWidth())
        layoutRelaxation.setColumnStretch(3,1)
        layout.addLayout(layoutRelaxation)

        layout.addStretch(1)
        
        # Set layout.
        self.setLayout(layout)

class PageTemperature(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        layout = QtGui.QVBoxLayout()

        self.title = self.createHeader('Temperature observations','Here you can provide observations on vertical temperature profiles. These are used to initialize the column, and optionally to relax model results to.')
        layout.addWidget(self.title)
        layout.addSpacing(20)

        # Create main layout
        layoutTemperature = QtGui.QGridLayout()
        
        # Create button group for the temperature option, and an explanatory label.
        editTemperature = self.factory.createEditor('obs/tprofile',self,selectwithradio=True)

        # Create editors for all different temperature configurations.
        editTemperatureConstant       = self.factory.createEditor('obs/tprofile/t_const',self)
        editTemperatureUpperThickness = self.factory.createEditor('obs/tprofile/z_t1',self)
        editTemperatureUpper          = self.factory.createEditor('obs/tprofile/t_1',self)
        editTemperatureLowerThickness = self.factory.createEditor('obs/tprofile/z_t2',self)
        editTemperatureLower          = self.factory.createEditor('obs/tprofile/t_2',self)
        editTemperatureSurface        = self.factory.createEditor('obs/tprofile/t_surf',self)
        editTemperatureNSquare        = self.factory.createEditor('obs/tprofile/t_obs_NN',self)
        editTemperatureFile           = self.factory.createEditor('obs/tprofile/t_prof_file',self)
        
        # Create editors for relaxation.
        editTemperatureRelaxation     = self.factory.createEditor('obs/tprofile/TRelax',self,boolwithcheckbox=True)
        editTemperatureBulk           = self.factory.createEditor('obs/tprofile/TRelax/TRelaxTauM',self)
        
        # Add explanatory label
        layoutTemperature.addWidget(editTemperature.createLabel(),       0,0,1,2)
        
        # Add button for no temperature
        layoutTemperature.addWidget(editTemperature.editor.buttonFromValue(0),     1,0,1,2)
        
        # Add button for constant temperature.
        layoutTemperature.addWidget(editTemperature.editor.buttonFromValue(11),    2,0,1,2)

        # Add controls to edit constant temperature.
        layoutConstant = QtGui.QGridLayout()
        editTemperatureConstant.addToGridLayout(layoutConstant)
        layoutConstant.setColumnStretch(3,1)
        layoutTemperature.addLayout(layoutConstant,3,1)
        
        # Add button for two-layer temperature.
        layoutTemperature.addWidget(editTemperature.editor.buttonFromValue(12),    4,0,1,2)
        
        # Add controls to edit two-layer temperature.
        layoutLayer = QtGui.QGridLayout()
        editTemperatureUpperThickness.addToGridLayout(layoutLayer)
        editTemperatureUpper.addToGridLayout(layoutLayer)
        editTemperatureLowerThickness.addToGridLayout(layoutLayer)
        editTemperatureLower.addToGridLayout(layoutLayer)
        layoutLayer.setColumnStretch(3,1)
        layoutTemperature.addLayout(layoutLayer,5,1)
        
        # Add button for stably stratified temperature.
        layoutTemperature.addWidget(editTemperature.editor.buttonFromValue(13),    6,0,1,2)

        # Add controls to edit stably stratified temperature.
        layoutNSquare = QtGui.QGridLayout()
        editTemperatureSurface.addToGridLayout(layoutNSquare)
        editTemperatureNSquare.addToGridLayout(layoutNSquare)
        layoutNSquare.setColumnStretch(3,1)
        layoutTemperature.addLayout(layoutNSquare,7,1)
        
        # Add button for custom salinities.
        layoutTemperature.addWidget(editTemperature.editor.buttonFromValue(2),     8,0,1,2)
        
        # Add control to choose custom salinities.
        layoutFile = QtGui.QHBoxLayout()
        editTemperatureFile.addToBoxLayout(layoutFile,label=False,unit=False)
        layoutFile.addStretch()
        layoutTemperature.addLayout(layoutFile,9,1)

        layoutTemperature.setColumnMinimumWidth(0,commonqt.getRadioWidth())
        layout.addLayout(layoutTemperature)

        # Add control to configure bulk relaxation.
        layout.addSpacing(20)
        layoutRelaxation = QtGui.QGridLayout()
        editTemperatureRelaxation.addToGridLayout(layoutRelaxation,0,0,1,5,label=False,unit=False)
        editTemperatureBulk.addToGridLayout(layoutRelaxation,1,1)
        layoutRelaxation.setColumnMinimumWidth(0,commonqt.getRadioWidth())
        layoutRelaxation.setColumnStretch(3,1)
        layout.addLayout(layoutRelaxation)

        layout.addStretch(1)
        
        # Set layout.
        self.setLayout(layout)

class PageTurbulence(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        # Create main layout
        layout = QtGui.QVBoxLayout()

        self.title = self.createHeader('Turbulence model','Here you choose the turbulence model to be used.')
        layout.addWidget(self.title)
        layout.addSpacing(20)
        
        layoutTurbulence = QtGui.QGridLayout()
        
        # Create button group for the turbulence method option, and an explanatory label.
        editMethod = self.factory.createEditor('gotmturb/turb_method',self,selectwithradio=True)
        bngrpMethod = editMethod.editor

        # Add explanatory label
        layoutTurbulence.addWidget(editMethod.createLabel(),0,0,1,3)
        
        # Add button for convective adjustment
        layoutTurbulence.addWidget(bngrpMethod.buttonFromValue(0), 1,0,1,3)

        # Add button for turbulence model calculating TKE and length scale
        layoutTurbulence.addWidget(bngrpMethod.buttonFromValue(2), 2,0,1,3)
        
        # Add controls specific to first-order model
        layoutFirstOrder = QtGui.QGridLayout()
        editStabilityMethod = self.factory.createEditor('gotmturb/stab_method',self)
        editStabilityMethod.addToGridLayout(layoutFirstOrder,0,0)
        layoutTurbulence.addLayout(layoutFirstOrder,      3,1)

        # Add button for second-order model
        layoutTurbulence.addWidget(bngrpMethod.buttonFromValue(3), 4,0,1,3)

        # Add controls specific to second-order model
        layoutSecondOrder = QtGui.QGridLayout()
        editSecondCoef = self.factory.createEditor('gotmturb/scnd/scnd_coeff',self)
        editSecondCoef.addToGridLayout(layoutSecondOrder,0,0)
        layoutTurbulence.addLayout(layoutSecondOrder,     5,1)

        # Add button for KPP model
        layoutTurbulence.addWidget(bngrpMethod.buttonFromValue(99),6,0,1,3)
        
        layoutTurbulence.setColumnStretch(3,1)
        layoutTurbulence.setColumnMinimumWidth(0,commonqt.getRadioWidth())
        layout.addLayout(layoutTurbulence)

        layout.addSpacing(20)

        layoutOther = QtGui.QGridLayout()
        editTkeMethod = self.factory.createEditor('gotmturb/tke_method',self)
        editLenScaleMethod = self.factory.createEditor('gotmturb/len_scale_method',self)
        editTkeMethod.addToGridLayout(layoutOther,0,0)
        editLenScaleMethod.addToGridLayout(layoutOther)
        layoutOther.setColumnStretch(3,1)
        layout.addLayout(layoutOther)

        layout.addStretch(1)
        
        # Set layout.
        self.setLayout(layout)
        
class PageAirSeaInteraction(ScenarioPage):

    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)
        
        radiowidth = commonqt.getRadioWidth()
        
        # Create main layout
        layout = QtGui.QVBoxLayout()

        self.title = self.createHeader('Air-sea interaction: heat and momentum','Here you specify fluxes of heat and momentum across the ocean-atmosphere interface. Note that heat fluxes include latent and sensible fluxes, but not short-wave solar radiation.')
        layout.addWidget(self.title)
        layout.addSpacing(20)

        layoutAirSea = QtGui.QGridLayout()
        layoutAirSea.setColumnMinimumWidth(0,radiowidth)
        
        editCalcFluxes = self.factory.createEditor('airsea/flux_source',self,selectwithradio=True)
        
        # Meteo file and unit

        meteolayout = QtGui.QVBoxLayout()
        editMeteoFile = self.factory.createEditor('airsea/meteo_file',self)
        editWetMode   = self.factory.createEditor('airsea/hum_method',  self)
        
        meteofilelayout = QtGui.QHBoxLayout()
        editMeteoFile.addToBoxLayout(meteofilelayout,label=False,unit=False)
        meteofilelayout.addStretch()
        meteolayout.addLayout(meteofilelayout)
        
        meteowetmodelayout = QtGui.QHBoxLayout()
        editWetMode.addToBoxLayout(meteowetmodelayout)
        meteowetmodelayout.addStretch()
        meteolayout.addLayout(meteowetmodelayout)

        # Shortwave radiation

        #groupboxSwr = self.factory.createEditor('airsea/swr_method',self,groupbox=True).editor
        #
        #swrlayout = QtGui.QGridLayout()
        #swrlayout.setColumnMinimumWidth(0,radiowidth)
#
        #editSwrMethod = self.factory.createEditor('airsea/swr_method', self,selectwithradio=True)
        #editConstSwr  = self.factory.createEditor('airsea/const_swr',  self)
        #editSwrFile   = self.factory.createEditor('airsea/swr_file',   self)
        #
        #swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(0),1,0,1,2)
#
        #swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(1),2,0,1,2)
        #constswrlayout = QtGui.QHBoxLayout()
        #editConstSwr.addToBoxLayout(constswrlayout)
        #swrlayout.addLayout(constswrlayout,3,1)
        #
        #swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(2),4,0,1,2)
        #swrfilelayout = QtGui.QHBoxLayout()
        #editSwrFile.addToBoxLayout(swrfilelayout,label=False,unit=False)
        #swrfilelayout.addStretch()
        #swrlayout.addLayout(swrfilelayout,5,1)
#
        #swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(3),6,0,1,2)
        #
        #swrlayout.setColumnStretch(2,1)
        #
        #groupboxSwr.setLayout(swrlayout)
#
        # Heat flux

        groupboxHeat = self.factory.createEditor('airsea/heat_method',self,groupbox=True).editor
        
        heatlayout = QtGui.QGridLayout()
        heatlayout.setColumnMinimumWidth(0,radiowidth)

        editHeatMethod   = self.factory.createEditor('airsea/heat_method',  self,selectwithradio=True)
        editConstHeat    = self.factory.createEditor('airsea/const_heat',   self)
        editHeatfluxFile = self.factory.createEditor('airsea/heatflux_file',self)
        
        heatlayout.addWidget(editHeatMethod.editor.buttonFromValue(0),1,0,1,2)

        heatlayout.addWidget(editHeatMethod.editor.buttonFromValue(1),2,0,1,2)
        constheatlayout = QtGui.QHBoxLayout()
        editConstHeat.addToBoxLayout(constheatlayout)
        heatlayout.addLayout(constheatlayout,3,1)
        
        heatlayout.addWidget(editHeatMethod.editor.buttonFromValue(2),4,0,1,2)
        heatfilelayout = QtGui.QHBoxLayout()
        editHeatfluxFile.addToBoxLayout(heatfilelayout,label=False,unit=False)
        heatfilelayout.addStretch()
        heatlayout.addLayout(heatfilelayout,5,1)
        
        heatlayout.setColumnStretch(2,1)
        
        groupboxHeat.setLayout(heatlayout)
        
        # Momentum fluxes

        groupboxMomentum = self.factory.createEditor('airsea/momentum_method',self,groupbox=True).editor
        
        layoutMomentum = QtGui.QGridLayout()
        layoutMomentum.setColumnMinimumWidth(0,radiowidth)

        editMomentumMethod  = self.factory.createEditor('airsea/momentum_method',  self,selectwithradio=True)
        editMomentumConstTx = self.factory.createEditor('airsea/const_tx',         self)
        editMomentumConstTy = self.factory.createEditor('airsea/const_ty',         self)
        editmomentumFile    = self.factory.createEditor('airsea/momentumflux_file',self)
        
        layoutMomentum.addWidget(editMomentumMethod.editor.buttonFromValue(0),1,0,1,2)

        layoutMomentum.addWidget(editMomentumMethod.editor.buttonFromValue(1),2,0,1,2)
        constmomentumlayout = QtGui.QGridLayout()
        editMomentumConstTx.addToGridLayout(constmomentumlayout)
        editMomentumConstTy.addToGridLayout(constmomentumlayout)
        layoutMomentum.addLayout(constmomentumlayout,3,1)
        
        layoutMomentum.addWidget(editMomentumMethod.editor.buttonFromValue(2),4,0,1,2)
        momentumfilelayout = QtGui.QHBoxLayout()
        editmomentumFile.addToBoxLayout(momentumfilelayout,label=False,unit=False)
        momentumfilelayout.addStretch()
        layoutMomentum.addLayout(momentumfilelayout,5,1)
        
        layoutMomentum.setColumnStretch(2,1)

        groupboxMomentum.setLayout(layoutMomentum)
        
        # Freshwater fluxes

#        groupboxPe = QtGui.QGroupBox('fresh water flux',self)
        
#        layoutPe = QtGui.QGridLayout()
#        layoutPe.setColumnMinimumWidth(0,radiowidth)

#        self.editPeMethod  = self.factory.createEditor(['airsea','p_e_method'],   self,selectwithradio=True)
#        self.editPeConst   = self.factory.createEditor(['airsea','const_p_e'],    self)
#        self.editPeFile    = self.factory.createEditor(['airsea','p_e_flux_file'],self)
        
#        layoutPe.addWidget(self.editPeMethod.editor.buttonFromValue(0),1,0,1,2)

#        layoutPe.addWidget(self.editPeMethod.editor.buttonFromValue(1),2,0,1,2)
#        constpelayout = QtGui.QGridLayout()
#        self.editPeConst.addToGridLayout(constpelayout)
#        layoutPe.addLayout(constpelayout,3,1)
        
#        layoutPe.addWidget(self.editPeMethod.editor.buttonFromValue(2),4,0,1,2)
#        pefilelayout = QtGui.QHBoxLayout()
#        self.editPeFile.addToBoxLayout(pefilelayout,label=False,unit=False)
#        pefilelayout.addStretch()
#        layoutPe.addLayout(pefilelayout,5,1)
        
#        groupboxPe.setLayout(layoutPe)
        
        # Create final layout

        layoutAirSea.addWidget(editCalcFluxes.createLabel(),   0,0,1,2)
        layoutAirSea.addWidget(editCalcFluxes.editor.buttonFromValue(0),1,0,1,2)
        layoutAirSea.addLayout(meteolayout,                    2,1)
        layoutAirSea.addWidget(editCalcFluxes.editor.buttonFromValue(1),3,0,1,2)
        layoutAirSea.addWidget(groupboxHeat,                   4,1)
        layoutAirSea.addWidget(groupboxMomentum,               5,1)
        #layoutAirSea.addWidget(groupboxSwr,                    6,0,1,2)
        #layoutAirSea.addWidget(groupboxPe,4,0,1,2)

        layout.addLayout(layoutAirSea)
        
        layout.addStretch(1)
                
        self.setLayout(layout)

class PageAirSeaInteraction2(ScenarioPage):

    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)
        
        radiowidth = commonqt.getRadioWidth()
        
        # Create main layout
        layout = QtGui.QVBoxLayout()

        self.title = self.createHeader('Air-sea interaction: short-wave radiation and precipitation','Here you specify short-wave solar radiation and precipitation at the ocean-atmosphere interface.')
        layout.addWidget(self.title)
        layout.addSpacing(20)

        layoutAirSea = QtGui.QGridLayout()
        layoutAirSea.setColumnMinimumWidth(0,radiowidth)

        # Shortwave radiation

        groupboxSwr = self.factory.createEditor('airsea/swr_method',self,groupbox=True).editor
        
        swrlayout = QtGui.QGridLayout()
        swrlayout.setColumnMinimumWidth(0,radiowidth)

        editSwrMethod = self.factory.createEditor('airsea/swr_method', self,selectwithradio=True)
        editConstSwr  = self.factory.createEditor('airsea/const_swr',  self)
        editSwrFile   = self.factory.createEditor('airsea/swr_file',   self)
        
        swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(0),1,0,1,2)

        swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(1),2,0,1,2)
        constswrlayout = QtGui.QHBoxLayout()
        editConstSwr.addToBoxLayout(constswrlayout)
        swrlayout.addLayout(constswrlayout,3,1)
        
        swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(2),4,0,1,2)
        swrfilelayout = QtGui.QHBoxLayout()
        editSwrFile.addToBoxLayout(swrfilelayout,label=False,unit=False)
        swrfilelayout.addStretch()
        swrlayout.addLayout(swrfilelayout,5,1)

        swrlayout.addWidget(editSwrMethod.editor.buttonFromValue(3),6,0,1,2)
        
        swrlayout.setColumnStretch(2,1)
        
        groupboxSwr.setLayout(swrlayout)

        # Freshwater fluxes

        groupboxPe = self.factory.createEditor('airsea/precip_method',self,groupbox=True).editor
        
        layoutPe = QtGui.QGridLayout()
        layoutPe.setColumnMinimumWidth(0,radiowidth)

        self.editPeMethod  = self.factory.createEditor('airsea/precip_method',self,selectwithradio=True)
        self.editPeConst   = self.factory.createEditor('airsea/const_precip', self)
        self.editPeFile    = self.factory.createEditor('airsea/precip_file',  self)
        
        layoutPe.addWidget(self.editPeMethod.editor.buttonFromValue(0),1,0,1,2)

        layoutPe.addWidget(self.editPeMethod.editor.buttonFromValue(1),2,0,1,2)
        constpelayout = QtGui.QGridLayout()
        self.editPeConst.addToGridLayout(constpelayout)
        layoutPe.addLayout(constpelayout,3,1)
        
        layoutPe.addWidget(self.editPeMethod.editor.buttonFromValue(2),4,0,1,2)
        pefilelayout = QtGui.QHBoxLayout()
        self.editPeFile.addToBoxLayout(pefilelayout,label=False,unit=False)
        pefilelayout.addStretch()
        layoutPe.addLayout(pefilelayout,5,1)
        
        layoutPe.setColumnStretch(2,1)

        groupboxPe.setLayout(layoutPe)
        
        # Create final layout
        layoutAirSea.addWidget(groupboxSwr,1,0,1,2)
        layoutAirSea.addWidget(groupboxPe, 2,0,1,2)

        layout.addLayout(layoutAirSea)
        
        layout.addStretch(1)
                
        self.setLayout(layout)

class PageBio(ScenarioPage):
    
    def __init__(self,parent=None):
        ScenarioPage.__init__(self, parent)

        layout = QtGui.QVBoxLayout()
        layout.setSpacing(25)

        self.title = self.createHeader('Biogeochemistry','Here you can specify a biogeochemical model to use, and configure this model.')
        layout.addWidget(self.title)

        # Set up model/treeview for bio section of scenario
        self.tree = xmlstore.gui_qt4.TypedStoreTreeView(self,self.scenario,self.scenario['/bio/bio_model'],datasourcedir=parent.getProperty('datasourcedir'))
        self.tree.setRootIsDecorated(False)

        editBioModel = self.factory.createEditor('bio/bio_model',self)
        self.factory.attachExternalEditor(self.tree,'bio/bio_model',conditiontype='ne',conditionvalue=0)
        editBioModel.addToBoxLayout(layout)
        layout.addWidget(self.tree,10000)
        layout.addStretch(1)

        self.setLayout(layout)

    def destroy(self,destroyWindow = True,destroySubWindows = True):
        self.tree.destroy(destroyWindow,destroySubWindows)
        commonqt.WizardPage.destroy(self,destroyWindow,destroySubWindows)
        
class PageAdvanced(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.scenario = parent.getProperty('scenario')
        if self.scenario is None: raise Exception('No scenario available; this page should not have been available.')
        
        self.tree = xmlstore.gui_qt4.TypedStoreTreeView(self,self.scenario,datasourcedir=parent.getProperty('datasourcedir'))

        layout = QtGui.QVBoxLayout()

        self.title = self.createHeader('Advanced settings','Here you can change all properties of your scenario. Values that differ from the default are shown in bold; they can be reset to their default by clicking them with the right mousebutton and choosing "Reset value".')
        layout.addWidget(self.title)
        layout.addSpacing(20)
        layout.addWidget(self.tree)
        self.setLayout(layout)

    def isComplete(self):
        return True

    def saveData(self,mustbevalid):
        if mustbevalid:
            progressdialog = commonqt.ProgressDialog(self,title='Validating settings...')
            try:
                errors = self.scenario.validate(callback=progressdialog.onProgressed,repair=1)
            finally:
                progressdialog.close()
            if len(errors)>0:
                QtGui.QMessageBox.critical(self,'Scenario is incomplete','The following problems remain:\n\n%s' % '\n'.join(errors),QtGui.QMessageBox.Ok,QtGui.QMessageBox.NoButton)
                return False
        return True

    def destroy(self,destroyWindow = True,destroySubWindows = True):
        self.tree.destroy(destroyWindow,destroySubWindows)
        commonqt.WizardPage.destroy(self,destroyWindow,destroySubWindows)
    
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

        if self.scenario.path is not None:
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
            dialog = commonqt.ProgressDialog(self,title='Saving...',suppressstatus=True)
            try:
                self.scenario.saveAll(targetpath,callback=dialog.onProgressed)
            except Exception,e:
                QtGui.QMessageBox.critical(self, 'Unable to save scenario', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                dialog.close()
                return False
            dialog.close()
            self.owner.settings.addUniqueValue('Paths/RecentScenarios','Path',targetpath)
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
        commonqt.WizardSequence.__init__(self,[PageLocation,PageDiscretization,PageAirSeaInteraction,PageAirSeaInteraction2,PageTurbulence,PageSalinity,PageTemperature,PageBio,PageAdvanced,PageSave])

def loadScenario():
    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([" "])
    else:
        app = QtGui.qApp

    # Create wizard dialog
    wiz = commonqt.Wizard(headerlogo=os.path.join(core.common.getDataRoot(),'logo.png'),allowfinish=True)
    wiz.setWindowTitle('Load scenario')
    wiz.resize(800, 600)
    seq = commonqt.WizardSequence([PageOpen])
    wiz.setSequence(seq)
    wiz.show()

    ret = app.exec_()

    scenario = wiz.getProperty('scenario')
    if scenario is not None: scenario.addref()

    wiz.destroy()
    
    return scenario

def editScenario(scenario):
    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([" "])
    else:
        app = QtGui.qApp

    import xmlplot.gui_qt4

    # Create wizard dialog
    wiz = commonqt.Wizard(headerlogo=os.path.join(core.common.getDataRoot(),'logo.png'),allowfinish=True)
    wiz.setWindowTitle('Scenario builder')
    wiz.resize(800, 600)
    
    wiz.setProperty('scenario',scenario.addref())
    wiz.setSequence(SequenceEditScenario())

    wiz.show()

    ret = app.exec_()

    scenario = wiz.getProperty('scenario')
    if scenario is not None: scenario.addref()

    wiz.destroy()
    
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

    import xmlplot.gui_qt4

    # Create wizard dialog
    wiz = commonqt.Wizard(headerlogo='./logo.png')
    wiz.setWindowTitle('Scenario builder')
    wiz.resize(800, 600)

    seq = commonqt.WizardSequence([PageOpen,SequenceEditScenario(),PageFinal])
    wiz.setSequence(seq)

    wiz.show()

    ret = app.exec_()
    
    scenario = wiz.getProperty('scenario')
    if scenario is not None: scenario.addref()

    wiz.destroy()

    return scenario

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'):
    ret = main()
    sys.exit(0)
