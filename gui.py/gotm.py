#!/usr/bin/python

# Import standard Python modules
import os,sys

# Import Qt Modules
from PyQt4 import QtGui,QtCore

# Import MatPlotLib to configure key parameters
import matplotlib
matplotlib.use('Qt4Agg')
matplotlib.rcParams['numerix'] = 'numpy'

# In order to find our custom data files, make sure that we are in the directory
# containing the executable.
oldworkingdir = os.getcwdu()
os.chdir(os.path.abspath(os.path.dirname(sys.argv[0])))

# Now import our custom modules
import common, commonqt, xmlstore, data, scenario

import scenariobuilder,simulator,visualizer

class GOTMWizard(commonqt.Wizard):
    """Customized wizard dialog that show the GOTM logo at the top of the wizard window,
    and adds a "Tools" button for various functionality such as "Save as" and export of
    scenarios to namelists.
    """
    
    def __init__(self,parent=None,sequence=None,closebutton=False):
        """Supplies the logo path to the Wizard, and adds a "Tools" button.
        """
        commonqt.Wizard.__init__(self,parent,sequence,closebutton,headerlogo='./logo.png')

        self.bnTools = QtGui.QPushButton('&Tools',self)
        self.bnTools.setEnabled(False)
        self.bnlayout.insertWidget(1,self.bnTools)

        self.menuTools = QtGui.QMenu(self)
        self.actSaveScenario   = self.menuTools.addAction('Save scenario as...',self.onSaveScenarioAs)
        self.actExportScenario = self.menuTools.addAction('Export scenario to namelists...',self.onExportScenario)
        self.actSaveResult     = self.menuTools.addAction('Save result as...',self.onSaveResultAs)
        self.actExportResult   = self.menuTools.addAction('Export result to NetCDF...',self.onExportResult)
        
        self.bnTools.setMenu(self.menuTools)
        
    def onPropertyChange(self,propertyname):
        """Called by the Wizard implementation when a property in the Wizard property store
        changes value. Used to enable/disable the "Tools" button when the scenario/result is (un)set.
        """
        if propertyname=='scenario' or propertyname=='result':
            scen   = self.getProperty('scenario')
            result = self.getProperty('result')
            self.bnTools.setEnabled(scen!=None or result!=None)
            self.actSaveScenario.setVisible(scen!=None)
            self.actExportScenario.setVisible(scen!=None)
            self.actSaveResult.setVisible(result!=None)
            self.actExportResult.setVisible(result!=None)
            
    def onSaveScenarioAs(self):
        scen = self.getProperty('scenario')
        path = commonqt.browseForPath(self,curpath=scen.path,save=True,filter='GOTM scenario files (*.gotmscenario);;All files (*.*)')
        if path!=None:
            dialog = commonqt.ProgressDialog(self,title='Saving...',suppressstatus=True)
            scen.saveAll(path,callback=dialog.onProgressed)
            dialog.close()

    def onExportScenario(self):
        class ChooseVersionDialog(QtGui.QDialog):
            """Dialog for choosing the version of GOTM to export namelists for.
            """
            def __init__(self,parent=None):
                QtGui.QDialog.__init__(self,parent,QtCore.Qt.Dialog | QtCore.Qt.MSWindowsFixedSizeDialogHint | QtCore.Qt.WindowTitleHint)
                
                layout = QtGui.QVBoxLayout()
                
                # Add introductory label.
                self.label = QtGui.QLabel('Choose the version of GOTM to export for:',self)
                layout.addWidget(self.label)
                
                # Add combobox with versions.
                self.comboVersion = QtGui.QComboBox(self)
                versions = scen.getDefaultSchemas().keys()
                versions.sort()
                for v in versions:
                    # Only show schemas for namelist-supporting GOTM
                    # (and not those for the GUI)
                    if v.startswith('gotm-'): self.comboVersion.addItem(v)
                self.comboVersion.setCurrentIndex(self.comboVersion.count()-1)
                layout.addWidget(self.comboVersion)
                
                layoutButtons = QtGui.QHBoxLayout()

                # Add "OK" button
                self.bnOk = QtGui.QPushButton('&OK',self)
                self.connect(self.bnOk, QtCore.SIGNAL('clicked()'), self.accept)
                layoutButtons.addWidget(self.bnOk)

                # Add "Cancel" button
                self.bnCancel = QtGui.QPushButton('&Cancel',self)
                self.connect(self.bnCancel, QtCore.SIGNAL('clicked()'), self.reject)
                layoutButtons.addWidget(self.bnCancel)
                
                layout.addLayout(layoutButtons)

                self.setLayout(layout)
                
                self.setWindowTitle('Choose GOTM version')
                
        scen = self.getProperty('scenario')
        dialog = ChooseVersionDialog(self)
        res = dialog.exec_()
        if res==QtGui.QDialog.Accepted:
            curpath = None
            if scen.path!=None: curpath = os.path.dirname(scen.path)
            path = commonqt.browseForPath(self,curpath=curpath,getdirectory=True)
            if path!=None:
                progdialog = commonqt.ProgressDialog(self,title='Exporting...',suppressstatus=True)
                progslicer = common.ProgressSlicer(progdialog.onProgressed,2)
                progslicer.nextStep('converting to desired version')
                exportscen = scen.convert(unicode(dialog.comboVersion.currentText()),callback=progslicer.getStepCallback())
                progslicer.nextStep('writing files')
                exportscen.writeAsNamelists(path,addcomments=True,callback=progslicer.getStepCallback())
                exportscen.release()
                progdialog.close()
                
    def onSaveResultAs(self):
        result = self.getProperty('result')
        path = commonqt.browseForPath(self,curpath=result.path,save=True,filter='GOTM result files (*.gotmresult);;All files (*.*)')
        if path!=None:
            result.save(path)
            
    def onExportResult(self):
        result = self.getProperty('result')
        curpath = None
        if result.path!=None:
            root,ext = os.path.splitext(result.path)
            curpath = root+'.nc'
        path = commonqt.browseForPath(self,curpath=curpath,save=True,filter='NetCDF files (*.nc);;All files (*.*)')
        if path!=None:
            result.saveNetCDF(path)

class PageIntroduction(commonqt.WizardPage):
    """First page in the GOTM-GUI Wizard.
    Shows an introductory text with links to internet resources, licensing information,
    author, commissioner, and versions of the various modules.
    """
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        # Clear all non-persistent settings.
        self.owner.clearProperties()

        # For version only:
        import matplotlib,numpy,gotm

        versions = []
        versions.append(('Python','%i.%i.%i %s %i' % sys.version_info))
        versions.append(('Qt4',QtCore.qVersion()))
        versions.append(('PyQt4',QtCore.PYQT_VERSION_STR))
        versions.append(('numpy',numpy.__version__))
        versions.append(('matplotlib',matplotlib.__version__))
        try:
            import Scientific
            versions.append(('Scientific',Scientific.__version__))
        except: pass
        versions.append(('gotm',gotm.gui_util.getversion().rstrip()))
        
        strversions = '<table cellspacing="0" cellpadding="0">'
        for v in versions:
            strversions += '<tr><td>%s</td><td>&nbsp;</td><td>%s</td></tr>' % v
        strversions += '</table>'

        layout = QtGui.QVBoxLayout()

        self.label = QtGui.QLabel( \
            """<p>This is the Graphical User Interface to the <a href="http://www.gotm.net">General Ocean Turbulence Model (GOTM)</a>.</p>

<p>GOTM is a one-dimensional water column model for natural (marine and limnic) waters based on the Reynolds-averaged Navier-Stokes equations. Vertical mixing is  included through an extensive library of state-of-the-art turbulence closure models. The hydrodynamics may be forced by wind stresses, surface heat and buoyancy fluxes, solar radiation and prescribed external and internal pressure gradients.</p>

<p>GOTM includes also a library of ecosystem models, ranging from simple settling of suspended matter to low-, medium- and high-complexity biogeochemical formulations.</p>

<p>There is a number of ready-to-use scenarios available for GOTM, with coastal, shelf sea, open ocean and limnic applications, a few of them including ecosystem modelling. These can be downloaded from <a href="http://www.gotm.net/index.php?go=software&page=testcases">the GOTM web site</a>.</p>

<p>This program offers a user-friendly interface to all options supported by GOTM. It allows you to run existing test cases, or to create and configure a custom scenario. The program will guide you step by step through the process of setting up a scenario, doing the calculations and displaying the results.</p>

<p>For any questions, please consult <a href="http://www.gotm.net">www.gotm.net</a> or write an email to <a href="mailto:gotm-users@gotm.net">gotm-users@gotm.net</a>.</p>

<p>GOTM-GUI was developed by <a href="mailto:jorn.bruggeman@xs4all.nl">Jorn Bruggeman</a> from funding by <a href="http://www.bolding-burchard.com">Bolding & Burchard Hydrodynamics</a>.</p>

<p>This program is licensed under the <a href="http://www.gnu.org">GNU General Public License</a>.</p>
""",self)
        self.label.setWordWrap(True)
        try:
            self.label.setOpenExternalLinks(True)
        except Exception,e:
            print 'Failed to enable links in QLabel. This may be because you are using a version of Qt prior to 4.2. Error: %s' % e
        layout.addWidget(self.label)

        layout.addStretch()

        layout.addStretch(1)

        self.labelVersions = QtGui.QLabel('Module versions:',self)
        layout.addWidget(self.labelVersions)
        
        self.textVersions = QtGui.QTextEdit(strversions,self)
        self.textVersions.setMaximumHeight(120)
        self.textVersions.setReadOnly(True)
        layout.addWidget(self.textVersions)

        self.setLayout(layout)

    def isComplete(self):
        return True
        
class PageChooseAction(commonqt.WizardPage):
    """Second page in the GOTM-GUI Wizard.
    Allows the user to choose to between working with a scenario or a result, and to select
    the source (e.g., path) of the scenario/result.
    """
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        pathnodes = self.parent().settings.root.getLocationMultiple(['Paths','RecentScenarios','Path'])
        mruscenarios = [p.getValue() for p in pathnodes]

        pathnodes = self.parent().settings.root.getLocationMultiple(['Paths','RecentResults','Path'])
        mruresults = [p.getValue() for p in pathnodes]

        self.label = QtGui.QLabel('What would you like to do?',self)
        self.radioScenario = QtGui.QRadioButton('I want to create, view or edit a scenario.',self)
        self.radioResult = QtGui.QRadioButton('I want to view or process the result of a previous simulation.',self)
        self.scenariowidget = scenariobuilder.ScenarioWidget(self,mrupaths=mruscenarios)
        self.connect(self.scenariowidget, QtCore.SIGNAL('onCompleteStateChanged()'),self.completeStateChanged)
        self.resultwidget = visualizer.OpenWidget(self,mrupaths=mruresults)
        self.connect(self.resultwidget, QtCore.SIGNAL('onCompleteStateChanged()'),self.completeStateChanged)

        self.bngroup     = QtGui.QButtonGroup()
        self.bngroup.addButton(self.radioScenario,0)
        self.bngroup.addButton(self.radioResult,1)
        self.connect(self.bngroup, QtCore.SIGNAL('buttonClicked(int)'), self.onSourceChange)

        layout = QtGui.QGridLayout()
        layout.addWidget(self.label,0,0,1,2)
        layout.addWidget(self.radioScenario,1,0,1,2)
        layout.addWidget(self.scenariowidget,2,1,1,1)
        layout.addWidget(self.radioResult,3,0,1,2)
        layout.addWidget(self.resultwidget,4,1,1,1)
        
        layout.setColumnMinimumWidth(0,commonqt.getRadioWidth())

        layout.setRowStretch(5,1)
        layout.setColumnStretch(1,1)
        
        self.setLayout(layout)

        # Pre-check result if a result object was loaded previously.
        if self.owner.getProperty('mainaction')=='result':
            self.radioResult.setChecked(True)
        else:
            self.radioScenario.setChecked(True)
            
        # Fill in path of currently loaded result or scenario.
        curres = self.owner.getProperty('result')
        if curres!=None and curres.path!=None:
            self.resultwidget.setPath(curres.path)
        else:
            curscen = self.owner.getProperty('scenario')
            if curscen!=None and curscen.path!=None:
                self.scenariowidget.setPath(curscen.path)

        # Clear currently loaded scenario and result.
        self.owner.setProperty('result', None)
        self.owner.setProperty('scenario', None)
            
        self.onSourceChange()

    def onSourceChange(self):
        checkedid = self.bngroup.checkedId()
        self.scenariowidget.setVisible(checkedid==0)
        self.resultwidget.setVisible(checkedid==1)
        self.completeStateChanged()

    def isComplete(self):
        checkedid = self.bngroup.checkedId()
        if checkedid==0:
            return self.scenariowidget.isComplete()
        elif checkedid==1:
            return self.resultwidget.isComplete()
        return False

    def saveData(self,mustbevalid):
        if not mustbevalid: return True
        checkedid = self.bngroup.checkedId()
        dialog = commonqt.ProgressDialog(self,title='Please wait...',suppressstatus=True)
        result = False
        if checkedid==0:
            try:
                newscen = self.scenariowidget.getScenario(callback=dialog.onProgressed)
            except Exception,e:
                QtGui.QMessageBox.critical(self, 'Unable to obtain scenario', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                dialog.close()
                return False
            self.owner.setProperty('mainaction','scenario')
            self.owner.setProperty('scenario', newscen)

            # Add to list of most-recently-used scenarios
            if newscen.path!=None:
                self.owner.settings.addUniqueValue('Paths/RecentScenarios','Path',newscen.path)
            
            result =  True
        if checkedid==1:
            try:
                newresult = self.resultwidget.getResult()
            except Exception,e:
                QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                dialog.close()
                return False
            self.owner.setProperty('mainaction','result')
            self.owner.setProperty('result', newresult)
            if newresult.scenario!=None:
                self.owner.setProperty('scenario', newresult.scenario.addref())

            # Add to list of most-recently-used results
            if newresult.path!=None:
                self.owner.settings.addUniqueValue('Paths/RecentResults','Path',newresult.path)

            result = True
        dialog.close()
        return result

class ForkOnAction(commonqt.WizardFork):
    def getSequence(self):
        if self.wizard.getProperty('mainaction')=='scenario':
            return commonqt.WizardSequence([scenariobuilder.SequenceEditScenario(),simulator.PageProgress])
        else:
            return commonqt.WizardSequence([commonqt.WizardDummyPage])
def main():
    # Debug info
    print 'Python version: %s' % unicode(sys.version_info)
    print 'PyQt4 version: %s' % QtCore.PYQT_VERSION_STR
    print 'Qt version: %s' % QtCore.qVersion()
	
    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([' '])
    else:
        app = QtGui.qApp

    # Create wizard dialog
    wiz = GOTMWizard(closebutton = commonqt.addCloseButton())
    seq = commonqt.WizardSequence([PageIntroduction,PageChooseAction,ForkOnAction(wiz),visualizer.PageVisualize,visualizer.PageReportGenerator,visualizer.PageSave,visualizer.PageFinal])
    wiz.setSequence(seq)
    wiz.setWindowTitle('GOTM-GUI')
    wiz.resize(800, 600)

    # Parse command line arguments
    openpath = None
    scen = None
    result = None
    if len(sys.argv)>1:
        openpath = os.path.normpath(os.path.join(oldworkingdir, sys.argv[1]))
        del sys.argv[1]
        
        try:
            container = xmlstore.DataContainer.fromPath(openpath)
        except Exception,e:
            QtGui.QMessageBox.critical(wiz, 'Unable to load specified path', unicode(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            container = None

        if container==None:
            pass
        elif scenario.Scenario.canBeOpened(container):
            # Try to open the file as a scenario.
            scen = scenario.Scenario.fromSchemaName(scenario.guiscenarioversion)
            try:
                scen.loadAll(container)
            except Exception,e:
                QtGui.QMessageBox.critical(wiz, 'Unable to load scenario', unicode(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                scen = None
        elif data.Result.canBeOpened(container):
            result = data.Result()
            # Try to open the file as a result.
            try:
                result.load(container)
            except Exception,e:
                QtGui.QMessageBox.critical(wiz, 'Unable to load result', unicode(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                result = None
        else:
            QtGui.QMessageBox.critical(wiz, 'Unable to open specified path', '"%s" is not a scenario or a result.' % openpath, QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                
        if container!=None: container.release()

    # If a file to open was specified on the command line, move some steps forward in the wizard.
    if result!=None:
        wiz.onNext()
        wiz.setProperty('mainaction','result')
        wiz.setProperty('result', result)
        if openpath.endswith('.gotmresult'):
            wiz.settings.addUniqueValue('Paths/RecentResults','Path',openpath)
        if result.scenario!=None:
            wiz.setProperty('scenario', result.scenario.addref())
        wiz.onNext(askoldpage=False)
    elif scen!=None:
        wiz.onNext()
        wiz.setProperty('mainaction','scenario')
        wiz.setProperty('scenario',scen)
        if openpath.endswith('.gotmscenario'):
            wiz.settings.addUniqueValue('Paths/RecentScenarios','Path',openpath)
        wiz.onNext(askoldpage=False)

    # Show wizard dialog
    wiz.show()

    # Redirect stderr to error dialog (last action before message loop is started,
    # because text sent to stderr will be lost if redirected to error dialog without
    # the message loop being started.
    commonqt.redirect_stderr()

    # Enter the main message loop.
    ret = app.exec_()

    # Clean-up the wizard
    wiz.unlink()

    # Return the exit code of the Qt message loop.    
    return ret

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): ret = main()

# Reset previous working directory (only if we had to change it)
os.chdir(os.path.dirname(oldworkingdir))

# Exit
sys.exit(ret)