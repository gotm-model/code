#!/usr/bin/python

# Import standard Python modules
import os,sys,optparse

# Import Qt Modules
from PyQt4 import QtGui,QtCore

# In order to find our custom data files, make sure that we are in the directory
# containing the executable.
oldworkingdir = os.getcwdu()
os.chdir(os.path.abspath(os.path.dirname(sys.argv[0])))

# Now import our custom modules
import xmlstore.util, xmlstore.gui_qt4
import core.common
import commonqt
import errortrap

def getVersions():
    yield ('Python','%i.%i.%i %s %i' % sys.version_info)
    yield ('Qt4',QtCore.qVersion())
    yield ('PyQt4',QtCore.PYQT_VERSION_STR)
    
    import numpy
    yield ('numpy',numpy.__version__)

    import matplotlib
    yield ('matplotlib',matplotlib.__version__)
    
    import xmlplot.data
    if xmlplot.data.netcdf.selectednetcdfmodule is None: xmlplot.data.chooseNetCDFModule()
    yield xmlplot.data.netcdf.netcdfmodules[xmlplot.data.netcdf.selectednetcdfmodule]

    import gotm
    yield ('gotm',gotm.gui_util.getversion().rstrip())

class GOTMWizard(commonqt.Wizard):
    """Customized wizard dialog that show the GOTM logo at the top of the wizard window,
    and adds a "Tools" button for various functionality such as "Save as" and export of
    scenarios to namelists.
    """
    
    class DataSourceDir(object):
        def __init__(self,dictionary):
            self.dictionary = dictionary
        def get(self,default=None):
            return self.dictionary.get('datasourcedir_value',default)
        def set(self,value):
            self.dictionary['datasourcedir_value'] = value
    
    def __init__(self,parent=None,sequence=None,closebutton=False,showoptions=False):
        """Supplies the logo path to the Wizard, and adds a "Tools" button.
        """
        commonqt.Wizard.__init__(self,parent,sequence,closebutton,headerlogo=os.path.join(core.common.getDataRoot(),'logo.png'))

        self.bnTools = QtGui.QPushButton(commonqt.getIcon('advanced.png'),'&Tools',self)
        #self.bnTools.setEnabled(False)
        self.bnlayout.insertWidget(1,self.bnTools)

        self.menuTools = QtGui.QMenu(self)
        self.actSaveScenario   = self.menuTools.addAction('Save scenario as...',self.onSaveScenarioAs)
        self.actExportScenario = self.menuTools.addAction('Export scenario to namelists...',self.onExportScenario)
        self.actSaveResult     = self.menuTools.addAction('Save result as...',self.onSaveResultAs)
        self.actExportResult   = self.menuTools.addAction('Export result to NetCDF...',self.onExportResult)
        self.actAbout          = self.menuTools.addAction('About GOTM-GUI...',self.onAbout)
        
        if showoptions:
            self.actShowSettings = self.menuTools.addAction('Options...',self.onShowSettings)
        else:
            self.actShowSettings = None
            
        self.bnTools.setMenu(self.menuTools)
        
        self.setProperty('datasourcedir',self.DataSourceDir(self.shared))
        
    def onPropertyChange(self,propertyname):
        """Called by the Wizard implementation when a property in the Wizard property store
        changes value. Used to enable/disable the "Tools" button when the scenario/result is (un)set.
        """
        if propertyname=='scenario' or propertyname=='result':
            scen = self.getProperty('scenario')
            res  = self.getProperty('result')
            #self.bnTools.setEnabled(scen is not None or res is not None or self.actShowSettings is not None)
            self.actSaveScenario.setVisible(scen is not None)
            self.actExportScenario.setVisible(scen is not None)
            self.actSaveResult.setVisible(res is not None)
            self.actExportResult.setVisible(res is not None)
            
    def onAbout(self):
        attr = QtCore.Qt.Dialog|QtCore.Qt.CustomizeWindowHint|QtCore.Qt.WindowTitleHint
        try:
            attr |= QtCore.Qt.WindowCloseButtonHint
        except AttributeError:
            pass
        dialog = QtGui.QDialog(self,attr)
        layout = QtGui.QVBoxLayout()

        label = QtGui.QLabel( \
            """<p>GOTM-GUI was developed by <a href="mailto:jorn.bruggeman@xs4all.nl">Jorn Bruggeman</a> from funding by <a href="http://www.bolding-burchard.com">Bolding & Burchard</a>.</p>

<p>This program is licensed under the <a href="http://www.gnu.org">GNU General Public License</a>.</p>

<p>For any questions, please consult <a href="http://www.gotm.net">www.gotm.net</a> or write an email to <a href="mailto:gotm-users@googlegroups.com">gotm-users@googlegroups.com</a> - subscription required.</p>
""",dialog)
        label.setWordWrap(True)
        label.setOpenExternalLinks(True)
        layout.addWidget(label)

        strversions = ''
        for v in getVersions():
            strversions += '%s %s<br>' % v

        labelVersions = QtGui.QLabel('In bug reports, please quote the following version information:',dialog)
        labelVersions.setWordWrap(True)
        layout.addWidget(labelVersions)
        
        textVersions = QtGui.QTextEdit(strversions,dialog)
        textVersions.setMaximumHeight(120)
        textVersions.setReadOnly(True)
        layout.addWidget(textVersions)
        
        bnOk = QtGui.QPushButton('&OK',self)
        dialog.connect(bnOk, QtCore.SIGNAL('clicked()'), dialog.accept)

        bnlayout = QtGui.QHBoxLayout()
        bnlayout.addStretch(1.)
        bnlayout.addWidget(bnOk)

        layout.addLayout(bnlayout)
        
        dialog.setLayout(layout)
        
        dialog.setWindowTitle('About GOTM-GUI')
        dialog.resize(450,200)
        
        dialog.exec_()        

    def onShowSettings(self):
        dialog = xmlstore.gui_qt4.PropertyEditorDialog(self,self.getSettings(),'Options',flags=QtCore.Qt.Tool)
        dialog.show()
            
    def onSaveScenarioAs(self):
        scen = self.getProperty('scenario')
        path = commonqt.browseForPath(self,curpath=scen.path,save=True,filter='GOTM scenario files (*.gotmscenario);;All files (*.*)')
        if path is not None:
            dialog = commonqt.ProgressDialog(self,title='Saving...',suppressstatus=True)
            try:
                scen.saveAll(path,callback=dialog.onProgressed)
            finally:
                dialog.close()
            self.getSettings().addUniqueValue('Paths/RecentScenarios','Path',path)

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
                versions = scen.getSchemaInfo().getSchemas().keys()
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
                
                self.setWindowTitle('Export scenario to namelists')
                
        scen = self.getProperty('scenario')
        dialog = ChooseVersionDialog(self)
        res = dialog.exec_()
        if res==QtGui.QDialog.Accepted:
            curpath = None
            if scen.path is not None: curpath = os.path.dirname(scen.path)
            path = commonqt.browseForPath(self,curpath=curpath,getdirectory=True)
            if path is not None:
                progdialog = commonqt.ProgressDialog(self,title='Exporting...',suppressstatus=True)
                try:
                    progslicer = xmlstore.util.ProgressSlicer(progdialog.onProgressed,2)
                    progslicer.nextStep('converting to desired version')
                    exportscen = scen.convert(unicode(dialog.comboVersion.currentText()),callback=progslicer.getStepCallback())
                    progslicer.nextStep('writing files')
                    exportscen.writeAsNamelists(path,addcomments=True,callback=progslicer.getStepCallback())
                    exportscen.release()
                finally:
                    progdialog.close()
                
    def onSaveResultAs(self):
        res = self.getProperty('result')
        path = commonqt.browseForPath(self,curpath=res.path,save=True,filter='GOTM result files (*.gotmresult);;All files (*.*)')
        if path is not None:
            dialog = commonqt.ProgressDialog(self,title='Saving...',suppressstatus=True)
            try:
                res.save(path,callback=dialog.onProgressed)
            finally:
                dialog.close()
            self.getSettings().addUniqueValue('Paths/RecentResults','Path',path)
            
    def onExportResult(self):
        res = self.getProperty('result')
        curpath = None
        if res.path is not None:
            root,ext = os.path.splitext(res.path)
            curpath = root+'.nc'
        path = commonqt.browseForPath(self,curpath=curpath,save=True,filter='NetCDF files (*.nc);;All files (*.*)')
        if path is not None:
            QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
            try:
                res.saveNetCDF(path)
            finally:
                QtGui.QApplication.restoreOverrideCursor()

class PageIntroduction(commonqt.WizardPage):
    """First page in the GOTM-GUI Wizard.
    Shows an introductory text with links to internet resources, licensing information,
    author, commissioner, and versions of the various modules.
    """
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        # Clear some non-persistent settings.
        self.owner.setProperty('scenario',None)
        self.owner.setProperty('result',None)

        layout = QtGui.QVBoxLayout()

        self.label = QtGui.QLabel( \
            """<p>This is the Graphical User Interface to the <a href="http://www.gotm.net">General Ocean Turbulence Model (GOTM)</a>.</p>

<p>GOTM is a one-dimensional water column model for natural (marine and limnic) waters based on the Reynolds-averaged Navier-Stokes equations. Vertical mixing is  included through an extensive library of state-of-the-art turbulence closure models. The hydrodynamics may be forced by wind stresses, surface heat and buoyancy fluxes, solar radiation and prescribed external and internal pressure gradients.</p>

<p>GOTM includes also a library of ecosystem models, ranging from simple settling of suspended matter to low-, medium- and high-complexity biogeochemical formulations.</p>

<p>There is a number of ready-to-use scenarios available for GOTM, with coastal, shelf sea, open ocean and limnic applications, a few of them including ecosystem modelling. These can be downloaded from <a href="http://www.gotm.net/index.php?go=software&page=testcases">the GOTM web site</a>.</p>

<p>This program offers a user-friendly interface to all options supported by GOTM. It allows you to run existing test cases, or to create and configure a custom scenario. The program will guide you step by step through the process of setting up a scenario, doing the calculations and displaying the results.</p>

<p>For any questions, please consult <a href="http://www.gotm.net">www.gotm.net</a> or write an email to <a href="mailto:gotm-users@googlegroups.com">gotm-users@googlegroups.com</a> - subscription required.</p>

<p>GOTM-GUI was developed by <a href="mailto:jorn.bruggeman@xs4all.nl">Jorn Bruggeman</a> from funding by <a href="http://www.bolding-burchard.com">Bolding & Burchard</a>.</p>

<p>This program is licensed under the <a href="http://www.gnu.org">GNU General Public License</a>.</p>
""",self)
        self.label.setWordWrap(True)
        try:
            self.label.setOpenExternalLinks(True)
        except Exception,e:
            print 'Failed to enable links in QLabel. This may be because you are using a version of Qt prior to 4.2. Error: %s' % e
        layout.addWidget(self.label)

        layout.addStretch(1)

        self.setLayout(layout)

    def isComplete(self):
        return True
        
class PageChooseAction(commonqt.WizardPage):
    """Second page in the GOTM-GUI Wizard.
    Allows the user to choose to between working with a scenario or a result, and to select
    the source (e.g., path) of the scenario/result.
    """
    
    def __init__(self,parent=None):
        import scenariobuilder,visualizer
    
        commonqt.WizardPage.__init__(self, parent)

        pathnodes = self.parent().getSettings().root.getLocationMultiple(['Paths','RecentScenarios','Path'])
        mruscenarios = [p.getValue() for p in pathnodes]

        pathnodes = self.parent().getSettings().root.getLocationMultiple(['Paths','RecentResults','Path'])
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
        if curres is not None and curres.path is not None:
            self.resultwidget.setPath(curres.path)
        else:
            curscen = self.owner.getProperty('scenario')
            if curscen is not None and curscen.path is not None:
                self.scenariowidget.setPath(curscen.path)

        if self.owner.getProperty('skipscenariobuilder'):
            self.scenariowidget.setSkipToSimulation(True)

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
        dialog = commonqt.ProgressDialog(self,title='Please wait')
        res = False
        if checkedid==0:
            simulate = self.scenariowidget.skipToSimulation()
            totweight = 1
            if simulate: totweight+=1
            progslicer = xmlstore.util.ProgressSlicer(dialog.onProgressed,totweight)

            progslicer.nextStep('Loading scenario...',nodetailedmessage=True)
            try:
                newscen = self.scenariowidget.getScenario(callback=progslicer.getStepCallback(),completecallback=dialog.close)
            except Exception,e:
                dialog.close()
                if isinstance(e,AssertionError): raise
                QtGui.QMessageBox.critical(self, 'Unable to obtain scenario', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False

            if simulate:
                progslicer.nextStep('Validating scenario...',nodetailedmessage=True)
                errors = newscen.validate(callback=progslicer.getStepCallback(),repair=1)
                dialog.close()
                if len(errors)>0:
                    res = QtGui.QMessageBox.warning(self,'Scenario is incomplete','The scenario cannot be simulated because of the following problems:\n\n%s\n\nDo you want to open the scenario for editing instead?' % '\n'.join(errors),QtGui.QMessageBox.Yes|QtGui.QMessageBox.No,QtGui.QMessageBox.Yes)
                    if res==QtGui.QMessageBox.No: return False
                    simulate = False
            else:
                dialog.close()

            self.owner.setProperty('mainaction','scenario')
            self.owner.setProperty('skipscenariobuilder',simulate)
            self.owner.setProperty('scenario', newscen)

            # Add to list of most-recently-used scenarios
            if newscen.path is not None:
                self.owner.getSettings().addUniqueValue('Paths/RecentScenarios','Path',newscen.path)
            
            res =  True
        if checkedid==1:
            dialog.suppressstatus = True
            try:
                newresult = self.resultwidget.getResult()
            except Exception,e:
                dialog.close()
                QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False
            self.owner.setProperty('mainaction','result')
            self.owner.setProperty('result', newresult)
            if newresult.scenario is not None:
                self.owner.setProperty('scenario', newresult.scenario.addref())

            # Add to list of most-recently-used results
            if newresult.path is not None:
                self.owner.getSettings().addUniqueValue('Paths/RecentResults','Path',newresult.path)

            res = True

            # Close progress dialog
            dialog.close()
        return res

def main(options,args):
    if options.verbose:
        print 'Module versions:'
        for module,version in getVersions():
            print '   %s %s' % (module,version)
        global core,xmlstore
        import core.common,xmlstore.xmlstore
        core.common.verbose = True
        xmlstore.xmlstore.verbose = True

    if options.nc is not None:
        import xmlplot.data
        if xmlplot.data.netcdf.selectednetcdfmodule is None: xmlplot.data.netcdf.chooseNetCDFModule()
        for xmlplot.data.netcdf.selectednetcdfmodule,(m,v) in enumerate(xmlplot.data.netcdf.netcdfmodules):
            if m==options.nc: break
        else:
            print 'Forced NetCDF module "%s" is not available. Available modules: %s.' % (options.nc,', '.join([m[0] for m in xmlplot.data.netcdf.netcdfmodules]))
            sys.exit(2)

    if options.schemadir is not None:
        import core.scenario
        core.scenario.schemadir = options.schemadir
	
    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([' '])
    else:
        app = QtGui.qApp

    app.setWindowIcon(QtGui.QIcon(os.path.join(core.common.getDataRoot(),'icon.png')))

    class ForkOnAction1(commonqt.WizardFork):
        def getSequence(self):
            if self.wizard.getProperty('mainaction')=='scenario':
                import simulator
                if self.wizard.getProperty('skipscenariobuilder'):
                    return commonqt.WizardSequence([simulator.PageProgress])
                else:
                    import scenariobuilder
                    return commonqt.WizardSequence([scenariobuilder.SequenceEditScenario(),simulator.PageProgress])
            else:
                return commonqt.WizardSequence([commonqt.WizardDummyPage])

    class ForkOnAction2(commonqt.WizardFork):
        def getSequence(self):
            import visualizer
            return commonqt.WizardSequence([visualizer.PageVisualize,visualizer.PageReportGenerator,visualizer.PageSave,visualizer.PageFinal])

    # Create wizard dialog
    wiz = GOTMWizard(closebutton = xmlstore.gui_qt4.needCloseButton(), showoptions=options.showoptions)
    seq = commonqt.WizardSequence([PageIntroduction,PageChooseAction,ForkOnAction1(wiz),ForkOnAction2(wiz)])
    wiz.setSequence(seq)
    wiz.setWindowTitle('GOTM-GUI')
    wiz.resize(850, 600)

    # Parse command line arguments
    openpath = None
    scen = None
    res = None
    if len(args)>0:
        import core.scenario, core.result
    
        openpath = os.path.normpath(os.path.join(oldworkingdir, args[0]))
        del args[0]
        
        try:
            container = xmlstore.datatypes.DataContainer.fromPath(openpath)
        except Exception,e:
            QtGui.QMessageBox.critical(wiz, 'Unable to load specified path', unicode(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            container = None

        if container is None:
            pass
        elif core.scenario.Scenario.canBeOpened(container):
            # Try to open the file as a scenario.
            scen = core.scenario.Scenario.fromSchemaName(core.scenario.guiscenarioversion)
            try:
                scen.loadAll(container)
            except Exception,e:
                QtGui.QMessageBox.critical(wiz, 'Unable to load scenario', unicode(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                scen = None
        elif core.result.Result.canBeOpened(container):
            res = core.result.Result()
            # Try to open the file as a result.
            try:
                res.load(container)
            except Exception,e:
                QtGui.QMessageBox.critical(wiz, 'Unable to load result', unicode(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                res = None
        else:
            QtGui.QMessageBox.critical(wiz, 'Unable to open specified path', '"%s" is not a scenario or a result.' % openpath, QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                
        if container is not None: container.release()

    # If a file to open was specified on the command line, move some steps forward in the wizard.
    if res is not None:
        wiz.onNext()
        wiz.setProperty('mainaction','result')
        wiz.setProperty('result', res)
        if openpath.endswith('.gotmresult'):
            wiz.getSettings().addUniqueValue('Paths/RecentResults','Path',openpath)
        if res.scenario is not None:
            wiz.setProperty('scenario', res.scenario.addref())
        wiz.onNext(askoldpage=False)
    elif scen is not None:
        wiz.onNext()
        wiz.setProperty('mainaction','scenario')
        wiz.setProperty('scenario',scen)
        if openpath.endswith('.gotmscenario'):
            wiz.getSettings().addUniqueValue('Paths/RecentScenarios','Path',openpath)
        wiz.onNext(askoldpage=False)

    # Show wizard dialog
    wiz.show()

    # Redirect stderr to error dialog (last action before message loop is started,
    # because text sent to stderr will be lost if redirected to error dialog without
    # the message loop being started.
    errortrap.redirect_stderr('GOTM-GUI','You may be able to continue working. However, we would appreciate it if you report this error. To do so, send an e-mail to <a href="mailto:gotm-users@googlegroups.com">gotm-users@googlegroups.com</a> with the above error message, and the circumstances under which the error occurred.')

    # Enter the main message loop.
    ret = app.exec_()

    # Clean-up the wizard
    wiz.destroy()

    # Return the exit code of the Qt message loop.    
    return ret

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'):
    # Parse command line options for profiling
    parser = optparse.OptionParser()
    parser.add_option('--showoptions',action='store_true',help='provides access to persistent program settings via the Tools menu.')
    parser.add_option('-v','--verbose',action='store_true',help='writes debug strings to standard output.')
    parser.add_option('-p','--profile',action='store_true',help='activates profiling.')
    parser.add_option('-d','--debug',action='store_true',help='activates debugging (e.g., reference counting).')
    if not hasattr(sys,'frozen'):
        parser.add_option('--nc', type='string', help='NetCDF module to use')
        parser.add_option('--schemadir', type='string', help='Path to scenario schema directory')
    parser.set_defaults(profile=False,showoptions=False,verbose=False,debug=False,nc=None,schemadir=None)
    (options, args) = parser.parse_args()
    
    if options.debug: xmlstore.util.referencedobject.checkreferences = True
    
    if options.profile:
        # We will do profiling
        import cProfile,pstats
        cProfile.run('main(options,args)', 'gotmprof')
        p = pstats.Stats('gotmprof')
        p.strip_dirs().sort_stats('cumulative').print_stats()
    else:
        # Just enter the main loop
        ret = main(options,args)

    # Exit
    sys.exit(ret)

# Reset previous working directory (only if we had to change it)
os.chdir(os.path.dirname(oldworkingdir))
