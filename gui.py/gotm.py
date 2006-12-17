#!/usr/bin/python

from PyQt4 import QtGui,QtCore

import common,commonqt
import sys,xml

# For version only:
import matplotlib,pycdf,numpy,Numeric

import scenariobuilder,simulator,visualizer

class PageIntroduction(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        layout = QtGui.QVBoxLayout()

        self.label = QtGui.QLabel('Placeholder for introduction to GOTM (and a pretty picture or so).',self)
        layout.addWidget(self.label)

        layout.addStretch()

        versions = []
        versions.append(('Python','%i.%i.%i %s %i' % sys.version_info))
        versions.append(('Qt4',QtCore.qVersion()))
        versions.append(('PyQt4',QtCore.PYQT_VERSION_STR))
        versions.append(('Numeric',Numeric.__version__))
        versions.append(('numpy',numpy.__version__))
        versions.append(('matplotlib',matplotlib.__version__))
        versions.append(('pycdf',pycdf.pycdfVersion()))

        strversions = ''
        for v in versions:
            strversions += '%s %s\n' % v

        self.labelVersions = QtGui.QLabel('Module versions:\n'+strversions,self)
        layout.addWidget(self.labelVersions)

        self.setLayout(layout)

    def isComplete(self):
        return True

class PageChooseAction(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('What would you like to do?',self)
        self.radioScenario = QtGui.QRadioButton('I want to create, view or edit a scenario.',self)
        self.radioResult = QtGui.QRadioButton('I want to view or process the result of a previous simulation.',self)
        self.scenariowidget = scenariobuilder.ScenarioWidget(self)
        self.connect(self.scenariowidget, QtCore.SIGNAL("onCompleteStateChanged()"),self.completeStateChanged)
        self.resultwidget = visualizer.OpenWidget(self)
        self.connect(self.resultwidget, QtCore.SIGNAL("onCompleteStateChanged()"),self.completeStateChanged)

        self.bngroup     = QtGui.QButtonGroup()
        self.bngroup.addButton(self.radioScenario,0)
        self.bngroup.addButton(self.radioResult,1)
        self.connect(self.bngroup, QtCore.SIGNAL("buttonClicked(int)"), self.onSourceChange)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addWidget(self.radioScenario)
        layout.addWidget(self.scenariowidget)
        layout.addWidget(self.radioResult)
        layout.addWidget(self.resultwidget)
        layout.addStretch()
        self.setLayout(layout)

        self.radioScenario.setChecked(True)
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
        if checkedid==0:
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
            self.parent().shared['mainaction'] = 'scenario'
            self.parent().shared['scenario'] = newscen
            return True
        if checkedid==1:
            self.parent().shared['mainaction'] = 'result'
            if 'result' in self.parent().shared:
                oldresult = self.parent().shared['result']
                if oldresult!=None: oldresult.unlink()
            try:
                newresult = self.resultwidget.getResult()
            except Exception,e:
                QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False
            self.parent().shared['result'] = newresult
            self.parent().shared['scenario'] = newresult.scenario
            return True
        return False

class ForkOnAction(commonqt.WizardFork):
    def getSequence(self):
        if self.wizard.shared['mainaction']=='scenario':
            return commonqt.WizardSequence([scenariobuilder.SequenceEditScenario(),simulator.PageProgress])
        else:
            return commonqt.WizardSequence([commonqt.WizardDummyPage])
def main():
    # Debug info
    print 'Python version: '+str(sys.version_info)
    print 'PyQt4 version: '+QtCore.PYQT_VERSION_STR
    print 'Qt version: '+QtCore.qVersion()

    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([' '])
    else:
        app = QtGui.qApp

    # Create wizard dialog
    wiz = commonqt.Wizard(closebutton=True)
    seq = commonqt.WizardSequence([PageIntroduction,PageChooseAction,ForkOnAction(wiz),visualizer.PageVisualize,visualizer.PageSave,visualizer.PageFinal])
    wiz.setSequence(seq)
    wiz.setWindowTitle('GOTM-GUI')
    wiz.resize(800, 600)
    wiz.show()

    ret = app.exec_()
    page = None

    wiz.unlink()

    sys.exit(ret)

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
