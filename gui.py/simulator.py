#!/usr/bin/python

#$Id: simulator.py,v 1.18 2008-02-18 20:33:49 jorn Exp $

from PyQt4 import QtGui,QtCore

import commonqt, core.common, core.simulator

# Here we can set the stack size for GOTM (in bytes). Note: bio modules sometimes
# need a very high stack size (in particular if Lagrangian variables are used)
stacksize = 16000000

class GOTMThread(QtCore.QThread):

  def __init__(self, parent):
    QtCore.QThread.__init__(self,parent)
    self.setStackSize(stacksize)
    
    self.scenario = None
    self.rwlock = QtCore.QReadWriteLock()
    self.stopped = False
    self.result = 0
    self.stderr = ''
    self.stdout = ''
    
  def rungotm(self,scen):
    self.scenario = scen
    #self.scenario = scen.convert(core.simulator.gotmscenarioversion)
    self.start(QtCore.QThread.LowPriority)
    
  def canContinue(self):
    self.rwlock.lockForRead()
    ret = not self.stopped
    self.rwlock.unlock()
    return ret
    
  def progressed(self,progress,remaining):
    self.emit(QtCore.SIGNAL('progressed(double,double)'),progress,remaining)
    
  def run(self):
    assert self.scenario!=None, 'No scenario specified.'

    self.res = core.simulator.simulate(self.scenario,continuecallback=self.canContinue,progresscallback=self.progressed)
    
  def stop(self):
    self.rwlock.lockForWrite()
    self.stopped = True
    self.rwlock.unlock()
    #self.scenario.release()
    
class PageProgress(commonqt.WizardPage):
    def __init__(self, parent):
        commonqt.WizardPage.__init__(self, parent)
        
        self.scenario = parent.getProperty('scenario')
        assert self.scenario!=None, 'No scenario available.'

        oldresult = parent.getProperty('result')
        
        layout = QtGui.QVBoxLayout()

        # Add label that asks user to wait
        self.busylabel = QtGui.QLabel('Please wait while the simulation runs...',self)
        self.busylabel.setVisible(oldresult==None)
        layout.addWidget(self.busylabel)
        
        # Add progress bar
        self.bar = QtGui.QProgressBar(self)
        self.bar.setRange(0,1000)
        self.bar.setVisible(oldresult==None)
        layout.addWidget(self.bar)
        
        # Add label for time remaining.
        self.labelRemaining = QtGui.QLabel(self)
        self.labelRemaining.setVisible(oldresult==None)
        layout.addWidget(self.labelRemaining)

        # Add (initially hidden) label for result.
        self.resultlabel = QtGui.QLabel('The simulation is complete.',self)
        self.resultlabel.setVisible(oldresult!=None)
        layout.addWidget(self.resultlabel)

        # Add (initially hidden) show/hide output button.
        self.showhidebutton = QtGui.QPushButton('Show diagnostic output',self)
        self.showhidebutton.setSizePolicy(QtGui.QSizePolicy.Fixed,QtGui.QSizePolicy.Fixed)
        self.showhidebutton.setVisible(oldresult!=None)
        layout.addWidget(self.showhidebutton)
        self.connect(self.showhidebutton, QtCore.SIGNAL('clicked()'),self.onShowHideOutput)

        # Add (initially hidden) text box for GOTM output.
        self.text = QtGui.QTextEdit(self)
        self.text.setLineWrapMode(QtGui.QTextEdit.NoWrap)
        self.text.setReadOnly(True)
        if oldresult!=None: self.text.setPlainText(oldresult.stderr)
        self.text.hide()
        layout.addWidget(self.text)
        layout.setStretchFactor(self.text,1)

        # Add (initially hidden) save-output button.
        self.savebutton = QtGui.QPushButton('Save output to file',self)
        self.savebutton.setSizePolicy(QtGui.QSizePolicy.Fixed,QtGui.QSizePolicy.Fixed)
        self.savebutton.hide()
        layout.addWidget(self.savebutton)
        self.connect(self.savebutton, QtCore.SIGNAL('clicked()'),self.onSaveOutput)

        layout.addStretch()
        
        self.setLayout(layout)
        
        # Initialize GOTM run variables.
        self.gotmthread = None
        self.tempdir = None
        self.bar.setValue(0)
       
    def showEvent(self,event):
        if self.owner.getProperty('result')==None: self.startRun()

    def startRun(self):
        self.gotmthread = GOTMThread(self)
        self.connect(self.gotmthread, QtCore.SIGNAL('progressed(double,double)'), self.progressed, QtCore.Qt.QueuedConnection)
        self.connect(self.gotmthread, QtCore.SIGNAL('finished()'), self.done, QtCore.Qt.QueuedConnection)
        self.gotmthread.rungotm(self.scenario)
        
    def progressed(self,progress,remaining):
        self.bar.setValue(int(round(self.bar.maximum()*progress)))
        remaining = round(remaining)
        if remaining<60:
            self.labelRemaining.setText('%i seconds remaining' % remaining)
        else:
            self.labelRemaining.setText('%i minutes %i seconds remaining' % divmod(remaining,60))
            
    def done(self):
        res = self.gotmthread.res
        if core.common.verbose: print 'GOTM thread shut-down; return code = %i' % res.returncode

        layout = self.layout()

        # Hide progress bar and remaining time.
        self.busylabel.hide()
        self.bar.hide()
        self.labelRemaining.hide()

        # Show label for result; change text if not successfull.
        if res.returncode==1:
            self.resultlabel.setText('The simulation failed: %s' % res.errormessage)
        elif res.returncode==2:
            self.resultlabel.setText('The simulation was cancelled')
        self.resultlabel.show()

        if res.returncode!=1:
            self.showhidebutton.show()
        else:
            self.text.show()
            self.savebutton.show()

        # Set text with GOTM output
        self.text.setPlainText(res.stderr)
        
        # Save result object
        if res.returncode==0:
            self.owner.setProperty('result',res)
            self.completeStateChanged()
        else:
            res.release()
        
    def isComplete(self):
        return (self.owner.getProperty('result')!=None)
    
    def saveData(self,mustbevalid):
        # Stop worker thread
        if self.gotmthread!=None:
            self.disconnect(self.gotmthread, QtCore.SIGNAL('progressed(double)'), self.progressed)
            self.disconnect(self.gotmthread, QtCore.SIGNAL('finished()'), self.done)
            self.gotmthread.stop()
            if not self.gotmthread.isFinished(): self.gotmthread.wait()
            self.gotmthread = None
            
        if not mustbevalid:
            # Remove any currently stored result.
            self.owner.setProperty('result',None)

        return True

    def onShowHideOutput(self):
        makevisible = self.text.isHidden()
        self.text.setVisible(makevisible)
        self.savebutton.setVisible(makevisible)
        curtext = unicode(self.showhidebutton.text())
        if makevisible:
            self.showhidebutton.setText(curtext.replace('Show','Hide'))
        else:
            self.showhidebutton.setText(curtext.replace('Hide','Show'))

    def onSaveOutput(self):
        path = unicode(QtGui.QFileDialog.getSaveFileName(self,'','','Text files (*.txt);;All files (*.*)'))
        if path=='': return
        f = open(path,'w')
        f.write(self.text.toPlainText())
        f.close()
