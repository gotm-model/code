#!/usr/bin/python
#$Id: simulator.py,v 1.2 2006-12-04 08:03:10 jorn Exp $
from PyQt4 import QtGui,QtCore
import common,commonqt
import os, tempfile, sys, math, shutil
import gotm
class GOTMThread(QtCore.QThread):
  def __init__(self, parent, scenariodir, receiver):
    QtCore.QThread.__init__(self,parent)
    self.scenariodir = scenariodir
    self.receiver = receiver
    self.rwlock = QtCore.QReadWriteLock()
    self.stopped = False
    self.result = 0
    self.stderr = ''
    self.stdout = ''
  def rungotm(self):
    self.start(QtCore.QThread.LowPriority)
  def run(self):
    # Result can be 0 (success), 1 (error occurred), or 2 (cancelled)
    self.result = 0
      
    # Save old working directory
    olddir = os.getcwdu()
    # Change to directory with GOTM scenario (catch exceptions that can occur,
    # for instance, if the specified directory does not exist).
    try:
        os.chdir(self.scenariodir)
    except Exception,e:
        self.error = 'Failed to enter scenario directory "' + self.scenariodir + '". ' + str(e)
        self.result = 1
        os.chdir(olddir)
        return
    # Redirect FORTRAN output to (temporary) files.
    (h,outfile) = tempfile.mkstemp('.txt','gotm')
    os.close(h)
    (h,errfile) = tempfile.mkstemp('.txt','gotm')
    os.close(h)
    gotm.gui_util.redirectoutput(outfile,errfile)
    # Initialize GOTM
    try:
        gotm.gotm.init_gotm()
    except Exception,e:
        self.error = 'Exception thrown while initializing GOTM: '+str(e)
        self.result = 1
    # Only enter the time loop if we succeeded so far.
    if self.result==0:
        # Calculate the size of time batches (small enough to respond rapidly to requests
        # for cancellation, and to show sufficiently detailed progress - e.g. in % -
        # but not so small that GUI slows down due to the avalanche of progress notifications)
        visualres = 0.01
        start = gotm.time.minn*1
        stop  = gotm.time.maxn*1
        stepcount = stop-start+1
        visualstep = int(math.floor(visualres*stepcount))
        visualstepcount = int(math.ceil(stepcount/float(visualstep)))
        for istep in range(1,visualstepcount+1):
            # Check if we have to cancel
            rl = QtCore.QReadLocker(self.rwlock)
            if self.stopped:
                print 'GOTM run was cancelled; exiting thread...'
                self.result = 2
                break
            rl.unlock()
            # Configure GOTM for new batch.
            gotm.time.minn = start + (istep-1)*visualstep
            gotm.time.maxn = start +     istep*visualstep - 1
            if istep==visualstepcount: gotm.time.maxn = stop
            # Process time batch
            try:
                gotm.gotm.time_loop()
            except Exception,e:
                self.error = 'Exception thrown in GOTM time loop: '+str(e)
                self.result = 1
                break
            # Send 'progress' event
            prog = gotm.time.maxn/float(stop)
            self.emit(QtCore.SIGNAL("progressed(double)"), prog)
    # GOTM clean-up
    try:
        gotm.gotm.clean_up()
    except Exception,e:
        self.error = 'Exception thrown during GOTM clean-up: '+str(e)
        if self.result==0: self.result = 1
    # Reset FORTRAN output
    gotm.gui_util.resetoutput()
    # Read GOTM output from temporary files, then delete these files.
    f = open(errfile,'r')
    self.stderr = f.read()
    f.close()
    os.remove(errfile)
    f = open(outfile,'r')
    self.stdout = f.read()
    f.close()
    os.remove(outfile)
    # Return to previous working directory.
    os.chdir(olddir)
  def stop(self):
    wl = QtCore.QWriteLocker(self.rwlock)
    self.stopped = True
class PageProgress(commonqt.WizardPage):
    def __init__(self, parent):
        commonqt.WizardPage.__init__(self, parent)
        self.scenario = parent.shared['scenario']
        layout = QtGui.QVBoxLayout()
        # Progress bar
        self.bar = QtGui.QProgressBar(self)
        self.bar.setMinimum(0)
        self.bar.setMaximum(1000)
        layout.addWidget(self.bar)
        # Add label for time remaining.
        self.labelRemaining = QtGui.QLabel(self)
        layout.addWidget(self.labelRemaining)
        # Text window to display GOTM output in.
        self.text = QtGui.QTextEdit(self)
        self.text.setLineWrapMode(QtGui.QTextEdit.NoWrap)
        self.text.setReadOnly(True)
        layout.addWidget(self.text)
        self.setLayout(layout)
        # Initialize GOTM run variables.
        self.gotmthread = None
        self.runcount = 1
        self.bar.setValue(0)
        self.result = None
    def showEvent(self,event):
        namelistscenario = self.scenario.convert(common.gotmscenarioversion,targetownstemp=False)
        self.tempdir = tempfile.mkdtemp('','gotm-')
        namelistscenario.setProperty(['gotmrun','output','out_fmt'],2)
        namelistscenario.setProperty(['gotmrun','output','out_dir'],'.')
        namelistscenario.setProperty(['gotmrun','output','out_fn'],'result')
        namelistscenario.writeAsNamelists(self.tempdir)
        namelistscenario.unlink()
        self.timer = QtCore.QTime()
        self.timer.start()
        self.gotmthread = GOTMThread(self,self.tempdir,self)
        self.connect(self.gotmthread, QtCore.SIGNAL("progressed(double)"), self.progressed, QtCore.Qt.QueuedConnection)
        self.connect(self.gotmthread, QtCore.SIGNAL("finished()"), self.done, QtCore.Qt.QueuedConnection)
        self.gotmthread.rungotm()
    def progressed(self,progress):
        #print 'progress = '+str(progress)
        self.bar.setValue(int(round(self.bar.maximum()*progress)))
        remaining = (1-progress)*self.timer.elapsed()/1000/progress
        if remaining<60:
            self.labelRemaining.setText('%i seconds remaining' % round(remaining))
        else:
            self.labelRemaining.setText('%i minutes %i seconds remaining' % (math.floor(remaining/60),round(remaining % 60)))
    def done(self):
        success = self.gotmthread.result
        print 'GOTM thread shut-down; return code = '+str(success)
        # Display GOTM output (append if error message if an error occurred)
        restext = ''
        if success==1:
            restext += 'Error: '+self.gotmthread.error + '\n'
        elif success==2:
            restext += 'GOTM run was cancelled.\n\n'
        if len(self.gotmthread.stderr)>0: restext += 'GOTM output:\n'+self.gotmthread.stderr
        self.text.setPlainText(restext)
        # Create result object
        if success==0:
            self.result = common.Result()
            self.result.attach(os.path.join(self.tempdir,'result.nc'),self.scenario)
            self.completeStateChanged()
        # For debugging purposes only: start GOTM again.
        self.runcount -= 1
        if self.runcount>0: self.startGotm(self.scendir)
    def isComplete(self):
        return (self.result!=None)
    def saveData(self,mustbevalid):
        # Stop worker thread
        if self.gotmthread!=None:
            self.gotmthread.stop()
            if not self.gotmthread.isFinished(): self.gotmthread.wait()
        # Store result (if needed and available)
        if (mustbevalid and self.result!=None):
            self.parent().shared['result'] = self.result
        # Remove temporary directory
        if self.tempdir!=None:
            try:
                shutil.rmtree(self.tempdir)
            except Exception,e:
                print 'Unable to completely remove GOTM temporary directory "'+self.tempdir+'".\nError: '+str(e)
            self.tempdir = None
        return True
