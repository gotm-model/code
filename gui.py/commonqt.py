#!/usr/bin/python

#$Id: commonqt.py,v 1.56 2008-03-11 10:16:00 jorn Exp $

# Import modules from standard Python (>= 2.4) library
import datetime, re, os.path, sys

# Import third-party modules
from PyQt4 import QtGui,QtCore
import matplotlib.figure, pytz
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_agg import FigureCanvasAgg

# Import our own custom modules
import xmlstore.xmlstore,xmlstore.util
import xmlplot.plot,xmlplot.data,xmlplot.common
import core.common,core.settings

radiowidth = None
def getRadioWidth():
    """Get the width of a radio button without text.
    Used to left align object below radio buttons to the position of the
    text associated with the radio button.
    """
    global radiowidth
    if radiowidth==None:
        radiowidth = QtGui.qApp.style().pixelMetric(QtGui.QStyle.PM_ExclusiveIndicatorWidth)
    return radiowidth
        
# =======================================================================
# Utilities for redirecting stderr (i.e., Python errors and tracebacks)
# to a Qt-based dialog.
# =======================================================================

class ErrorReceiver(QtCore.QObject):
    """Objects of this class accept an ErrorEvent (contained), takes the contained error message
    and passes it to ErrorDialog for showing the error in the GUI. Errors can be posted from
    any thread (not necessarily the GUI thread) using QtGui.QApplication.postEvent.
    """
    def __init__(self):
        QtCore.QObject.__init__(self)
    
    class ErrorEvent(QtCore.QEvent):
        """Custom class derived from QEvent, used only to contain a string (error message).
        """
        def __init__(self,error):
            QtCore.QEvent.__init__(self,QtCore.QEvent.User)
            self.error = error

    def event(self,ev):
        """Ignores all event except those of type QtCore.QEvent.User. Events of this type must
        be objects of class ErrorEvent. Their contained string is passed to the ErrorDialog for
        display.
        """
        if ev.type()==QtCore.QEvent.User:
            ErrorDialog.postError(ev.error)
            return True
        return QtGui.QWidget.event(self,ev)
        
# The one and only errorreceiver object, responsible for receiving posted errors from all threads.
# Posted errors will be passed to an ErrorDialog.
errorreceiver = ErrorReceiver()

def redirect_stderr():
    """Redirects all text written to stderr to a contained class that posts the error
    message to the one and only errorreceiver object. The posting mechanism allows the
    error message to be passed between threads.
    """
    class Stderr(object):
        """Customized stderr; when a string is written to this object, it is posted
        to the one and only errorreceiver object.
        """
        softspace = 0   # Must be provided by file-like objects
        def write(self, text):
            QtGui.QApplication.postEvent(errorreceiver,ErrorReceiver.ErrorEvent(text))
        def flush(self):
            pass
    sys.stderr = Stderr()

class ErrorDialog(QtGui.QWidget):
    errdlg = None

    @staticmethod
    def postError(string):
        """Creates the one and only ErrorDialog if it does not exist yet, then shows the
        provided error message in the dialog.
        """
        if ErrorDialog.errdlg==None:
            ErrorDialog.errdlg = ErrorDialog()
        ErrorDialog.errdlg.write(string)
        ErrorDialog.errdlg.show()
    
    def __init__(self,parent=None):
        if parent==None: parent = QtGui.QApplication.activeWindow()
        QtGui.QWidget.__init__(self,parent,QtCore.Qt.Tool)

        self.labelStart = QtGui.QLabel('Errors occurred during execution of GOTM-GUI:',self)
        self.labelStart.setWordWrap(True)
        self.labelStop = QtGui.QLabel('You may be able to continue working. However, we would appreciate it if you report this error. To do so, send an e-mail to <a href="mailto:gotm-users@gotm.net">gotm-users@gotm.net</a> with the above error message, and the circumstances under which the error occurred.',self)
        self.labelStop.setOpenExternalLinks(True)
        self.labelStop.setWordWrap(True)

        self.textedit = QtGui.QTextEdit(self)
        self.textedit.setLineWrapMode(QtGui.QTextEdit.NoWrap)
        self.textedit.setReadOnly(True)
        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.labelStart)
        layout.addWidget(self.textedit)
        layout.addWidget(self.labelStop)
        self.setLayout(layout)
        
        self.setWindowTitle('Errors occurred')
        self.resize(600, 200)

        if parent!=None:
            self.setAttribute(QtCore.Qt.WA_QuitOnClose,False)
            
    def write(self,string):
        cur = self.textedit.textCursor()
        cur.movePosition(QtGui.QTextCursor.End)
        cur.insertText(string)

# =======================================================================
# Function for showing a Qt-based file/directory browse dialog
# =======================================================================

def browseForPath(parent=None,curpath=None,getdirectory=False,save=False,filter='',dlgoptions=None):
    """Shows browse dialog for opening/saving a file, or selecting a directory.
    Supports automatic append of file extension based on chosen file type.
    """
    if curpath==None: curpath=''
    if dlgoptions==None: dlgoptions = QtGui.QFileDialog.Option()
    if getdirectory:
        path = unicode(QtGui.QFileDialog.getExistingDirectory(parent,'',curpath))
    elif save:
        selfilt = QtCore.QString()
        path = unicode(QtGui.QFileDialog.getSaveFileName(parent,'',curpath,filter,selfilt,dlgoptions))
        selfilt = unicode(selfilt)
    else:
        path = unicode(QtGui.QFileDialog.getOpenFileName(parent,'',curpath,filter))
        
    # If the browse dialog was cancelled, just return.
    if path=='': return None

    # if we are saving, make sure that the extension matches the filter selected.
    if save:
        re_ext = re.compile('\*\.(.+?)[\s)]')
        exts = []
        pos = 0
        match = re_ext.search(selfilt,pos)
        goodextension = False
        while match!=None:
            ext = match.group(1)
            if ext!='*':
                exts.append(ext)
                if path.endswith(ext): goodextension = True
            pos = match.end(0)
            match = re_ext.search(selfilt,pos)

        # Append first imposed extension
        if not goodextension and len(exts)>0: path += '.'+exts[0]
    
    return os.path.normpath(path)

# =======================================================================
# PathEditor: a Qt widget for editing paths, combines line-edit widget
# for path name, and a browse button.
# =======================================================================

class PathEditor(QtGui.QWidget):
    def __init__(self,parent=None,compact=False,header=None,getdirectory=False,save=False,mrupaths=[]):
        QtGui.QWidget.__init__(self, parent)

        if compact:
            text = '...'
        else:
            text = 'Browse...'

        lo = QtGui.QHBoxLayout()

        if header!=None:
            self.header = QtGui.QLabel(header,self)
            lo.addWidget(self.header)

        if len(mrupaths)>0:
            # One or more recently used paths: use a combobox for the path.
            self.editor = QtGui.QComboBox(self)
            lo.addWidget(self.editor)
            self.editor.setEditable(True)
            self.editor.setSizePolicy(QtGui.QSizePolicy.Expanding,QtGui.QSizePolicy.Fixed)
            for p in mrupaths: self.editor.addItem(p)
            self.editor.setEditText('')
            self.defaultpath = os.path.dirname(mrupaths[0])
            self.lineedit = self.editor.lineEdit()
        else:
            # No recently used paths: use a line edit control for the path.
            self.lineedit = QtGui.QLineEdit(self)
            lo.addWidget(self.lineedit)
            self.defaultpath = None

        self.browsebutton = QtGui.QPushButton(text,self)
        lo.addWidget(self.browsebutton)

        lo.setMargin(0)

        self.setLayout(lo)

        self.connect(self.lineedit,     QtCore.SIGNAL('textChanged(const QString &)'), self.onChanged)
        self.connect(self.lineedit,     QtCore.SIGNAL('editingFinished()'),            self.onEditingFinished)
        self.connect(self.browsebutton, QtCore.SIGNAL('clicked()'),                    self.onBrowse)

        self.getdirectory = getdirectory
        self.save = save

        self.filter=''

        self.dlgoptions = QtGui.QFileDialog.DontConfirmOverwrite

    def setPath(self,path):
        return self.lineedit.setText(path)
        #return self.editor.setEditText(path)

    def path(self):
        return unicode(self.lineedit.text())
        #return unicode(self.editor.currentText())

    def onBrowse(self):
        curpath = self.path()
        if curpath=='' and self.defaultpath != None: curpath=self.defaultpath
        path = browseForPath(self,curpath=curpath,getdirectory=self.getdirectory,save=self.save,filter=self.filter,dlgoptions=self.dlgoptions)
        if path!=None: self.setPath(path)

    def hasPath(self):
        return (len(self.path())>0)

    def onChanged(self,text):
        self.emit(QtCore.SIGNAL('onChanged()'))

    def onEditingFinished(self):
        self.emit(QtCore.SIGNAL('editingFinished()'))

# =======================================================================
# Wizard: dialog for hosting series of 'wizard' pages
#   based on Qt example of a complex wizard
#   pages must inherit from class WizardPage below.
# =======================================================================

class Wizard(QtGui.QDialog):
    
    def __init__(self,parent=None,sequence=None,closebutton=False,headerlogo=None):
        QtGui.QDialog.__init__(self, parent, QtCore.Qt.Window|QtCore.Qt.WindowContextHelpButtonHint)

        layout = QtGui.QVBoxLayout()
        layout.setMargin(0)
        
        if headerlogo!=None:
            self.pm = QtGui.QPixmap(headerlogo,'PNG')
            self.piclabel = QtGui.QLabel(self)
            self.piclabel.setPixmap(self.pm)
            self.piclabel.setScaledContents(True)
            layout.addWidget(self.piclabel)

        self.bnlayout = QtGui.QHBoxLayout()
        self.bnlayout.addStretch()

        self.bnHome = QtGui.QPushButton('&Home',self)
        self.connect(self.bnHome, QtCore.SIGNAL('clicked()'), self.onHome)
        self.bnlayout.addWidget(self.bnHome)

        self.bnBack = QtGui.QPushButton('< &Back',self)
        self.connect(self.bnBack, QtCore.SIGNAL('clicked()'), self.onBack)
        self.bnlayout.addWidget(self.bnBack)

        self.bnNext = QtGui.QPushButton('&Next >',self)
        self.connect(self.bnNext, QtCore.SIGNAL('clicked()'), self.onNext)
        self.bnlayout.addWidget(self.bnNext)

        if closebutton:
            self.bnClose = QtGui.QPushButton('&Close',self)
            self.connect(self.bnClose, QtCore.SIGNAL('clicked()'), self.accept)
            self.bnlayout.addWidget(self.bnClose)

        self.bnlayout.setMargin(11)
        layout.addLayout(self.bnlayout)

        self.setLayout(layout)

        self.shared = {}

        self.settings = core.settings.SettingsStore()

        self.sequence = sequence
        self.currentpage = None

    def getProperty(self,propertyname):
        if propertyname not in self.shared: return None
        return self.shared[propertyname]

    def setProperty(self,propertyname,value):
        if propertyname in self.shared and isinstance(self.shared[propertyname],xmlstore.util.referencedobject):
            self.shared[propertyname].release()
        self.shared[propertyname] = value
        self.onPropertyChange(propertyname)
        
    def onPropertyChange(self,propertyname):
        pass
        
    def clearProperties(self):
        for propertyname in self.shared.keys():
            self.setProperty(propertyname,None)

    def destroy(self, destroyWindow = True, destroySubWindows = True):
        self.settings.save()
        self.settings.release()
        self.settings = None
        for v in self.shared.values():
            try:
                v.release()
            except:
                pass
        if self.currentpage!=None:
            self.currentpage.destroy()
        QtGui.QDialog.destroy(self,destroyWindow,destroySubWindows)

    def setSequence(self,sequence):
        self.sequence = sequence
        cls = self.sequence.getNextPage()
        self.switchPage(cls(self))

    def onNext(self,askoldpage=True):
        if askoldpage:
            oldpage = self.currentpage
            if not oldpage.saveData(mustbevalid=True): return
        
        ready = False
        while not ready:
            cls = self.sequence.getNextPage()
            assert cls!=None, 'No next page available to show; the next button should have been disabled.'
            newpage = cls(self)
            ready = (not newpage.doNotShow())
        self.switchPage(newpage)

    def onBack(self):
        oldpage = self.currentpage
        if not oldpage.saveData(mustbevalid=False): return
        ready = False
        while not ready:
            cls = self.sequence.getPreviousPage()
            assert cls!=None, 'No previous page available to show; the back button should have been disabled.'
            newpage = cls(self)
            ready = (not newpage.doNotShow())
        self.switchPage(newpage)

    def onHome(self):
        oldpage = self.currentpage
        if not oldpage.saveData(mustbevalid=False): return
        cls = self.sequence.getPreviousPage()
        assert cls!=None, 'No previous page available to show; the home button should have been disabled.'
        while cls!=None:
            prevcls = cls
            cls = self.sequence.getPreviousPage()
        newpage = prevcls(self)
        self.switchPage(newpage)

    def switchPage(self,newpage):
        layout = self.layout()
        if self.currentpage!=None:
            self.currentpage.hide()
            layout.removeWidget(self.currentpage)
            self.disconnect(self.currentpage, QtCore.SIGNAL('onCompleteStateChanged()'),self.onCompleteStateChanged)
            self.currentpage.destroy()
        self.currentpage = newpage
        layout.insertWidget(1,self.currentpage)
        self.currentpage.show()
        self.connect(self.currentpage, QtCore.SIGNAL('onCompleteStateChanged()'),self.onCompleteStateChanged)
        cangoback = (self.sequence.getPreviousPage(stay=True)!=None)
        self.bnHome.setEnabled(cangoback)
        self.bnBack.setEnabled(cangoback)
        self.onCompleteStateChanged()

    def onCompleteStateChanged(self):
        curpage = self.currentpage
        enable = (curpage.isComplete() and self.sequence.getNextPage(stay=True)!=None)
        self.bnNext.setEnabled(enable)

# =======================================================================
# WizardPage: single page for the above Wizard class
#   based on Qt example of a complex wizard
# =======================================================================

class WizardPage(QtGui.QWidget):

    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)
        self.owner = parent
        self.hide()

    def isComplete(self):
        return False

    def completeStateChanged(self):
        self.emit(QtCore.SIGNAL('onCompleteStateChanged()'))

    def saveData(self,mustbevalid):
        return True

    def doNotShow(self):
        return False

    def createHeader(self,title,description):
        label = QtGui.QLabel('<span style="font-size:large;font-weight:bold;">%s</span><hr>%s' % (title,description),self)
        label.setWordWrap(True)
        return label
        
    def destroy(self,destroyWindow = True,destroySubWindows = True):
        QtGui.QWidget.destroy(self,destroyWindow,destroySubWindows)

class WizardDummyPage(WizardPage):
    def doNotShow(self):
        return True

class WizardSequence:

    def __init__(self,items=[]):
        self.items = items
        self.index = -1

    def getCurrentPage(self):
        if self.index==-1: return None
        cur = self.items[self.index]
        if isinstance(cur,WizardSequence):
            return cur.getCurrentPage()
        else:
            return cur

    def getNextPage(self,stay=False):
        if self.index==-1:
            if len(self.items)==0: raise Exception('WizardSequence contains no items')
        elif isinstance(self.items[self.index],WizardSequence):
            new = self.items[self.index].getNextPage(stay=stay)
            if new!=None:
                return new
            elif not stay:
                self.items[self.index].reset()
        if self.index>=(len(self.items)-1): return None
        ind = self.index + 1
        if not stay: self.index = ind
        new = self.items[ind]
        if isinstance(new,WizardSequence):
            return new.getNextPage(stay=stay)
        else:
            return new

    def getPreviousPage(self,stay=False):
        if self.index==-1:
            if len(self.items)==0: raise Exception('WizardSequence contains no items')
        elif isinstance(self.items[self.index],WizardSequence):
            new = self.items[self.index].getPreviousPage(stay=stay)
            if new!=None:
                return new
            elif not stay:
                self.items[self.index].reset()
        if self.index==0: return None
        
        if self.index==-1:
            ind = len(self.items)-1
        else:
            ind = self.index - 1
        if not stay: self.index = ind
        new = self.items[ind]
        if isinstance(new,WizardSequence):
            return new.getPreviousPage(stay=stay)
        else:
            return new
        
    def reset(self):
        self.index = -1

class WizardFork(WizardSequence):
    def __init__(self,wiz):
        WizardSequence.__init__(self,[])
        self.wizard = wiz

    def getNextPage(self,stay=False):
        if stay: return WizardSequence()
        if self.index==-1:
            seq = self.getSequence()
            assert seq!=None, 'Fork did not return a new sequence'
            self.items = [seq]
        return WizardSequence.getNextPage(self,stay=False)

    def getSequence(self):
        return None
        
# =======================================================================
# ProgressDialog: generic progress dialog that receives progress messages
# via a callback with 2 arguments: (1) progress as float between 0 and 1,
# (2) a string describing the task currently being performed.
# =======================================================================

class ProgressDialog(QtGui.QProgressDialog):
    def __init__(self,parent=None,minimumduration=500,title=None,suppressstatus=False):
        QtGui.QProgressDialog.__init__(self,'',QtCore.QString(),0,0,parent,QtCore.Qt.Dialog|QtCore.Qt.WindowTitleHint|QtCore.Qt.MSWindowsFixedSizeDialogHint)
        self.setModal(True)
        self.setMinimumDuration(minimumduration)
        self.setRange(0,0)
        self.suppressstatus = suppressstatus
        if title!=None: self.setWindowTitle(title)
        QtGui.QApplication.setOverrideCursor(QtGui.QCursor(QtCore.Qt.WaitCursor))
            
    def onProgressed(self,progress,status):
        if progress!=None:
            if self.maximum()==0: self.setMaximum(100)
            self.setValue(int(100*progress))
        elif progressdialog.maximum()!=0:
            self.setValue(0)
        if not self.suppressstatus: self.setLabelText(status)
        QtGui.qApp.processEvents()

    def close(self):
        QtGui.QApplication.restoreOverrideCursor()
        QtGui.QProgressDialog.close(self)
        