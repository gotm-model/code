#!/usr/bin/python

#$Id: commonqt.py,v 1.48 2007-12-14 14:21:01 jorn Exp $

from PyQt4 import QtGui,QtCore
import datetime, re, os.path, sys

import common,xmlstore,settings,plot,data

import matplotlib.figure, pytz
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_agg import FigureCanvasAgg

def getTopLevelWidget(child):
    parent = child.parent()
    if parent==None or (child.windowFlags() & QtCore.Qt.Window): return child
    return getTopLevelWidget(parent)

radiowidth = None
def getRadioWidth():
    global radiowidth
    if radiowidth==None:
        radiowidth = QtGui.QRadioButton().sizeHint().width()
    return radiowidth
    
def addCloseButton():
    return sys.platform!='win32'
    
# =======================================================================
# Functions for converting between Qt date/time object and Python
# date/time objects
# =======================================================================

# qtdatetime2datetime: Convert Qt QDateTime object to Python datetime object
def qtdatetime2datetime(qtdatetime):
    qdt = qtdatetime.toUTC()
    d = qdt.date()
    t = qdt.time()
    return datetime.datetime(d.year(),d.month(),d.day(),t.hour(),t.minute(),t.second(),tzinfo=common.utc)

# datetime2qtdatetime: Convert Python datetime object to Qt QDateTime object
def datetime2qtdatetime(dt):
    tm = dt.utctimetuple()
    return QtCore.QDateTime(QtCore.QDate(tm.tm_year,tm.tm_mon,tm.tm_mday),QtCore.QTime(tm.tm_hour,tm.tm_min,tm.tm_sec),QtCore.Qt.UTC)

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
# LinkedFilePlotDialog: a Qt widget for viewing the data files linked
# to a GOTM scenario. This widget displays series of data points or
# profiles in time, and allows for import or export of the data.
# =======================================================================

class LinkedFilePlotDialog(QtGui.QDialog):
    class LinkedDataModel(QtCore.QAbstractItemModel):
        def __init__(self,datastore,type=0):
            QtCore.QAbstractItemModel.__init__(self)
            self.datastore = datastore
            self.type = type
            self.pos = 0
            
            self.datamatrix = None
            self.rowlabels = None
            self.datelabels = True
            
            self.loadData()

        def loadData(self):
            rawdata = self.datastore.data
            self.rowlabels = None
            self.datamatrix = None
            if isinstance(self.datastore,data.LinkedMatrix):
                self.datamatrix = rawdata[-1]
                if len(self.datastore.dimensions)==1:
                    self.rowlabels = rawdata[0]
                    dimname = self.datastore.getDimensionNames()[0]
                    self.datelabels = (self.datastore.getDimensionInfo(dimname)['datatype']=='datetime')
            elif isinstance(self.datastore,data.LinkedProfilesInTime):
                if self.type==0:
                    if self.pos<len(rawdata[1]):
                        self.rowlabels = rawdata[1][self.pos]
                        self.datamatrix = rawdata[2][self.pos]
                        self.datelabels = False
                else:
                    self.rowlabels = rawdata[0]
                    self.datelabels = True
            else:
                assert False, 'Unknown data file type "%s".' % self.datastore.type
                
        def saveData(self):
            rawdata = self.datastore.data
            if isinstance(self.datastore,data.LinkedMatrix):
                if len(self.datastore.dimensions)==1:
                    rawdata[0] = self.rowlabels
                rawdata[-1] = self.datamatrix
            elif isinstance(self.datastore,data.LinkedProfilesInTime):
                if self.type==0:
                    if self.pos<len(rawdata[1]):
                        rawdata[1][self.pos] = self.rowlabels
                        rawdata[2][self.pos] = self.datamatrix
                else:
                    rawdata[0] = self.rowlabels
            else:
                assert False, 'Unknown data file type "%s".' % self.datastore.type
            
        def reset(self):
            self.loadData()
            QtCore.QAbstractItemModel.reset(self)
            
        def index(self,irow,icolumn,parent=None):
            if parent==None: parent=QtCore.QModelIndex()
            assert not parent.isValid(), 'Only the root can have child nodes.'
            return self.createIndex(irow,icolumn)

        def parent(self,index):
            return QtCore.QModelIndex()
            
        def rowCount(self,parent=None):
            if parent==None: parent=QtCore.QModelIndex()
            if parent.isValid(): return 0
            if self.rowlabels!=None:
                return self.rowlabels.shape[0]
            elif self.datamatrix!=None:
                return self.datamatrix.shape[0]
            else:
                return 0

        def columnCount(self,parent=None):
            if parent==None: parent=QtCore.QModelIndex()
            colcount = 0
            if self.rowlabels!=None:
                colcount += 1
            if self.datamatrix!=None:
                colcount += self.datamatrix.shape[1]
            return colcount

        def data(self,index,role=QtCore.Qt.DisplayRole):
            if role==QtCore.Qt.DisplayRole or role==QtCore.Qt.EditRole:
                rowindex = index.row()
                colindex = index.column()
                if self.rowlabels!=None: colindex -= 1
                if colindex==-1:
                    if self.datelabels:
                        val = common.num2date(self.rowlabels[rowindex])
                    else:
                        val = self.rowlabels[rowindex]
                else:
                    val = self.datamatrix[rowindex,colindex]

                if role==QtCore.Qt.DisplayRole:
                    if isinstance(val,datetime.datetime):
                        return QtCore.QVariant(val.strftime(common.datetime_displayformat))
                    else:
                        return QtCore.QVariant('%.6g' % float(val))
                        
                if isinstance(val,datetime.datetime):
                    val = datetime2qtdatetime(val)
                else:
                    val = float(val)
                    
                return QtCore.QVariant(val)
                
            return QtCore.QVariant()
            
        def setData(self,index,value,role=QtCore.Qt.EditRole):
            if role==QtCore.Qt.EditRole:
                if value.canConvert(QtCore.QVariant.DateTime):
                    value = value.toDateTime()
                    value.setTimeSpec(QtCore.Qt.UTC)
                    value = common.date2num(qtdatetime2datetime(value))
                elif value.canConvert(QtCore.QVariant.Double):
                    (value,ok) = value.toDouble()
                else:
                    assert False, 'Do not know variant type %s.' % val.type()
                rowindex = index.row()
                colindex = index.column()
                if self.rowlabels!=None: colindex -= 1
                if colindex==-1:
                    if self.rowlabels[rowindex]==value: return True
                    newrowindex = self.rowlabels.searchsorted(value)
                    self.rowlabels[rowindex] = value
                    if newrowindex!=rowindex and newrowindex!=rowindex+1:
                        buflab = self.rowlabels[rowindex]
                        bufdata = self.datamatrix[rowindex,:].copy()
                        if newrowindex>rowindex+1:
                            self.rowlabels[rowindex:newrowindex-1] = self.rowlabels[rowindex+1:newrowindex]
                            self.rowlabels[newrowindex-1] = buflab
                            if self.datamatrix!=None:
                                self.datamatrix[rowindex:newrowindex-1,:] = self.datamatrix[rowindex+1:newrowindex,:]
                                self.datamatrix[newrowindex-1,:] = bufdata
                            if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type!=0:
                                self.datastore.data[1].insert(newrowindex-1,self.datastore.data[1].pop(rowindex))
                                self.datastore.data[2].insert(newrowindex-1,self.datastore.data[2].pop(rowindex))
                            self.emitRowsChanged(rowindex,newrowindex-1)
                        elif newrowindex<rowindex:
                            self.rowlabels[newrowindex+1:rowindex+1] = self.rowlabels[newrowindex:rowindex]
                            self.rowlabels[newrowindex] = buflab
                            if self.datamatrix!=None:
                                self.datamatrix[newrowindex+1:rowindex+1,:] = self.datamatrix[newrowindex:rowindex,:]
                                self.datamatrix[newrowindex,:] = bufdata
                            if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type!=0:
                                self.datastore.data[1].insert(newrowindex,self.datastore.data[1].pop(rowindex))
                                self.datastore.data[2].insert(newrowindex,self.datastore.data[2].pop(rowindex))
                            self.emitRowsChanged(newrowindex,rowindex)
                else:
                    if self.datamatrix[rowindex,colindex]==value: return True
                    self.datamatrix[rowindex,colindex] = value
                self.datastore.dataChanged()
                self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex &,const QModelIndex &)'),index,index)
            return True
            
        def addRow(self):
            newrowindex = self.datamatrix.shape[0]
            self.beginInsertRows(QtCore.QModelIndex(),newrowindex,newrowindex)
            if self.datamatrix!=None:
                newrow = matplotlib.numerix.zeros((1,self.datamatrix.shape[1]),matplotlib.numerix.typecode(self.datamatrix))
                self.datamatrix = matplotlib.numerix.concatenate((self.datamatrix,newrow))
            if self.rowlabels!=None:
                if self.datelabels:
                    if newrowindex==0:
                        newval = common.date2num(datetime.datetime.today())
                    else:
                        newval = self.rowlabels[-1]
                        if newrowindex>1: newval += self.rowlabels[-1]-self.rowlabels[-2]
                else:
                    newval = 0.
                self.rowlabels = matplotlib.numerix.concatenate((self.rowlabels,[newval]))
            self.saveData()
            self.datastore.dataChanged()
            self.endInsertRows()
            
        def removeRow(self,start,stop=None):
            if stop==None: stop=start
            self.beginRemoveRows(QtCore.QModelIndex(),start,stop)
            if self.datamatrix!=None:
                self.datamatrix = matplotlib.numerix.concatenate((self.datamatrix[0:start,:],self.datamatrix[stop+1:,:]))
            if self.rowlabels!=None:
                self.rowlabels = matplotlib.numerix.concatenate((self.rowlabels[0:start],self.rowlabels[stop+1:]))
            self.saveData()
            self.datastore.dataChanged()
            self.endRemoveRows()
            
        def emitRowsChanged(self,start,stop):
            startindex = self.index(start,0)
            stopindex = self.index(stop,self.columnCount()-1)
            self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex &,const QModelIndex &)'),startindex,stopindex)
            
        def flags(self,index):
            return QtCore.Qt.ItemIsEnabled | QtCore.Qt.ItemIsSelectable | QtCore.Qt.ItemIsEditable
            
        def headerData(self,section, orientation, role=QtCore.Qt.DisplayRole):
            if orientation==QtCore.Qt.Horizontal and role==QtCore.Qt.DisplayRole:
                if isinstance(self.datastore,data.LinkedMatrix):
                    if len(self.datastore.dimensions)==1: section-=1
                    if section==-1:
                        val = self.datastore.getDimensionNames()[0]
                    else:
                        val = self.datastore.getVariableNames()[section]
                if isinstance(self.datastore,data.LinkedProfilesInTime) and self.type==0:
                    if section==0:
                        val = 'depth'
                    else:
                        val = self.datastore.getVariableNames()[section-1]
                return QtCore.QVariant(val)
            return QtCore.QVariant()
    
    class LinkedFileDelegate(QtGui.QItemDelegate):

        def __init__(self,parent=None):
            QtGui.QItemDelegate.__init__(self,parent)

        # createEditor (inherited from QtGui.QItemDelegate)
        #   Creates the editor widget for the model item at the given index
        def createEditor(self, parent, option, index):
            val = index.data(QtCore.Qt.EditRole)
            type = val.type()
            if type==QtCore.QVariant.Double:
                editor = ScientificDoubleEditor(parent)
                self.currenteditor = editor
            elif type==QtCore.QVariant.DateTime:
                editor = QtGui.QDateTimeEdit(parent)

            # Install event filter that captures key events for view from the editor (e.g. return press).
            editor.installEventFilter(self)
            
            return editor
            
        # setEditorData (inherited from QtGui.QItemDelegate)
        #   Sets value in the editor widget, for the model item at the given index
        def setEditorData(self, editor,index):
            value = index.data(QtCore.Qt.EditRole)
            if not value.isValid(): return
            type = value.type()
            if type==QtCore.QVariant.Double:
                value,ret = value.toDouble()
                editor.setValue(value)
            elif type==QtCore.QVariant.DateTime:
                value = value.toDateTime()
                editor.setDateTime(value)

        # setModelData (inherited from QtGui.QItemDelegate)
        #   Obtains the value from the editor widget, and set it for the model item at the given index
        def setModelData(self, editor, model, index):
            if isinstance(editor,ScientificDoubleEditor):
                editor.interpretText()
                model.setData(index, QtCore.QVariant(editor.value()))
            elif isinstance(editor,QtGui.QDateTimeEdit):
                model.setData(index, QtCore.QVariant(editor.dateTime()))

    def __init__(self,node,parent=None,datafile=None):
        QtGui.QDialog.__init__(self,parent)

        self.node = node
        self.varstore = data.LinkedFileVariableStore.fromNode(self.node)

        lo = QtGui.QGridLayout()
        
        loLeft = QtGui.QVBoxLayout()

        # Left panel: data editor
        loDataEdit = QtGui.QHBoxLayout()
        if isinstance(self.varstore,data.LinkedProfilesInTime):
            self.listTimes = QtGui.QListView(self)
            self.listmodel = LinkedFilePlotDialog.LinkedDataModel(self.varstore,type=1)
            self.listTimes.setSelectionMode(QtGui.QAbstractItemView.ExtendedSelection)
            self.listTimes.setModel(self.listmodel)
            loDataEdit.addWidget(self.listTimes)
            self.connect(self.listTimes.selectionModel(), QtCore.SIGNAL('currentChanged(const QModelIndex &,const QModelIndex &)'), self.onTimeChanged)
        self.tableData = QtGui.QTableView(self)
        self.tablemodel = LinkedFilePlotDialog.LinkedDataModel(self.varstore)
        self.connect(self.tablemodel, QtCore.SIGNAL('dataChanged(const QModelIndex &,const QModelIndex &)'), self.onDataEdited)
        self.connect(self.tablemodel, QtCore.SIGNAL('rowsInserted(const QModelIndex &,int,int)'), self.onRowsInserted)
        self.connect(self.tablemodel, QtCore.SIGNAL('rowsRemoved(const QModelIndex &,int,int)'), self.onRowsRemoved)
        self.tableData.verticalHeader().hide()
        self.tableData.verticalHeader().setDefaultSectionSize(20)
        self.tableData.setSelectionBehavior(QtGui.QAbstractItemView.SelectRows)
        self.tableData.setModel(self.tablemodel)
        self.tabledelegate = self.LinkedFileDelegate()
        self.tableData.setItemDelegate(self.tabledelegate)
        
        loDataEdit.addWidget(self.tableData)
        
        # Left panel: editor buttons
        loEditorButtons = QtGui.QHBoxLayout()

        self.addrowbutton = QtGui.QPushButton('Add row',self)
        loEditorButtons.addWidget(self.addrowbutton)
        self.connect(self.addrowbutton, QtCore.SIGNAL('clicked()'), self.addRow)

        self.removerowbutton = QtGui.QPushButton('Remove row',self)
        loEditorButtons.addWidget(self.removerowbutton)
        self.connect(self.removerowbutton, QtCore.SIGNAL('clicked()'), self.removeRow)

        self.importbutton = QtGui.QPushButton('Import data...',self)
        loEditorButtons.addWidget(self.importbutton)
        self.connect(self.importbutton, QtCore.SIGNAL('clicked()'), self.onImport)

        self.exportbutton = QtGui.QPushButton('Export data...',self)
        loEditorButtons.addWidget(self.exportbutton)
        self.connect(self.exportbutton, QtCore.SIGNAL('clicked()'), self.onExport)

        loEditorButtons.addStretch(1)

        loLeft.addLayout(loDataEdit)
        loLeft.addLayout(loEditorButtons)

        lo.addLayout(loLeft,0,0)

        loRight = QtGui.QVBoxLayout()

        # Right panel: list of variables and plot panel.
        lolist = QtGui.QHBoxLayout()
        self.label = QtGui.QLabel('Available variables:',self)
        lolist.addWidget(self.label)
        self.list = QtGui.QComboBox(self)
        namedict = self.varstore.getVariableLongNames()
        for name in self.varstore.getVariableNames():
            self.list.addItem(namedict[name],QtCore.QVariant(name))
        self.list.setEnabled(self.list.count()>0)
        
        lolist.addWidget(self.list,1)
        loRight.addLayout(lolist)

        self.panel = FigurePanel(self)
        loRight.addWidget(self.panel)

        lo.addLayout(loRight,0,1)

        # Bottom panel: OK and Cancel button
        lobuttons = QtGui.QHBoxLayout()
        self.okbutton = QtGui.QPushButton('OK',self)
        self.cancelbutton = QtGui.QPushButton('Cancel',self)
        lobuttons.addStretch(1)
        lobuttons.addWidget(self.okbutton)
        lobuttons.addWidget(self.cancelbutton)
        
        lo.addLayout(lobuttons,1,0,1,2)

        self.setLayout(lo)

        self.connect(self.list, QtCore.SIGNAL('currentIndexChanged(int)'), self.onSelectionChanged)
        self.connect(self.okbutton, QtCore.SIGNAL('clicked()'), self.accept)
        self.connect(self.cancelbutton, QtCore.SIGNAL('clicked()'), self.reject)

        self.resize(750, 450)

        self.datafile = datafile
        self.owndatafile = False

        self.first = True
        
        self.onSelectionChanged(0)

        self.progressdialog = QtGui.QProgressDialog('',QtCore.QString(),0,0,self,QtCore.Qt.Dialog|QtCore.Qt.WindowTitleHint)
        self.progressdialog.setModal(True)
        self.progressdialog.setMinimumDuration(0)
        self.progressdialog.setAutoReset(False)
        self.progressdialog.setWindowTitle('Parsing data file...')

        self.setWindowTitle(self.node.getText(detail=1,capitalize=True))
        
    def addRow(self):
        """Event handler: called when user clicks the "Add row" button.
        """
        self.tablemodel.addRow()
        self.tableData.selectRow(self.tablemodel.rowCount()-1)

    def removeRow(self):
        """Event handler: called when user clicks the "Remove row" button.
        """
        rows = self.tableData.selectionModel().selectedRows()
        if len(rows)==0: return
        bottom = rows[-1].row()
        top = bottom
        for index in reversed(rows[:-1]):
            currow = index.row()
            if currow!=top-1:
                self.tablemodel.removeRow(top,bottom)
                bottom = currow
            top = currow
        self.tablemodel.removeRow(top,bottom)
        
    def dataChanged(self):
        """Event handler: called when the data in the underlying store change
        (new data loaded from source)        
        """
        self.tablemodel.reset()
        self.tableData.verticalScrollBar().setValue(0)
        self.tableData.horizontalScrollBar().setValue(0)
        
    def onDataEdited(self,topleft,bottomright):
        """Event handler: called when the user has edited the data in our data model.
        """
        self.panel.figure.update()
        
    def onRowsInserted(self,parent,start,stop):
        """Event handler: called when the user has inserted a row in our data model.
        """
        self.panel.figure.update()

    def onRowsRemoved(self,parent,start,stop):
        """Event handler: called when the user has removed one or more rows in our data model.
        """
        self.panel.figure.update()
        
    def onTimeChanged(self,current,previous):
        """Event handler: called when the user selects another date (only used for profiles in time).
        """
        self.tablemodel.pos = current.row()
        self.dataChanged()

    def showEvent(self,ev):
        if self.first and self.datafile!=None:
            self.setData(self.datafile)
            self.first = False

    def setData(self,datafile):

        # Close any detached figures
        self.panel.closeDetached()

        # Try to parse the supplied data file.
        try:
            try:
                self.varstore.loadDataFile(datafile,callback=self.onParseProgress)
            finally:
                self.progressdialog.reset()
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Invalid data file', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            return

        # Reset the models attached to the variable store.
        self.dataChanged()
        if isinstance(self.varstore,data.LinkedProfilesInTime): self.listmodel.reset()
        
        # Update figure
        self.panel.figure.update()
        
        self.exportbutton.setEnabled(self.datafile.isValid())

    def onParseProgress(self,progress,status):
        if self.progressdialog.isHidden(): self.progressdialog.show()
        if progress!=None:
            if self.progressdialog.maximum()==0: self.progressdialog.setMaximum(100)
            self.progressdialog.setValue(int(100*progress))
        elif self.progressdialog.maximum()!=0:
            self.progressdialog.setMaximum(0)
            self.progressdialog.setValue(0)
            
        self.progressdialog.setLabelText(status)
        QtGui.qApp.processEvents()

    def onImport(self):
        path = unicode(QtGui.QFileDialog.getOpenFileName(self,'','',''))

        # If the browse dialog was cancelled, just return.
        if path=='': return

        # Create data file for file-on-disk, copy it to memory, and release the data file.
        df = xmlstore.DataContainerDirectory.DataFileFile(path)
        memdf = xmlstore.DataFileMemory.fromDataFile(df)
        df.release()
        self.owndatafile = True

        # Use the in-memory data file.
        self.setData(memdf)

    def onExport(self):
        path = unicode(QtGui.QFileDialog.getSaveFileName(self,'','',''))
        
        # If the browse dialog was cancelled, just return.
        if path=='': return

        # Save data file.
        self.varstore.saveToFile(path)

    def onSelectionChanged(self,index):
        if index<0:
            self.panel.clear()
        else:
            varname = unicode(self.list.itemData(index,QtCore.Qt.UserRole).toString())
            self.panel.plot(varname,self.varstore)

    def reject(self):
        if self.owndatafile: self.datafile.release()
        QtGui.QDialog.reject(self)
        
    def accept(self):
        self.datafile = self.varstore.getAsDataFile()
        QtGui.QDialog.accept(self)

# =======================================================================
# LinkedFileEditor: a Qt widget for "editing" a linked file. Currently
# just displays a button that, when clicked, displays a separate dialog.
# =======================================================================

class LinkedFileEditor(QtGui.QWidget):
    def __init__(self,parent=None,prefix=''):
        QtGui.QWidget.__init__(self, parent)

        lo = QtGui.QHBoxLayout()

        self.plotbutton = QtGui.QPushButton(prefix+'...',self)
        lo.addWidget(self.plotbutton)
        #lo.addStretch(1)

        self.setLayout(lo)

        self.connect(self.plotbutton, QtCore.SIGNAL('clicked()'), self.onPlot)

    def setNode(self,node):
        self.node = node
        self.datafile = self.node.getValue()

    def onPlot(self):
        dialog = LinkedFilePlotDialog(self.node,self,datafile=self.datafile)
        ret = dialog.exec_()
        if ret == QtGui.QDialog.Accepted:
            self.datafile = dialog.datafile
            self.emit(QtCore.SIGNAL('onChanged()'))

# =======================================================================
# TimeDeltaEditor
# =======================================================================

class TimeDeltaEditor(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self, parent)

        lo = QtGui.QHBoxLayout()
        
        self.spinValue = QtGui.QDoubleSpinBox(self)
        self.spinValue.setMinimum(0.)
        
        self.comboUnits = QtGui.QComboBox(self)
        self.comboUnits.addItems(['seconds','minutes','hours','days'])

        lo.addWidget(self.spinValue)
        lo.addWidget(self.comboUnits)

        self.connect(self.spinValue,  QtCore.SIGNAL('editingFinished()'), self.onChange)
        self.connect(self.comboUnits, QtCore.SIGNAL('currentIndexChanged(int)'),    self.onUnitChange)

        self.setLayout(lo)

    def setValue(self,delta=None):
        if delta==None: delta = xmlstore.StoreTimeDelta()
        seconds = delta.seconds + delta.microseconds/1000000.
        if delta.days>0:
            self.comboUnits.setCurrentIndex(3)
            self.spinValue.setValue(delta.days+seconds/86400.)
        elif delta.seconds>=3600:
            self.comboUnits.setCurrentIndex(2)
            self.spinValue.setValue(seconds/3600)
        elif delta.seconds>=60:
            self.comboUnits.setCurrentIndex(1)
            self.spinValue.setValue(seconds/60)
        else:
            self.comboUnits.setCurrentIndex(0)
            self.spinValue.setValue(seconds)

    def value(self):
        unit = self.comboUnits.currentIndex()
        if   unit==0:
            return xmlstore.StoreTimeDelta(seconds=self.spinValue.value())
        elif unit==1:
            return xmlstore.StoreTimeDelta(seconds=self.spinValue.value()*60)
        elif unit==2:
            return xmlstore.StoreTimeDelta(seconds=self.spinValue.value()*3600)
        elif unit==3:
            return xmlstore.StoreTimeDelta(days=self.spinValue.value())
            
    def onUnitChange(self,unit):
        if   unit==0:
            self.spinValue.setMaximum(60.)
        elif unit==1:
            self.spinValue.setMaximum(60.)
        elif unit==2:
            self.spinValue.setMaximum(24.)
        elif unit==3:
            self.spinValue.setMaximum(3650.)
        self.onChange()
            
    def onChange(self):
        self.emit(QtCore.SIGNAL('editingFinished()'))

# =======================================================================
# ScientificDoubleValidator: a Qt validator for floating point values
#   Less strict than the standard QDoubleValidator, in the sense that is
#   also accepts values in scientific format (e.g. 1.2e6)
#   Also has properties 'minimum' and 'maximum', used for validation and
#   fix-up.
# =======================================================================

class ScientificDoubleValidator(QtGui.QValidator):
    def __init__(self,parent=None):
        QtGui.QValidator.__init__(self,parent)
        self.minimum = None
        self.maximum = None
        self.suffix = ''

    def validate(self,input,pos):
        input = unicode(input)
        if self.suffix != '':
            # Check for suffix (if ok, cut it off for further value checking)
            if not input.endswith(self.suffix): return (QtGui.QValidator.Invalid,pos)
            input = input[0:len(input)-len(self.suffix)]

        # Check for invalid characters
        rx = QtCore.QRegExp('[^\d\-+eE,.]')
        if rx.indexIn(input)!=-1: return (QtGui.QValidator.Invalid,pos)
        
        # Check if we can convert it into a floating point value
        try:
            v = float(input)
        except ValueError:
            return (QtGui.QValidator.Intermediate,pos)

        # Check for minimum and maximum.
        if self.minimum!=None and v<self.minimum: return (QtGui.QValidator.Intermediate,pos)
        if self.maximum!=None and v>self.maximum: return (QtGui.QValidator.Intermediate,pos)
        
        return (QtGui.QValidator.Acceptable,pos)

    def fixup(self,input):
        if not input.endsWith(self.suffix): return
        vallength = input.length()-len(self.suffix)

        try:
            v = float(input.left(vallength))
        except ValueError:
            return

        if self.minimum!=None and v<self.minimum: input.replace(0,vallength,str(self.minimum))
        if self.maximum!=None and v>self.maximum: input.replace(0,vallength,str(self.maximum))

    def setSuffix(self,suffix):
        self.suffix = suffix

class ScientificDoubleEditor(QtGui.QLineEdit):
    def __init__(self,parent=None):
        QtGui.QLineEdit.__init__(self,parent)
        self.curvalidator = ScientificDoubleValidator(self)
        self.setValidator(self.curvalidator)
        self.suffix = ''

    def setSuffix(self,suffix):
        value = self.value()
        self.suffix = suffix
        self.curvalidator.setSuffix(suffix)
        self.setValue(value)

    def value(self):
        text = self.text()
        text = text[0:len(text)-len(self.suffix)]
        if text=='': return 0
        return float(text)

    def setValue(self,value,format=None):
        if format==None:
            strvalue = unicode(value)
        else:
            strvalue = format % value
        self.setText(strvalue+self.suffix)

    def focusInEvent(self,e):
        QtGui.QLineEdit.focusInEvent(self,e)
        self.selectAll()

    def selectAll(self):
        QtGui.QLineEdit.setSelection(self,0,self.text().length()-len(self.suffix))

    def setMinimum(self,minimum):
        self.curvalidator.minimum = minimum

    def setMaximum(self,maximum):
        self.curvalidator.maximum = maximum

    def interpretText(self):
        if not self.hasAcceptableInput():
            text = self.text()
            self.curvalidator.fixup(text)
            self.setText(text)
            
class ColorEditor(QtGui.QComboBox):
    def __init__(self,parent=None):
        QtGui.QComboBox.__init__(self,parent)
        self.connect(self, QtCore.SIGNAL('activated(int)'), self.onActivated)
        
        #self.addColorRGB('red',255,0,0)
        #self.addColorRGB('green',0,255,0)
        #self.addColorRGB('blue',0,0,255)
        
        for cn in QtGui.QColor.colorNames():
            self.addColor(cn,QtGui.QColor(cn))
        
        self.addColorRGB('custom...',255,255,255)
        
    def addColorRGB(self,text,r,g,b):
        self.addColor(text,QtGui.QColor(r,g,b))

    def addColor(self,text,col):
        iconsize = self.iconSize()
        pm = QtGui.QPixmap(iconsize.width(),iconsize.height())
        pm.fill(col)
        ic = QtGui.QIcon(pm)
        self.addItem(ic,text,QtCore.QVariant(col))
        
    def setColor(self,color):
        for i in range(self.count()-1):
            if QtGui.QColor(self.itemData(i))==color:
                self.setCurrentIndex(i)
                break
        else:
            index = self.count()-1
            self.setItemColor(index,color)
            self.setCurrentIndex(index)

    def color(self):
        return QtGui.QColor(self.itemData(self.currentIndex()))
        
    def setItemColor(self,index,color):
        iconsize = self.iconSize()
        pm = QtGui.QPixmap(iconsize.width(),iconsize.height())
        pm.fill(color)
        ic = QtGui.QIcon(pm)
        self.setItemIcon(index,ic)

        self.setItemData(index,QtCore.QVariant(color))
        
    def onActivated(self,index):
        if index==self.count()-1:
            col = QtGui.QColorDialog.getColor(QtGui.QColor(self.itemData(index)),self)
            self.setItemColor(index,col)

class FontNameEditor(QtGui.QComboBox):
    def __init__(self,parent=None):
        QtGui.QComboBox.__init__(self,parent)

        import matplotlib.font_manager
        fontnames = matplotlib.font_manager.fontManager.ttfdict.keys()
        for fontname in sorted(fontnames,key=str.lower):
            self.addItem(fontname)
            
    def setFontName(self,fontname):
        if fontname==None:
            self.setCurrentIndex(0)
            return
        index = self.findText(fontname)
        if index==-1:
            self.setCurrentIndex(0)
        else:
            self.setCurrentIndex(index)
        
    def fontName(self):
        return self.currentText()

# =======================================================================
# PropertyDelegate: a Qt delegate used to create editors for property
# values.
#   Built to handle properties from our custom TypedXMLPropertyStore,
#   which stores typed properties in hierarchical structure (XML)
#   The internalPointer attribute of provided model indices must refer
#   to a node in the TypedXMLPropertyStore.
# =======================================================================

class PropertyDelegate(QtGui.QItemDelegate):

    def __init__(self,parent=None):
        QtGui.QItemDelegate.__init__(self,parent)

    # createEditor (inherited from QtGui.QItemDelegate)
    #   Creates the editor widget for the model item at the given index
    def createEditor(self, parent, option, index):
        node = index.internalPointer()

        editor = self.createNodeEditor(node,parent)

        lo = editor.layout()
        if lo!=None:
            lo.setMargin(0)
            lo.setSpacing(0)

        # Install event filter that captures key events for view from the editor (e.g. return press).
        editor.installEventFilter(self)
        
        return editor
        
    def paint(self,painter,option,index):
        node = index.internalPointer()
        QtGui.QItemDelegate.paint(self,painter,option,index)
        if index.column()==1 and node.getValueType()=='color':
            dat = index.data()
            r = option.rect.adjusted(1,1,-2,-2)
            r.setWidth(r.height())
            painter.fillRect(r,QtGui.QBrush(dat))
            painter.drawRect(r)

    def createNodeEditor(self,node,parent):
        templatenode = node.templatenode
        nodetype = node.getValueType()
        editor = None
        if nodetype=='string':
            editor = QtGui.QLineEdit(parent)
        elif nodetype=='int':
            editor = QtGui.QSpinBox(parent)
            if templatenode.hasAttribute('minInclusive'):
                editor.setMinimum(int(templatenode.getAttribute('minInclusive')))
            else:
                editor.setMinimum(-sys.maxint-1)
            if templatenode.hasAttribute('maxInclusive'):
                editor.setMaximum(int(templatenode.getAttribute('maxInclusive')))
            else:
                editor.setMaximum(sys.maxint)
            unit = node.getUnit()
            if unit!=None: editor.setSuffix(' '+unit)
        elif nodetype=='float':
            editor = ScientificDoubleEditor(parent)
            if templatenode.hasAttribute('minInclusive'):
                editor.setMinimum(float(templatenode.getAttribute('minInclusive')))
            if templatenode.hasAttribute('maxInclusive'):
                editor.setMaximum(float(templatenode.getAttribute('maxInclusive')))
            unit = node.getUnit()
            if unit!=None: editor.setSuffix(' '+unit)
            self.currenteditor = editor
        elif nodetype=='bool':
            editor = QtGui.QComboBox(parent)
            editor.addItem('Yes',QtCore.QVariant(True))
            editor.addItem('No',QtCore.QVariant(False))
        elif nodetype=='select':
            editor = QtGui.QComboBox(parent)
            options = common.findDescendantNode(templatenode,['options'])
            assert options!=None, 'Node %s is of type "select" but lacks "options" childnode.' % node
            for ch in options.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option' and not ch.hasAttribute('disabled'):
                    editor.addItem(ch.getAttribute('label'),QtCore.QVariant(int(ch.getAttribute('value'))))
        elif nodetype=='datetime':
            editor = QtGui.QDateTimeEdit(parent)
        elif nodetype=='duration':
            editor = TimeDeltaEditor(parent)
        elif nodetype=='file':
            editor = LinkedFileEditor(parent)
            self.currenteditor = editor
        elif nodetype=='color':
            editor = ColorEditor(parent)
        elif nodetype=='fontname':
            editor = FontNameEditor(parent)
        else:
            assert False, 'Unknown node type "%s".' % nodetype

        return editor

    # setEditorData (inherited from QtGui.QItemDelegate)
    #   Sets value in the editor widget, for the model item at the given index
    def setEditorData(self, editor,index):
        value = index.data(QtCore.Qt.EditRole)
        if not value.isValid(): return
        node = index.internalPointer()

        nodetype = node.getValueType()
        if nodetype=='string':
            editor.setText(value.toString())
        elif nodetype=='int':
            value,ret = value.toInt()
            editor.setValue(value)
        elif nodetype=='float':
            value,ret = value.toDouble()
            editor.setValue(value)
        elif nodetype=='bool':
            value = value.toBool()
            for ioption in range(editor.count()):
                optionvalue = editor.itemData(ioption).toBool()
                if optionvalue==value:
                    editor.setCurrentIndex(ioption)
                    break
        elif nodetype=='select':
            value,ret = value.toInt()
            for ioption in range(editor.count()):
                optionvalue,ret = editor.itemData(ioption).toInt()
                if optionvalue==value:
                    editor.setCurrentIndex(ioption)
                    break
        elif nodetype=='datetime':
            value = value.toDateTime()
            editor.setDateTime(value)
        elif nodetype=='duration':
            value = value.toList()
            days,ret  = value[0].toInt()
            secs,ret  = value[1].toInt()
            musecs,ret = value[2].toDouble()
            value = xmlstore.StoreTimeDelta(days=days,seconds=secs,microseconds=musecs)
            editor.setValue(value)
        elif nodetype=='file':
            editor.setNode(node)
        elif nodetype=='color':
            editor.setColor(QtGui.QColor(value))
        elif nodetype=='fontname':
            editor.setFontName(value.toString())

    # setModelData (inherited from QtGui.QItemDelegate)
    #   Obtains the value from the editor widget, and set it for the model item at the given index
    def setModelData(self, editor, model, index):
        node = index.internalPointer()
        nodetype = node.getValueType()
        if nodetype=='string':
            model.setData(index, QtCore.QVariant(editor.text()))
        elif nodetype=='int':
            editor.interpretText()
            model.setData(index, QtCore.QVariant(editor.value()))
        elif nodetype=='float':
            editor.interpretText()
            model.setData(index, QtCore.QVariant(editor.value()))
        elif nodetype=='bool' or nodetype=='select':
            model.setData(index,editor.itemData(editor.currentIndex()))
        elif nodetype=='datetime':
            value = editor.dateTime()
            value.setTimeSpec(QtCore.Qt.UTC)
            model.setData(index, QtCore.QVariant(value))
        elif nodetype=='duration':
            node.setValue(editor.value())
        elif nodetype=='file':
            node.setValue(editor.datafile)
        elif nodetype=='color':
            model.setData(index,QtCore.QVariant(editor.color()))
        elif nodetype=='fontname':
            node.setValue(editor.fontName())

# =======================================================================
# PropertyData: a Qt item model that encapsulates our custom
# TypedXMLPropertyStore, used for hierarchical storage of typed properties
# =======================================================================

class PropertyStoreModel(QtCore.QAbstractItemModel):
    
    def __init__(self,typedstore,nohide = False, novalues = False, checkboxes = False):
        QtCore.QAbstractItemModel.__init__(self)

        self.typedstore = typedstore
        self.nohide = nohide
        self.novalues = novalues
        self.checkboxes = checkboxes

        self.storeinterface = self.typedstore.getInterface(showhidden=self.nohide,omitgroupers=True)
        self.storeinterface.processDefaultChange = 1    # Warn always if a default changes (even if it is not used)

        self.storeinterface.connect('beforeVisibilityChange',self.beforeNodeVisibilityChange)
        self.storeinterface.connect('afterVisibilityChange', self.afterNodeVisibilityChange)
        self.storeinterface.connect('afterChange',self.onNodeChanged)
        self.storeinterface.connect('afterStoreChange',self.reset)

        self.inheritingchecks = False
        
    def unlink(self):
        self.typedstore.disconnectInterface(self.storeinterface)
        self.storeinterface.unlink()
        self.storeinterface = None
        
    # index (inherited from QtCore.QAbstractItemModel)
    #   Supplies unique index for the node at the given (row,column) position
    #   below the given parent (specified as index).
    def index(self,irow,icolumn,parent):
        if not parent.isValid():
            parentnode = self.typedstore.root
        else:
            parentnode = parent.internalPointer()
        child = self.storeinterface.getChildByIndex(parentnode,irow)
        if child==None: return QtCore.QModelIndex()
        assert isinstance(child,xmlstore.Node), 'Object returned by getChildByIndex is not of type "Node" (but "%s").' % child
        return self.createIndex(irow,icolumn,child)

    # parent (inherited from QtCore.QAbstractItemModel)
    #   Supplies unique index for the parent of the given node (specified as index).
    def parent(self,index):
        # We must have a valid index
        assert index.isValid(), 'Asked for parent of root node (invalid index), but Qt asker knows it is the root.'

        current = index.internalPointer()
        assert isinstance(current,xmlstore.Node), 'Node data is not a Node, but: %s.' % current
        parent = self.storeinterface.getParent(current)
        assert isinstance(parent,xmlstore.Node), 'Object returned by getParent is not of type "Node" (but "%s").' % (parent,)

        assert parent!=None, 'We were asked for the parent of the actual root, but we should never have been able to get so far up the tree.'
        
        if parent.parent==None: return QtCore.QModelIndex()
        iparentrow = self.storeinterface.getOwnIndex(parent)
        return self.createIndex(iparentrow,0,parent)

    # rowCount (inherited from QtCore.QAbstractItemModel)
    #   Returns the number of child rows below the given parent (specified as index).
    def rowCount(self,parent=QtCore.QModelIndex()):
        if not parent.isValid():
            parentnode = self.typedstore.root
        else:
            parentnode = parent.internalPointer()
        return self.storeinterface.getChildCount(parentnode)

    # columnCount (inherited from QtCore.QAbstractItemModel)
    #   Returns the number of child columns below the given parent (specified as index).
    def columnCount(self,parent):
        if self.novalues:
            return 1
        else:
            return 2

    # data (inherited from QtCore.QAbstractItemModel)
    #   Returns data for the given node (specified as index), and the given role.
    def data(self,index,role=QtCore.Qt.DisplayRole):

        # In some cases (e.g. when using What's-this) this function is called for the root node
        # (i.e. with an invalid index). In that case we just return the default data value.
        if not index.isValid(): return QtCore.QVariant()
        
        # Shortcut to the xmlstore.TypedStore node (used below in many places)
        node = index.internalPointer()

        # First handle roles that are shared over the whole row.
        if role==QtCore.Qt.WhatsThisRole:
            templatenode = node.templatenode
            text = node.getText(detail=2)
            nodetype = node.getValueType()
            if nodetype=='select':
                optionsroot = common.findDescendantNode(templatenode,['options'])
                assert optionsroot!=None, 'Variable with "select" type lacks "options" element below.'
                optionnodes = common.findDescendantNodes(optionsroot,['option'])
                assert len(optionnodes)>0, 'Variable with "select" type does not have any options assigned to it.'
                text += '\n\nAvailable options:'
                for optionnode in optionnodes:
                    text += '\n- '
                    if optionnode.hasAttribute('description'):
                        text += optionnode.getAttribute('description')
                    else:
                        text += optionnode.getAttribute('label')
            elif nodetype=='int' or nodetype=='float':
                if templatenode.hasAttribute('minInclusive'): text += '\nminimum value: '+templatenode.getAttribute('minInclusive')
                if templatenode.hasAttribute('maxInclusive'): text += '\nmaximum value: '+templatenode.getAttribute('maxInclusive')
            return QtCore.QVariant(text)
        elif role==QtCore.Qt.TextColorRole:
            if self.nohide and not node.visible:
                # If we should show 'hidden' nodes too, color them blue to differentiate.
                return QtCore.QVariant(QtGui.QColor(0,0,255))
            elif index.column()==1 and node.isReadOnly():
                # Color read-only nodes grey to differentiate.
                return QtCore.QVariant(QtGui.QColor(128,128,128))
        elif self.checkboxes and role==QtCore.Qt.CheckStateRole:
            if node.canHaveValue():
                # Node has own checkbox.
                if node.getValue():
                    return QtCore.QVariant(QtCore.Qt.Checked)
                else:
                    return QtCore.QVariant(QtCore.Qt.Unchecked)
            elif node.hasChildren():
                # Node is parent of other nodes with their own checkbox; check value is derived from children.
                state = None
                for i in range(self.rowCount(index)):
                    chstate,ret = index.child(i,0).data(QtCore.Qt.CheckStateRole).toInt()
                    if chstate==QtCore.Qt.PartiallyChecked:
                        return QtCore.QVariant(QtCore.Qt.PartiallyChecked)
                    elif state==None:
                        state = chstate
                    elif chstate!=state:
                        return QtCore.QVariant(QtCore.Qt.PartiallyChecked)
                return QtCore.QVariant(state)

        # Now handle column-specific roles.
        if index.column()==0:
            if role==QtCore.Qt.DisplayRole:
                return QtCore.QVariant(node.getText(detail=1))
            else:
                return QtCore.QVariant()
        else:
            # We only process the 'display', 'edit' and 'font' roles.
            if not (role==QtCore.Qt.DisplayRole or role==QtCore.Qt.EditRole or role==QtCore.Qt.FontRole): return QtCore.QVariant()

            # Column 1 is only used for variables that can have a value.
            if not node.canHaveValue(): return QtCore.QVariant()

            fieldtype = node.getValueType()
            if role==QtCore.Qt.FontRole:
                # Return bold font if the node value is set to something different than the default.
                if self.typedstore.defaultstore==None: QtCore.QVariant()
                font = QtGui.QFont()
                val = node.getValue()
                font.setBold(val!=None and val!=node.getDefaultValue())
                return QtCore.QVariant(font)
            elif role==QtCore.Qt.DisplayRole:
                if fieldtype=='color':
                    value = node.getValue(usedefault=True)
                    pm = QtGui.QPixmap(10,10)
                    pm.fill(QtGui.QColor(value.red,value.green,value.blue))
                    return QtCore.QVariant(pm)
                return QtCore.QVariant(node.getValueAsString(usedefault=True))
            elif role==QtCore.Qt.EditRole:
                value = node.getValue(usedefault=True)
                if value==None: return QtCore.QVariant()
                if fieldtype=='datetime':
                    # First convert Python datetime to QDateTime, then cast to variant.
                    return QtCore.QVariant(datetime2qtdatetime(value))
                elif fieldtype=='duration':
                    return QtCore.QVariant([QtCore.QVariant(int(value.days)),QtCore.QVariant(int(value.seconds)),QtCore.QVariant(float(value.microseconds))])
                elif fieldtype=='file':
                    # Return full path
                    return QtCore.QVariant(unicode(value))
                elif fieldtype=='color':
                    return QtCore.QVariant(QtGui.QColor(value.red,value.green,value.blue))
                else:
                    # Simply cast the current value to variant.
                    return QtCore.QVariant(value)
            else:
                assert False, 'Don\'t know how to handle role %s.' % role

    # setData (inherited from QtCore.QAbstractItemModel)
    #   Set data for the given node (specified as index), and the given role.
    def setData(self,index,value,role=QtCore.Qt.EditRole):
        # Get node (XML element) from given node index (QtCore.QModelIndex)
        node = index.internalPointer()

        if self.checkboxes and role==QtCore.Qt.CheckStateRole:
            if node.canHaveValue():
                node.setValue(value.toBool())
                self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),index,index)
            elif node.hasChildren():
                checkroot = (not self.inheritingchecks)
                self.inheritingchecks = True
                for i in range(self.rowCount(index)):
                    self.setData(index.child(i,0),value,role=QtCore.Qt.CheckStateRole)
                self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),index,index)
                if checkroot: self.inheritingchecks = False
            if not self.inheritingchecks:
                par = index.parent()
                while par.isValid():
                    self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),par,par)
                    par = par.parent()
            return True
        
        assert index.column()==1, 'Column %i is being set, but should not be editable (only column 1 should)' % index.column()

        if not value.isValid():
            node.clearValue()
            self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),index,index)
            return True
        
        templatenode = node.templatenode
        fieldtype = node.getValueType()

        # Convert given variant to the type we need.
        if fieldtype=='string':
            value = value.toString()
        elif fieldtype=='file':
            value = xmlstore.DataContainerDirectory.DataFileFile(unicode(value.toString()))
        elif fieldtype=='int':
            value,converted = value.toInt()
            if not converted: return False
            if templatenode.hasAttribute('minInclusive'):
                if value<int(templatenode.getAttribute('minInclusive')): return False
            if templatenode.hasAttribute('maxInclusive'):
                if value>int(templatenode.getAttribute('maxInclusive')): return False
        elif fieldtype=='float':
            value,converted = value.toDouble()
            if not converted: return False
            if templatenode.hasAttribute('minInclusive'):
                if value<float(templatenode.getAttribute('minInclusive')): return False
            if templatenode.hasAttribute('maxInclusive'):
                if value>float(templatenode.getAttribute('maxInclusive')): return False
        elif fieldtype=='bool':
            value = value.toBool()
        elif fieldtype=='datetime':
            value = qtdatetime2datetime(value.toDateTime())
        elif fieldtype=='duration':
            value = value.toList()
            days,converted = value[0].toInt()
            secs,converted  = value[1].toInt()
            musecs,converted  = value[2].toDouble()
            value = xmlstore.StoreTimeDelta(days=days,seconds=secs,microseconds=musecs)
        elif fieldtype=='select':
            value,converted = value.toInt()
            if not converted: return False
        elif fieldtype=='color':
            col = QtGui.QColor(value)
            value = xmlstore.StoreColor(col.red(),col.green(),col.blue())
        else:
            assert False, 'unknown variable type "%s" in XML scenario template' % fieldtype

        node.setValue(value)

        self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),index,index)
        return True

    # flags (inherited from QtCore.QAbstractItemModel)
    #   Returns flags applicable to the given node.
    def flags(self,index):
        # If we do not have a valid index, return the default.
        if not index.isValid(): return QtCore.QAbstractItemModel.flags(self,index)

        # Default flags: selectable and enabled
        f = QtCore.Qt.ItemIsSelectable | QtCore.Qt.ItemIsEnabled

        node = index.internalPointer()

        # Add checkbox if needed
        if self.checkboxes:
            f |= QtCore.Qt.ItemIsUserCheckable
            if node.hasChildren(): f |= QtCore.Qt.ItemIsTristate

        # Make editable if it's the 1st column and the node is editable.
        if index.column()==1 and node.canHaveValue() and (not node.isReadOnly()): f |= QtCore.Qt.ItemIsEditable
            
        return f

    # headerData (inherited from QtCore.QAbstractItemModel)
    #   Returns the header for the given row or column (in our case columns only).
    def headerData(self,section,orientation,role):
        if role==QtCore.Qt.DisplayRole:
            if section==0:
                return QtCore.QVariant('variable')
            elif section==1:
                return QtCore.QVariant('value')
        return QtCore.QVariant()

    def beforeNodeVisibilityChange(self,node,newvisibility,showhide):
        assert isinstance(node,xmlstore.Node), 'Supplied object is not of type "Node" (but "%s").' % node
        if self.nohide and showhide: return
        irow = self.storeinterface.getOwnIndex(node)
        index = self.createIndex(irow,1,node)
        par = self.parent(index)
        if newvisibility:
            self.beginInsertRows(par,irow,irow)
        else:
            self.beginRemoveRows(par,irow,irow)

    def afterNodeVisibilityChange(self,node,newvisibility,showhide):
        assert isinstance(node,xmlstore.Node), 'Supplied object is not of type "Node" (but "%s").' % node
        if self.nohide and showhide: return self.onNodeChanged(node,'visibility',headertoo=True)
        if newvisibility:
            self.endInsertRows()
        else:
            self.endRemoveRows()

    def onNodeChanged(self,node,feature,headertoo = False):
        assert isinstance(node,xmlstore.Node), 'Supplied object is not of type "Node" (but "%s").' % node
        irow = self.storeinterface.getOwnIndex(node)
        index = self.createIndex(irow,1,node)
        self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),index,index)

        if headertoo:
            index = self.createIndex(irow,0,node)
            self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex&,const QModelIndex&)'),index,index)

        # For debugging purposes only: write current scenario values to XML
        # self.typedstore.save('./scenario.xml')

    def resetData(self,index,recursive=False):
        node = index.internalPointer()
        node.clearValue(recursive=recursive,skipreadonly=True,deleteclones=False)

    def hasDefaultValue(self,index):
        node = index.internalPointer()
        if node==None or not node.canHaveValue(): return True
        value = node.getValue()
        return value==None or value==node.getDefaultValue()

    def getCheckedNodes(self,index=None):
        if index==None: index = QtCore.QModelIndex()
        res = []
        for irow in range(self.rowCount(index)):
            child = self.index(irow,0,index)
            state,ret = child.data(QtCore.Qt.CheckStateRole).toInt()
            if state==QtCore.Qt.Checked:
                res.append(child.internalPointer())
            res += self.getCheckedNodes(child)
        return res

# =======================================================================
# ExtendedTreeView: based on Qt QTreeView, additionally supports batch
# collapse/expand.
# =======================================================================

class ExtendedTreeView(QtGui.QTreeView):

    def __init__(self,parent=None):
        QtGui.QTreeView.__init__(self,parent)

    def setExpandedAll(self,value=True,maxdepth=1000,root=None,depth=0):
        model = self.model()
        if root==None: root=QtCore.QModelIndex()
        rc = model.rowCount(root)
        if rc>0:
            self.setExpanded(root,value)
            if depth<maxdepth:
                for ich in range(rc):
                    ch = model.index(ich,0,root)
                    self.setExpandedAll(value=value,root=ch,depth=depth+1,maxdepth=maxdepth)

    def expandNonDefaults(self,root=None):
        model = self.model()
        if root==None: root=QtCore.QModelIndex()
        exp = False
        rc = model.rowCount(root)
        if rc>0:
            for ich in range(rc):
                ch = model.index(ich,0,root)
                if self.expandNonDefaults(root=ch):
                    exp = True
            if exp: self.expand(root)
        if not model.hasDefaultValue(root): exp = True
        return exp

    def contextMenuEvent(self,e):
        index = self.indexAt(e.pos())
        if not index.isValid(): return

        # We will handle the context menu event.
        e.accept()
        
        # Get indices of name and value cell.
        if index.column()==0:
            nameindex = index
            valueindex = index.sibling(index.row(),1)
        else:
            nameindex = index.sibling(index.row(),0)
            valueindex = index

        # Check whether we can reset the current value and/or its decendants. Stop if not.
        model = self.model()
        resetself = (model.flags(valueindex) & QtCore.Qt.ItemIsEditable) and not model.hasDefaultValue(valueindex)
        resetchildren = model.hasChildren(nameindex)
        if not (resetself or resetchildren): return
        
        # Build context menu
        menu = QtGui.QMenu(self)
        if resetself:     actReset = menu.addAction('Reset value')
        if resetchildren: actResetChildren = menu.addAction('Reset entire branch')
        actChosen = menu.exec_(e.globalPos())
        if resetself and actChosen is actReset:
            model.resetData(nameindex)
        elif resetchildren and actChosen is actResetChildren:
            model.resetData(nameindex,recursive=True)

    def rowsInserted(self,parent,start,end):
        QtGui.QTreeView.rowsInserted(self,parent,start,end)
        model = self.model()
        if model.rowCount(parent)==end-start+1:
            self.expand(parent)

    def selectionChanged(self,selected,deselected):
        QtGui.QTreeView.selectionChanged(self,selected,deselected)
        self.emit(QtCore.SIGNAL('onSelectionChanged()'))
    
class PropertyEditorDialog(QtGui.QDialog):
    
    def __init__(self,parent,store,title='',instructions='',flags=QtCore.Qt.Dialog):
        QtGui.QDialog.__init__(self, parent, flags)

        self.model = PropertyStoreModel(store,nohide=False)

        self.tree = ExtendedTreeView(self)
        self.delegate = PropertyDelegate()
        self.tree.setItemDelegate(self.delegate)
        self.tree.setModel(self.model)
        self.tree.setExpandedAll(maxdepth=3)

        self.grip = QtGui.QSizeGrip(self)

        layout = QtGui.QVBoxLayout()
        layout.setMargin(0)

        if instructions!='':
            lab = QtGui.QLabel(instructions,self)
            lab.setWordWrap(True)
            layout.addWidget(lab)

        layout.addWidget(self.tree)
        layout.addWidget(self.grip)
        self.setLayout(layout)

        if title!='':
            self.setWindowTitle(title)

    def resizeColumns(self):
        self.tree.header().resizeSections(QtGui.QHeaderView.Stretch)
        self.tree.resizeColumnToContents(0)

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

        self.settings = settings.SettingsStore()

        self.sequence = sequence
        self.currentpage = None

    def getProperty(self,propertyname):
        if propertyname not in self.shared: return None
        return self.shared[propertyname]

    def setProperty(self,propertyname,value):
        if propertyname in self.shared and isinstance(self.shared[propertyname],common.referencedobject):
            self.shared[propertyname].release()
        self.shared[propertyname] = value
        self.onPropertyChange(propertyname)
        
    def onPropertyChange(self,propertyname):
        pass
        
    def clearProperties(self):
        for propertyname in self.shared.keys():
            self.setProperty(propertyname,None)

    def unlink(self):
        self.settings.save()
        self.settings.release()
        self.settings = None
        for v in self.shared.values():
            try:
                v.release()
            except:
                pass

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

class PropertyEditorFactory:

    def __init__(self,typedstore,live=False,allowhide=False,unitinside=False):
        self.store = typedstore
        self.changed = False
        self.live = live
        self.allowhide = allowhide
        self.unitinside = unitinside
        self.editors = []

        if self.live:
            self.storeinterface = self.store.getInterface()
            self.storeinterface.connect('afterChange',self.onStoreNodeChanged)
            self.storeinterface.connect('afterVisibilityChange',self.onStoreVisibilityChanged)
            
    def unlink(self):
        if self.live:
            self.store.disconnectInterface(self.storeinterface)
            self.storeinterface.unlink()
            self.storeinterface = None

    def createEditor(self,location,parent,allowhide=None,**kwargs):
        assert location!=None, 'Specified node is None (non-existent?).'
        if isinstance(location,xmlstore.Node):
            node = location
        else:
            node = self.store[location]
            assert node!=None, 'Unable to create editor for "%s"; this node does not exist.' % location

        # The editor inherits some optional arguments from the responsible factory.
        if 'allowhide' not in kwargs: kwargs['allowhide'] = self.allowhide
        if 'unitinside' not in kwargs: kwargs['unitinside'] = self.unitinside

        # Create the editor object.        
        editor = PropertyEditor(node,parent,**kwargs)
        
        # If we are "live", update the enabled/disabled state or visibility of the editor.
        if self.live: editor.updateEditorEnabled()
        
        # Make sure we receive notifications when the value in the editor changes.
        editor.addChangeHandler(self.onNodeEdited)
        
        # Add the editor to our list of editors.
        self.editors.append(editor)
        
        return editor
        
    def destroyEditor(self,editor,layout=None):
        for i in range(len(self.editors)-1,-1,-1):
            if self.editors[i] is editor:
                del self.editors[i]
                editor.destroy(layout)
                break

    def updateStore(self):
        for editor in self.editors:
            editor.updateStore()

    def hasChanged(self):
        return self.changed

    def onStoreNodeChanged(self,node,feature):
        for editor in self.editors:
            if editor.node is node:
                editor.updateEditorValue()
                break

    def onStoreVisibilityChanged(self,node,visible,showhide):
        if not showhide: return
        for editor in self.editors:
            #if node is editor.node:
            if ('/'.join(editor.node.location)).startswith('/'.join(node.location)):
                editor.updateEditorEnabled()

    def onNodeEdited(self,editor):
        self.changed = True
        if self.live:
            if not editor.updateStore(): editor.updateEditorValue()
            
    def getEditedNodes(self):
        return [editor.node for editor in self.editors]

class PropertyEditor:

    def __init__(self,node,parent,allowhide=False,unitinside=False,**kwargs):
        self.node = node
        self.unit = None
        self.label = None
        self.icon = None
        self.allowhide = allowhide
        self.unitinside = unitinside

        self.editor = self.createEditor(node,parent,**kwargs)
        self.updateEditorValue()
        
        self.changehandlers = []
        self.suppresschangeevent = False
        self.location = node.location[:]
        
    def addToGridLayout(self,gridlayout,irow=None,icolumn=0,rowspan=1,colspan=1,label=True,unit=True,icon=None):
        """Adds the editor plus label to an existing QGridLayout, in the specified row, starting at the specified column.
        """
        if irow==None: irow = gridlayout.rowCount()
        if icon==None: icon = (self.node.getText(detail=2,minimumdetail=2)!=None)

        if label:
            if self.label==None: self.createLabel()
            gridlayout.addWidget(self.label,irow,icolumn)
            icolumn += 1

        gridlayout.addWidget(self.editor,irow,icolumn,rowspan,colspan)
        icolumn += colspan

        if unit and not self.unitinside:
            if self.unit==None: self.createUnit()
            gridlayout.addWidget(self.unit,irow,icolumn)
            icolumn += 1

    def addToBoxLayout(self,boxlayout,label=True,unit=True,addstretch=True,icon=None):
        """Adds the editor plus label to an existing QBoxLayout.
        """
        if icon==None: icon = (self.node.getText(detail=2,minimumdetail=2)!=None)

        if not isinstance(boxlayout,QtGui.QHBoxLayout):
            layout = QtGui.QHBoxLayout()
        else:
            layout = boxlayout
        
        if label:
            if self.label==None: self.createLabel()
            layout.addWidget(self.label)
            
        layout.addWidget(self.editor)
        
        if unit and not self.unitinside:
            if self.unit==None: self.createUnit()
            layout.addWidget(self.unit)
            
        if addstretch:
            layout.addStretch(1)
            
        if layout is not boxlayout:
            boxlayout.addLayout(layout)

    def createUnit(self):
        """Creates a label with the unit of the editor, based on the description in the source node.
        This function can be called only once in the life time of the object.
        """
        assert self.unit==None, 'Cannot create unit because it has already been created.'
        unittext = self.node.getUnit()
        if unittext==None: unittext=''
        self.unit = QtGui.QLabel(unittext,self.editor.parent())
        if self.allowhide and self.node.isHidden(): self.unit.setVisible(False)
        return self.unit
        
    def createLabel(self,detail=1,wrap=False,addcolon=True,text=None):
        """Creates a label for the editor, based on the description in the source node.
        This function can be called only once in the life time of the object.
        """
        assert self.label==None, 'Cannot create label because it has already been created.'
        if text==None:
            text = self.node.getText(detail=detail,capitalize=True)
            if addcolon: text += ': '
        self.label = QtGui.QLabel(text,self.editor.parent())
        if wrap: self.label.setWordWrap(True)
        if self.allowhide and self.node.isHidden(): self.label.setVisible(False)
        return self.label
        
    def setVisible(self,visible):
        """Sets the visibility of the editor and label (if any).
        """
        if self.label!=None: self.label.setVisible(visible)
        if self.icon !=None: self.icon.setVisible(visible)
        if self.unit !=None: self.unit.setVisible(visible)
        self.editor.setVisible(visible)

    def destroy(self,layout=None):
        """Removes all widgets belonging to this editor from the layout.
        """
        if layout!=None:
            if self.label!=None: layout.removeWidget(self.label)
            if self.icon !=None: layout.removeWidget(self.icon)
            if self.unit !=None: layout.removeWidget(self.unit)
            layout.removeWidget(self.editor)

        if self.label!=None:
            self.label.destroy()
            self.label = None
        if self.icon!=None:
            self.icon.destroy()
            self.icon = None
        if self.unit!=None:
            self.unit.destroy()
            self.unit = None
        self.editor.destroy()
        self.editor = None

    def updateStore(self):
        """Updates the value of the source node with the current value of the editor.
        """
        return self.setNodeData(self.editor,self.node)

    def updateEditorValue(self):
        """Updates the value in the editor, so it reflects the current value of the source node.
        """
        self.setEditorData(self.editor,self.node)
        if self.unit!=None:
            unittext = self.node.getUnit()
            if unittext==None: unittext=''
            self.unit.setText(unittext)

    def updateEditorEnabled(self):
        """Enables/disables or shows/hides the editor (and label, if any) based on the visibility of the source node.
        Called by the responsible factory when the "hidden" state of the source node changes.
        """
        visible = not self.node.isHidden()
        if isinstance(self.editor,QtGui.QWidget):
            if self.allowhide:
                self.setVisible(visible)
            else:
                self.editor.setEnabled(visible)
        elif isinstance(self.editor,QtGui.QButtonGroup):
            for bn in self.editor.buttons():
                if self.allowhide:
                    bn.setVisible(visible)
                else:
                    bn.setEnabled(visible)

    def addChangeHandler(self,callback):
        """Registers an event handler to be called when the value in the editor changes.
        Used by the responsible factory to immediately update the source node, if editing is "live".
        """
        self.changehandlers.append(callback)

    def onChange(self):
        """Called internally when the value in the editor changes. Dispatches the change
        event to the attached event handlers (if any).
        """
        if not self.suppresschangeevent:
            for callback in self.changehandlers:
                callback(self)

    def createEditor(self,node,parent,selectwithradio=False,boolwithcheckbox=False,fileprefix=None,groupbox=False,whatsthis=True):
        templatenode = node.templatenode
        nodetype = node.getValueType()
        editor = None
        
        if groupbox:
            editor = QtGui.QGroupBox(node.getText(detail=1,capitalize=True),parent)
            #editor.setFlat(True)
            whatsthis = False
        elif nodetype=='string':
            editor = QtGui.QLineEdit(parent)
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='int':
            editor = QtGui.QSpinBox(parent)
            if templatenode.hasAttribute('minInclusive'):
                editor.setMinimum(int(templatenode.getAttribute('minInclusive')))
            else:
                editor.setMinimum(-sys.maxint-1)
            if templatenode.hasAttribute('maxInclusive'):
                editor.setMaximum(int(templatenode.getAttribute('maxInclusive')))
            else:
                editor.setMaximum(sys.maxint)
            if self.unitinside:
                unit = node.getUnit()
                if unit!=None: editor.setSuffix(' '+unit)
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='float':
            editor = ScientificDoubleEditor(parent)
            if templatenode.hasAttribute('minInclusive'):
                editor.setMinimum(float(templatenode.getAttribute('minInclusive')))
            if templatenode.hasAttribute('maxInclusive'):
                editor.setMaximum(float(templatenode.getAttribute('maxInclusive')))
            if self.unitinside:
                unit = node.getUnit()
                if unit!=None: editor.setSuffix(' '+unit)
            self.currenteditor = editor
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='bool':
            if boolwithcheckbox:
                editor = QtGui.QCheckBox(node.getText(detail=1,capitalize=True),parent)
                editor.connect(editor, QtCore.SIGNAL('stateChanged(int)'), self.onChange)
            else:
                editor = QtGui.QComboBox(parent)
                editor.addItem('True',QtCore.QVariant(True))
                editor.addItem('False',QtCore.QVariant(False))
                editor.connect(editor, QtCore.SIGNAL('currentIndexChanged(int)'), self.onChange)
        elif nodetype=='select':
            options = common.findDescendantNode(templatenode,['options'])
            if options==None: raise 'Node is of type "select" but lacks "options" childnode.'
            if selectwithradio:
                editor = QtGui.QButtonGroup()
                for ch in options.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option' and not ch.hasAttribute('disabled'):
                        opt = QtGui.QRadioButton(ch.getAttribute('label'),parent)
                        if ch.hasAttribute('description'):
                            opt.setWhatsThis(ch.getAttribute('description'))
                        editor.addButton(opt,int(ch.getAttribute('value')))
                editor.connect(editor, QtCore.SIGNAL('buttonClicked(int)'), self.onChange)
            else:
                editor = QtGui.QComboBox(parent)
                for ch in options.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option' and not ch.hasAttribute('disabled'):
                        editor.addItem(ch.getAttribute('label'),QtCore.QVariant(int(ch.getAttribute('value'))))
                editor.connect(editor, QtCore.SIGNAL('currentIndexChanged(int)'), self.onChange)
        elif nodetype=='datetime':
            editor = QtGui.QDateTimeEdit(parent)
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='duration':
            editor = TimeDeltaEditor(parent)
            self.currenteditor = editor
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='file':
            if fileprefix==None: fileprefix = node.getText(detail=1,capitalize=True)
            editor = LinkedFileEditor(parent,prefix=fileprefix)
            self.currenteditor = editor
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        else:
            assert False, 'Unknown node type "%s".' % nodetype
            
        # Add what's-this information.
        if whatsthis and isinstance(editor,QtGui.QWidget):
            editor.setWhatsThis(node.getText(detail=2,capitalize=True))
            
        return editor

    def setEditorData(self,editor,node):
        if isinstance(editor,QtGui.QGroupBox): return
        self.suppresschangeevent = True
        value = node.getValue(usedefault=True)
        nodetype = node.getValueType()
        if value==None:
            if nodetype=='string':
                editor.setText('')
            elif nodetype=='int':
                editor.setValue(0)
            elif nodetype=='float':
                editor.setText(editor.suffix)
            elif nodetype=='bool':
                if isinstance(editor,QtGui.QCheckBox):
                    editor.setChecked(False)
                else:
                    editor.setCurrentIndex(0)
            elif nodetype=='select':
                if isinstance(editor,QtGui.QWidget):
                    editor.setCurrentIndex(0)
                else:
                    editor.button(0).setChecked(True)
            elif nodetype=='datetime':
                editor.setDateTime(QtCore.QDateTime())
                #editor.setDateTime(QtCore.QDateTime.currentDateTime().toUTC())
            elif nodetype=='duration':
                editor.setValue(None)
            elif nodetype=='file':
                editor.setNode(node)
        else:
            if nodetype=='string':
                editor.setText(value)
            elif nodetype=='int':
                editor.setValue(value)
            elif nodetype=='float':
                editor.setValue(value)
            elif nodetype=='bool':
                if isinstance(editor,QtGui.QCheckBox):
                    editor.setChecked(value)
                else:
                    for ioption in range(editor.count()):
                        optionvalue = editor.itemData(ioption).toBool()
                        if optionvalue==value:
                            editor.setCurrentIndex(ioption)
                            break
            elif nodetype=='select':
                if isinstance(editor,QtGui.QWidget):
                    for ioption in range(editor.count()):
                        optionvalue,ret = editor.itemData(ioption).toInt()
                        if optionvalue==value:
                            editor.setCurrentIndex(ioption)
                            break
                else:
                    editor.button(value).setChecked(True)
            elif nodetype=='datetime':
                value = datetime2qtdatetime(value)
                editor.setDateTime(value)
            elif nodetype=='duration':
                editor.setValue(value)
            elif nodetype=='file':
                editor.setNode(node)
        self.suppresschangeevent = False

    def setNodeData(self,editor,node):
        if isinstance(editor,QtGui.QGroupBox): return True
        nodetype = node.getValueType()
        if nodetype=='string':
            return node.setValue(editor.text())
        elif nodetype=='int':
            editor.interpretText()
            return node.setValue(editor.value())
        elif nodetype=='float':
            editor.interpretText()
            return node.setValue(editor.value())
        elif nodetype=='bool':
            if isinstance(editor,QtGui.QCheckBox):
                return node.setValue(editor.checkState()==QtCore.Qt.Checked)
            else:
                return node.setValue(editor.itemData(editor.currentIndex()).toBool())
        elif nodetype=='select':
            if isinstance(editor,QtGui.QWidget):
                optionvalue,ret = editor.itemData(editor.currentIndex()).toInt()
                return node.setValue(optionvalue)
            else:
                return node.setValue(editor.checkedId())
        elif nodetype=='datetime':
            value = editor.dateTime()
            value.setTimeSpec(QtCore.Qt.UTC)
            return node.setValue(qtdatetime2datetime(value))
        elif nodetype=='duration':
            return node.setValue(editor.value())
        elif nodetype=='file':
            return node.setValue(editor.datafile)

def getFontSubstitute(fontname):
    assert isinstance(fontname,basestring), 'Supplied argument must be a string.'

    substitute = fontname

    if sys.platform=='win32':
        # Windows has a font substitution table in the registry, which links
        # "virtual" fonts (e.g. the "dialog box font") to their actual TrueType
        # name. Look up the supplied font name in this table, and return the
        # substitute if present.
        import _winreg
        hkey = None
        try:
            hkey = _winreg.OpenKey(_winreg.HKEY_LOCAL_MACHINE,r'SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes')
            value,datatype = _winreg.QueryValueEx(hkey,fontname)
            substitute = value
        except WindowsError:
            pass
        except EnvironmentError:
            pass
        if hkey!=None:
            _winreg.CloseKey(hkey)
        
    return substitute

class FigureToolbar(matplotlib.backend_bases.NavigationToolbar2):
    """Class derived from MatPlotLib NavigationToolbar2, used only for its
    zooming code. Code for some methods (rectange drawing) has been taken from
    the NavigationToolbar2QT from backend_qt4. The draw method that would normally
    force a canvas redraw has been reimplemented to call a callback specified at
    initialization. Thus the axes changes can be caught, and reflected in the
    XML-based plot properties."""
        
    def __init__( self, canvas, callback=None):
        matplotlib.backend_bases.NavigationToolbar2.__init__( self, canvas )
        self.callback = callback
        
    def _init_toolbar( self ):
        pass

    def dynamic_update( self ):
        self.canvas.draw()

    def set_cursor( self, cursor ):
        cursord = {
            matplotlib.backend_bases.cursors.MOVE          : QtCore.Qt.PointingHandCursor,
            matplotlib.backend_bases.cursors.HAND          : QtCore.Qt.WaitCursor,
            matplotlib.backend_bases.cursors.POINTER       : QtCore.Qt.ArrowCursor,
            matplotlib.backend_bases.cursors.SELECT_REGION : QtCore.Qt.CrossCursor,
            }
        self.canvas.setCursor(QtGui.QCursor(cursord[cursor]))
                
    def draw_rubberband( self, event, x0, y0, x1, y1 ):
        height = self.canvas.figure.bbox.height()
        y1 = height - y1
        y0 = height - y0
        
        w = abs(x1 - x0)
        h = abs(y1 - y0)

        rect = [ int(val)for val in min(x0,x1), min(y0, y1), w, h ]
        self.canvas.drawRectangle( rect )

    def draw(self):
        if self.callback!=None: self.callback()

    def mouse_move(self, event):
        #print 'mouse_move', event.button

        if not self._active:
            if self._lastCursor != matplotlib.backend_bases.cursors.POINTER:
                self.set_cursor(matplotlib.backend_bases.cursors.POINTER)
                self._lastCursor = matplotlib.backend_bases.cursors.POINTER
        else:
            if self._active=='ZOOM':
                if self._lastCursor != matplotlib.backend_bases.cursors.SELECT_REGION:
                    self.set_cursor(matplotlib.backend_bases.cursors.SELECT_REGION)
                    self._lastCursor = matplotlib.backend_bases.cursors.SELECT_REGION
                if self._xypress:
                    x, y = event.x, event.y
                    lastx, lasty, a, ind, lim, trans= self._xypress[0]
                    bb = a.bbox
                    if   x<bb.xmin(): x = bb.xmin()
                    elif x>bb.xmax(): x = bb.xmax()
                    if   y<bb.ymin(): y = bb.ymin()
                    elif y>bb.ymax(): y = bb.ymax()
                    self.draw_rubberband(event, x, y, lastx, lasty)
            elif (self._active=='PAN' and
                  self._lastCursor != cursors.MOVE):
                self.set_cursor(matplotlib.backend_bases.cursors.MOVE)

                self._lastCursor = matplotlib.backend_bases.cursors.MOVE

        if event.inaxes and event.inaxes.get_navigate():

            try: s = event.inaxes.format_coord(event.xdata, event.ydata)
            except ValueError: pass
            except OverflowError: pass
            else:
                if len(self.mode):
                    self.set_message('%s : %s' % (self.mode, s))
                else:
                    self.set_message(s)
        else: self.set_message(self.mode)
        
class FigurePanel(QtGui.QWidget):
    
    def __init__(self,parent,detachbutton=True):
        QtGui.QWidget.__init__(self,parent)

        # Create MatPlotLib figure with background and border colors equal to our background color.
        bgcolor = self.palette().window().color()
        mplfigure = matplotlib.figure.Figure(facecolor=(bgcolor.red()/255., bgcolor.green()/255., bgcolor.blue()/255.),edgecolor=(bgcolor.red()/255., bgcolor.green()/255., bgcolor.blue()/255.),dpi=self.logicalDpiX())

        # Create MatPlotLib canvas (Qt-backend) attached to our MatPlotLib figure.
        self.canvas = FigureCanvas(mplfigure)
        self.canvas.setSizePolicy(QtGui.QSizePolicy.Expanding,QtGui.QSizePolicy.Expanding)
        self.canvas.setMinimumSize(300,250)

        # Create our figure that encapsulates MatPlotLib figure.
        deffont = getFontSubstitute(unicode(self.fontInfo().family()))
        self.figure = plot.Figure(mplfigure,defaultfont=deffont)
        self.figure.registerCallback('completeStateChange',self.onFigureStateChanged)
        
        self.navtoolbar = FigureToolbar(self.canvas,self.updateAxesBounds)

        self.factory = PropertyEditorFactory(self.figure.properties,live=True,allowhide=True)

        layout = QtGui.QVBoxLayout()

        self.layoutButtons = QtGui.QHBoxLayout()

        # Button for showing/hiding properties
        self.buttonProperties = QtGui.QPushButton(self.tr('&Properties'),self)
        self.buttonProperties.setAutoDefault(False)
        self.buttonProperties.setDefault(False)
        self.connect(self.buttonProperties, QtCore.SIGNAL('clicked()'), self.onAdvancedClicked)
        self.layoutButtons.addWidget(self.buttonProperties)

        # Button for zooming
        self.buttonZoom = QtGui.QPushButton(self.tr('Zoom'),self)
        self.buttonZoom.setAutoDefault(False)
        self.buttonZoom.setDefault(False)
        self.connect(self.buttonZoom, QtCore.SIGNAL('clicked()'), self.onZoomClicked)
        self.buttonZoom.setCheckable(True)
        self.layoutButtons.addWidget(self.buttonZoom)

        # Button for reset view
        self.buttonResetView = QtGui.QPushButton(self.tr('Reset view'),self)
        self.buttonResetView.setAutoDefault(False)
        self.buttonResetView.setDefault(False)
        self.connect(self.buttonResetView, QtCore.SIGNAL('clicked()'), self.onResetViewClicked)
        self.layoutButtons.addWidget(self.buttonResetView)

        # Button for exporting to file
        self.buttonExport = QtGui.QPushButton(self.tr('&Export to file...'),self)
        self.buttonExport.setAutoDefault(False)
        self.buttonExport.setDefault(False)
        self.connect(self.buttonExport, QtCore.SIGNAL('clicked()'), self.onExport)
        self.layoutButtons.addWidget(self.buttonExport)

        # Button for printing
        self.buttonPrint = QtGui.QPushButton(self.tr('&Print...'),self)
        self.buttonPrint.setAutoDefault(False)
        self.buttonPrint.setDefault(False)
        self.connect(self.buttonPrint, QtCore.SIGNAL('clicked()'), self.onPrint)
        self.layoutButtons.addWidget(self.buttonPrint)

        # Detach button
        if detachbutton:
            self.buttonDetach = QtGui.QPushButton('&Detach figure',self)
            self.buttonDetach.setAutoDefault(False)
            self.buttonDetach.setDefault(False)
            self.connect(self.buttonDetach, QtCore.SIGNAL('clicked()'), self.onDetach)
            self.layoutButtons.addWidget(self.buttonDetach)

        layout.addWidget(self.canvas)
        layout.addLayout(self.layoutButtons)

        self.setLayout(layout)

        self.dialogAdvanced = None

        # Initially disable all controls; we have no plot to configure yet...
        self.setEnabled(False)

        self.detachedfigures = []
        
        self.axescontrols = []
        
    def onFigureStateChanged(self,complete):
        self.setEnabled(complete)

    def plot(self,varname,varstore=None):
        ownupdating = self.figure.updating
        if ownupdating: self.figure.setUpdating(False)
        self.figure.clearVariables()
        self.figure.clearProperties(deleteoptional=False)
        if isinstance(varstore,basestring) or varstore==None:
            self.figure.addVariable(varname,source=varstore)
        else:
            self.figure.clearSources()
            self.figure.addDataSource('main',varstore)
            self.figure.addVariable(varname)
        if ownupdating: self.figure.setUpdating(True)
        self.figure.resetChanged()
        
    def afterFigureStoreChange(self):
        for i in range(len(self.axescontrols)-1,-1,-1):
            self.destroyAxisControls(i)
        for node in self.figure.properties['Axes'].getLocationMultiple(['Axis']):
            self.createAxisControls(node)
        
    def beforeFigureNodeVisibilityChange(self,node,visible,showhideonly):
        if showhideonly: return
        if (not visible) and '/'.join(node.location) == 'Axes/Axis':
            axisid = node.getSecondaryId()
            for i in range(len(self.axescontrols)-1,-1,-1):
                if self.axescontrols[i]['axis']==axisid:
                    self.destroyAxisControls(i)

    def afterFigureNodeVisibilityChange(self,node,visible,showhideonly):
        if showhideonly: return
        if visible and '/'.join(node.location) == 'Axes/Axis':
            self.createAxisControls(node)
            
    def destroyAxisControls(self,iaxis):
        for ed in self.axescontrols[iaxis]['editors']:
            self.factory.destroyEditor(ed,layout=self.layoutAxesRange)
        self.axescontrols.pop(iaxis)
            
    def createAxisControls(self,node):
        axisid = node.getSecondaryId()
        axisname = axisid
        irow = self.layoutAxesRange.rowCount()

        editorMin = self.factory.createEditor(node['Minimum'],self)
        editorMax = self.factory.createEditor(node['Maximum'],self)
        editorMin.createLabel(text='%s range: ' % axisname)
        editorMax.createLabel(text=' to ')
        editorMin.addToGridLayout(self.layoutAxesRange,irow,0)
        editorMax.addToGridLayout(self.layoutAxesRange,irow,3)
        self.axescontrols.append({'axis':axisid,'editors':[editorMin,editorMax]})
        
        irow+=1

        editorMinTime = self.factory.createEditor(node['MinimumTime'],self)
        editorMaxTime = self.factory.createEditor(node['MaximumTime'],self)
        editorMinTime.createLabel(text='%s range: ' % axisname)
        editorMaxTime.createLabel(text=' to ')
        editorMinTime.addToGridLayout(self.layoutAxesRange,irow,0)
        editorMaxTime.addToGridLayout(self.layoutAxesRange,irow,3)
        self.axescontrols.append({'axis':axisid,'editors':[editorMinTime,editorMaxTime]})

    def plotFromProperties(self,properties):
        self.figure.setProperties(properties)    

    def clear(self):
        self.figure.clearProperties()
        self.figure.clearSources()

    def closeDetached(self):
        for ch in self.detachedfigures: ch.close()

    def onAdvancedClicked(self):
        if self.dialogAdvanced==None:
            self.dialogAdvanced = PropertyEditorDialog(self,self.figure.properties,title='Figure properties',flags=QtCore.Qt.Tool)
            self.dialogAdvanced.resize(350, 300)
            self.dialogAdvanced.resizeColumns()
        self.dialogAdvanced.show()
        self.dialogAdvanced.activateWindow()
        
    def onZoomClicked(self,*args):
        self.navtoolbar.zoom( self, *args )

    def updateAxesBounds(self):
        a = self.canvas.figure.gca()
        Xmin,Xmax=a.get_xlim()
        Ymin,Ymax=a.get_ylim()
        axes = self.figure.properties['Axes']
        xaxis = axes.getChildById('Axis','x')
        yaxis = axes.getChildById('Axis','y')
        oldupdating = self.figure.setUpdating(False)
        for axis,minval,maxval in ((xaxis,Xmin,Xmax),(yaxis,Ymin,Ymax)):
            if axis['IsTimeAxis'].getValue(usedefault=True):
                axis['MinimumTime'].setValue(common.num2date(minval))
                axis['MaximumTime'].setValue(common.num2date(maxval))
            else:
                axis['Minimum'].setValue(minval)
                axis['Maximum'].setValue(maxval)
        self.figure.setUpdating(oldupdating)

    def onResetViewClicked(self,*args):
        if self.buttonZoom.isChecked(): self.buttonZoom.click()
        axes = self.figure.properties['Axes']
        xaxis = axes.getChildByNumber('Axis',0)
        yaxis = axes.getChildByNumber('Axis',1)
        oldupdating = self.figure.setUpdating(False)
        for axis in (xaxis,yaxis):
            axis['MinimumTime'].clearValue()
            axis['MaximumTime'].clearValue()
            axis['Minimum'].clearValue()
            axis['Maximum'].clearValue()
        self.figure.setUpdating(oldupdating)

    def onPropertiesClicked(self):
        window = getTopLevelWidget(self)
        window.setUpdatesEnabled(False)
        sz = window.size()
        if not self.widgetProperties.isVisible():
            self.widgetProperties.setVisible(True)
            self.buttonProperties.setText('Hide plot &properties')
            self.propheight = self.widgetProperties.height()+self.layout().spacing()
            sz.setHeight(sz.height() + self.propheight)
        else:
            self.widgetProperties.setVisible(False)
            self.buttonProperties.setText('Edit plot &properties')
            sz.setHeight(sz.height() - self.propheight)
        window.resize(sz)
        window.setUpdatesEnabled(True)

    class ExportSettings(QtGui.QDialog):
        """Dialog with settings for figure export
        """
        def __init__(self,parent=None):
            QtGui.QDialog.__init__(self,parent,QtCore.Qt.Dialog | QtCore.Qt.MSWindowsFixedSizeDialogHint | QtCore.Qt.WindowTitleHint)
            
            layout = QtGui.QGridLayout()
            
            labWidth = QtGui.QLabel('Width:',self)
            labHeight = QtGui.QLabel('Height:',self)
            labResolution = QtGui.QLabel('Resolution:',self)
            layout.addWidget(labWidth,     0,0)
            layout.addWidget(labHeight,    1,0)
            layout.addWidget(labResolution,2,0)

            self.editWidth = ScientificDoubleEditor(self)
            self.editHeight = ScientificDoubleEditor(self)
            self.editResolution = ScientificDoubleEditor(self)
            layout.addWidget(self.editWidth,     0,1)
            layout.addWidget(self.editHeight,    1,1)
            layout.addWidget(self.editResolution,2,1)
            
            labWidthUnit = QtGui.QLabel('cm',self)
            labHeightUnit = QtGui.QLabel('cm',self)
            labResolutionUnit = QtGui.QLabel('dpi',self)
            layout.addWidget(labWidthUnit,     0,2)
            layout.addWidget(labHeightUnit,    1,2)
            layout.addWidget(labResolutionUnit,2,2)
            
            layout.setColumnStretch(3,1)

            layoutButtons = QtGui.QHBoxLayout()

            # Add "OK" button
            self.bnOk = QtGui.QPushButton('&OK',self)
            self.connect(self.bnOk, QtCore.SIGNAL('clicked()'), self.accept)
            layoutButtons.addWidget(self.bnOk)

            # Add "Cancel" button
            self.bnCancel = QtGui.QPushButton('&Cancel',self)
            self.connect(self.bnCancel, QtCore.SIGNAL('clicked()'), self.reject)
            layoutButtons.addWidget(self.bnCancel)
            
            layout.addLayout(layoutButtons,3,0,1,4)

            self.setLayout(layout)
            
            self.setWindowTitle('Figure export settings')

    def onExport(self):
        dialog = self.ExportSettings(self)
        oldwidth = self.canvas.figure.get_figwidth()
        oldheight = self.canvas.figure.get_figheight()
        olddpi = self.canvas.figure.get_dpi()
        dialog.editWidth.setValue(oldwidth*2.54,'%.1f')
        dialog.editHeight.setValue(oldheight*2.54,'%.1f')
        dialog.editResolution.setValue(olddpi,'%.0f')
        if dialog.exec_()!=QtGui.QDialog.Accepted: return
        fname = QtGui.QFileDialog.getSaveFileName(self,'Choose location to save plot to','','Portable Network Graphics (*.png);;Encapsulated PostScript (*.eps);;Scalable Vector Graphics (*.svg);;Bitmap (*.bmp)')
        if fname:
            width = dialog.editWidth.value()/2.54
            height = dialog.editHeight.value()/2.54
            QtGui.qApp.setOverrideCursor(QtCore.Qt.WaitCursor)
            agg = self.canvas.switch_backends(FigureCanvasAgg)
            self.canvas.figure.set_figwidth(width)
            self.canvas.figure.set_figheight(height)
            agg.print_figure(str(fname.toLatin1()),dpi=dialog.editResolution.value(), facecolor='w', edgecolor='w', orientation='portrait')
            self.canvas.figure.set_figwidth(oldwidth)
            self.canvas.figure.set_figheight(oldheight)
            self.canvas.figure.set_canvas(self.canvas)
            QtGui.qApp.restoreOverrideCursor()
        
    def onPrint(self):
        printer = QtGui.QPrinter(QtGui.QPrinter.HighResolution)
        printDialog = QtGui.QPrintDialog(printer, self)
        if printDialog.exec_() == QtGui.QDialog.Accepted:
            print 'Printing to %s' % printer.printerName()
            p = QtGui.QPainter(printer)
            
            canvas = self.canvas.switch_backends(FigureCanvasAgg)

            # Store current DPI and colors.
            origDPI       = canvas.figure.dpi.get()
            origfacecolor = canvas.figure.get_facecolor()
            origedgecolor = canvas.figure.get_edgecolor()

            res = printer.resolution()
            if res>600: res=600
            print 'Using resolution of %i dpi.' % res

            # Adjust DPI and colors for printer.
            canvas.figure.dpi.set(res)
            canvas.figure.set_facecolor('w')
            canvas.figure.set_edgecolor('w')

            # Draw the plot (in memory)
            print 'MatPlotLib: creating Agg in-memory bitmap.'
            canvas.draw()

            # matplotlib is in rgba byte order.
            # qImage wants to put the bytes into argb format and
            # is in a 4 byte unsigned int.  little endian system is LSB first
            # and expects the bytes in reverse order (bgra).
            print 'Converting in-memory bitmap to format suitable for Qt.'
            if (QtCore.QSysInfo.ByteOrder == QtCore.QSysInfo.LittleEndian):
                stringBuffer = canvas.renderer._renderer.tostring_bgra()
            else:
                stringBuffer = canvas.renderer._renderer.tostring_argb()
                
            print 'Creating QImage object from in-memory data. Using width = %.2f, height = %.2f' % (canvas.renderer.width,canvas.renderer.height)
            qImage = QtGui.QImage(stringBuffer, canvas.renderer.width, canvas.renderer.height, QtGui.QImage.Format_ARGB32)
            #pixmap = QtGui.QPixmap.fromImage(qImage)
            #print 'Drawing pixmap to QPainter object connected to printer.'
            #p.drawPixmap(QtCore.QPoint(0, 0), pixmap )
            print 'Drawing bitmap to QPainter object connected to printer.'
            p.drawImage(QtCore.QPoint(0, 0), qImage)
            #p.drawImage(QtCore.QPoint(0, 0), qImage, QtCore.QRect(0,0,1000,1000))
            print 'Done drawing; closing painter.'
            p.end()

            print 'printing done; restoring original figure resolution and color.'

            # Restore original DPI and colors.
            canvas.figure.dpi.set(origDPI)
            canvas.figure.set_facecolor(origfacecolor)
            canvas.figure.set_edgecolor(origedgecolor)

            # Restore original canvas.
            self.figure.figure.set_canvas(self.canvas)

    def onDetach(self):
        fd = FigureDialog(self,sourcefigure=self.figure)
        fd.show()
        self.detachedfigures.append(fd)
        
    def closeEvent(self,ev):
        self.closeDetached()
        if self.dialogAdvanced!=None: self.dialogAdvanced.close()
        QtGui.QWidget.closeEvent(self,ev)

class FigureDialog(QtGui.QDialog):
    
    def __init__(self,parent,varstore=None,varname=None,sourcefigure=None,figureproperties=None,quitonclose=False,closebutton=None):
        QtGui.QDialog.__init__(self,parent,QtCore.Qt.Window | QtCore.Qt.WindowMaximizeButtonHint | QtCore.Qt.WindowSystemMenuHint )

        if closebutton==None: closebutton = addCloseButton()
        
        self.setSizeGripEnabled(True)
        layout = QtGui.QVBoxLayout(self)
        self.panel = FigurePanel(self,detachbutton=False)
        layout.addWidget(self.panel)

        self.panel.figure.setUpdating(False)
        if sourcefigure!=None:
            # A figure to copy settings from is provided.
            properties = sourcefigure.getPropertiesCopy()
            self.panel.figure.sources = sourcefigure.sources
            self.panel.figure.defaultsource = sourcefigure.defaultsource
            self.panel.plotFromProperties(properties)
        elif figureproperties!=None:
            # An XML DOM tree with figure settings is provided
            assert varstore!=None,'If figure properties are specified, the variable store must be given as well.'
            self.panel.figure.addDataSource('main',varstore)
            self.panel.plotFromProperties(figureproperties)
        elif varstore!=None and varname!=None:
            # A figure store and variable name are provided.
            self.panel.figure.addDataSource('main',varstore)
            self.panel.figure.addVariable(varname)
        else:
            # Nothing provided; figure will be empty.
            assert varstore==None and varname==None,'If a variable is to be plotted, both the variable store and the variable name must be provided.'
        self.panel.figure.setUpdating(True)
        
        if closebutton:
            self.buttonClose = QtGui.QPushButton('&Close',self)
            self.buttonClose.setAutoDefault(False)
            self.buttonClose.setDefault(False)
            self.connect(self.buttonClose, QtCore.SIGNAL('clicked()'), self.accept)
            self.panel.layoutButtons.addWidget(self.buttonClose)

        title = self.panel.figure.properties['Title'].getValue(usedefault=True)
        if title==None: title = 'Figure'
        self.setWindowTitle(title)

        # Prevent this window from keeping the appliaction alive after the main window was closed.
        self.setAttribute(QtCore.Qt.WA_QuitOnClose,quitonclose)
        
        self.resize(500, 500)
        
    def getFigure(self):
        return self.panel.figure

class ProgressDialog(QtGui.QProgressDialog):
    def __init__(self,parent=None,minimumduration=500,title=None,suppressstatus=False):
        QtGui.QProgressDialog.__init__(self,'',QtCore.QString(),0,0,parent,QtCore.Qt.Dialog|QtCore.Qt.WindowTitleHint|QtCore.Qt.MSWindowsFixedSizeDialogHint)
        self.setModal(True)
        self.setMinimumDuration(minimumduration)
        self.setRange(0,0)
        self.suppressstatus = suppressstatus
        if title!=None: self.setWindowTitle(title)
            
    def onProgressed(self,progress,status):
        if progress!=None:
            if self.maximum()==0: self.setMaximum(100)
            self.setValue(int(100*progress))
        elif progressdialog.maximum()!=0:
            self.setValue(0)
        if not self.suppressstatus: self.setLabelText(status)
        QtGui.qApp.processEvents()
