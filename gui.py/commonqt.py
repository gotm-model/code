#!/usr/bin/python

#$Id: commonqt.py,v 1.28 2007-06-28 16:59:42 jorn Exp $

from PyQt4 import QtGui,QtCore
import datetime, re, os.path

import common,xmlstore,settings,plot,data

import matplotlib.figure
from matplotlib.backends.backend_qt4agg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.backends.backend_agg import FigureCanvasAgg

def getTopLevelWidget(child):
    parent = child.parent()
    if parent==None or (child.windowFlags() & QtCore.Qt.Window): return child
    return getTopLevelWidget(parent)

# =======================================================================
# Functions for converting between Qt date/time object and Python
# date/time objects
# =======================================================================

# qtdate2datetime: Convert Qt QDate object to Python datetime object
def qtdate2datetime(qtdate):
    return datetime.datetime(qtdate.year(),qtdate.month(),qtdate.day())

# datetime2qtdate: Convert Python datetime object to Qt QDate object
def datetime2qtdate(dt):
    return QtCore.QDate(dt.year,dt.month,dt.day)

# qtdatetime2datetime: Convert Qt QDateTime object to Python datetime object
def qtdatetime2datetime(qtdatetime):
    d = qtdatetime.date()
    t = qtdatetime.time()
    return datetime.datetime(d.year(),d.month(),d.day(),t.hour(),t.minute(),t.second())

# datetime2qtdatetime: Convert Python datetime object to Qt QDateTime object
def datetime2qtdatetime(dt):
    return QtCore.QDateTime(QtCore.QDate(dt.year,dt.month,dt.day),QtCore.QTime(dt.hour,dt.minute,dt.second))

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
            ErrorDialog.write(ev.error)
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
    import sys
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
    def write(string):
        """Creates the one and only ErrorDialog if it does not exist yet, then shows the
        provided error message in the dialog.
        """
        if ErrorDialog.errdlg==None:
            ErrorDialog.errdlg = ErrorDialog()
        ErrorDialog.errdlg.textedit.setPlainText(ErrorDialog.errdlg.textedit.toPlainText()+string)
        ErrorDialog.errdlg.show()
    
    def __init__(self,parent=None):
        if parent==None: parent = QtGui.QApplication.activeWindow()
        QtGui.QWidget.__init__(self,parent,QtCore.Qt.Tool | QtCore.Qt.WindowTitleHint)
        # QtCore.Qt.Dialog | QtCore.Qt.WindowStaysOnTopHint
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
        if self.getdirectory:
            path = unicode(QtGui.QFileDialog.getExistingDirectory(self,'',curpath))
        elif self.save:
            selfilt = QtCore.QString()
            path = unicode(QtGui.QFileDialog.getSaveFileName(self,'',curpath,self.filter,selfilt,self.dlgoptions))
            selfilt= unicode(selfilt)
        else:
            path = unicode(QtGui.QFileDialog.getOpenFileName(self,'',curpath,self.filter))
            
        # If the browse dialog was cancelled, just return.
        if path=='': return

        # if we are saving, make sure that the extension matches the filter selected.
        if self.save:
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
        
        self.setPath(path)

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
            data = self.datastore.data
            self.rowlabels = None
            self.datamatrix = None
            if self.datastore.type=='pointsintime':
                self.rowlabels = data[0]
                self.datamatrix = data[1]
                self.datelabels = True
            elif self.datastore.type=='profilesintime':
                if self.type==0:
                    if self.pos<len(data[1]):
                        self.rowlabels = data[1][self.pos]
                        self.datamatrix = data[2][self.pos]
                        self.datelabels = False
                else:
                    self.rowlabels = data[0]
                    self.datelabels = True
            else:
                assert False, 'Unknown data file type "%s".' % self.datastore.type
                
        def saveData(self):
            data = self.datastore.data
            if self.datastore.type=='pointsintime':
                data[0] = self.rowlabels
                data[1] = self.datamatrix
            elif self.datastore.type=='profilesintime':
                if self.type==0:
                    if self.pos<len(data[1]):
                        data[1][self.pos] = self.rowlabels
                        data[2][self.pos] = self.datamatrix
                else:
                    data[0] = self.rowlabels
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
                    value = qtdatetime2datetime(value.toDateTime())
                elif value.canConvert(QtCore.QVariant.Double):
                    (value,ok) = value.toDouble()
                else:
                    assert False, 'Do not know variant type %s.' % val.type()
                rowindex = index.row()
                colindex = index.column()
                if self.rowlabels!=None: colindex -= 1
                if colindex==-1:
                    newrowindex = self.rowlabels.searchsorted(value)
                    self.rowlabels[rowindex] = value
                    buflab = self.rowlabels[rowindex]
                    bufdata = self.datamatrix[rowindex,:].copy()
                    if newrowindex>rowindex:
                        self.rowlabels[rowindex:newrowindex-1] = self.rowlabels[rowindex+1:newrowindex]
                        self.rowlabels[newrowindex-1] = buflab
                        if self.datamatrix!=None:
                            self.datamatrix[rowindex:newrowindex-1,:] = self.datamatrix[rowindex+1:newrowindex,:]
                            self.datamatrix[newrowindex-1,:] = bufdata
                        if self.datastore.type=='profilesintime' and self.type!=0:
                            self.datastore.data[1].insert(newrowindex-1,self.datastore.data[1].pop(rowindex))
                            self.datastore.data[2].insert(newrowindex-1,self.datastore.data[2].pop(rowindex))
                        self.emitRowsChanged(rowindex,newrowindex-1)
                    elif newrowindex<rowindex:
                        self.rowlabels[newrowindex+1:rowindex+1] = self.rowlabels[newrowindex:rowindex]
                        self.rowlabels[newrowindex] = buflab
                        if self.datamatrix!=None:
                            self.datamatrix[newrowindex+1:rowindex+1,:] = self.datamatrix[newrowindex:rowindex,:]
                            self.datamatrix[newrowindex,:] = bufdata
                        if self.datastore.type=='profilesintime' and self.type!=0:
                            self.datastore.data[1].insert(newrowindex,self.datastore.data[1].pop(rowindex))
                            self.datastore.data[2].insert(newrowindex,self.datastore.data[2].pop(rowindex))
                        self.emitRowsChanged(newrowindex,rowindex)
                else:
                    self.datamatrix[rowindex,colindex] = value
                self.datastore.dataChanged()
                self.emit(QtCore.SIGNAL('dataChanged(const QModelIndex &,const QModelIndex &)'),index,index)
            return True
            
        def addRow(self):
            newrowindex = self.datamatrix.shape[0]
            self.beginInsertRows(QtCore.QModelIndex(),newrowindex,newrowindex)
            if self.datamatrix!=None:
                newrow = matplotlib.numerix.array([[0]*self.datamatrix.shape[1]],matplotlib.numerix.typecode(self.datamatrix))
                self.datamatrix = matplotlib.numerix.concatenate((self.datamatrix,newrow))
            if self.rowlabels!=None:
                if self.datelabels:
                    if newrowindex==0:
                        newval = datetime.datetime.today()
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
                if self.datastore.type=='pointsintime':
                    if section==0:
                        val = 'time'
                    else:
                        val = self.datastore.getVariableNames()[section-1]
                if self.datastore.type=='profilesintime' and self.type==0:
                    if section==0:
                        val = 'depth'
                    else:
                        val = self.datastore.getVariableNames()[section-1]
                return QtCore.QVariant(val)
            return QtCore.QVariant()
    
    def __init__(self,node,parent=None,datafile=None):
        QtGui.QDialog.__init__(self,parent)

        self.node = node
        self.varstore = data.LinkedFileVariableStore(self.node)

        lo = QtGui.QGridLayout()
        
        loLeft = QtGui.QVBoxLayout()

        # Left panel: data editor
        loDataEdit = QtGui.QHBoxLayout()
        if self.varstore.type=='profilesintime':
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
        if self.varstore.type=='profilesintime': self.listmodel.reset()
        
        # Update figure
        self.panel.figure.update()
        
        self.exportbutton.setEnabled(self.datafile.isValid())

    def onParseProgress(self,status,progress=None):
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

# =======================================================================
# LinkedFileEditor: a Qt widget for "editing" a linked file. Currently
# just displays a button that, when clicked, displays a separate dialog.
# =======================================================================

class LinkedFileEditor(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self, parent)

        lo = QtGui.QHBoxLayout()

        self.plotbutton = QtGui.QPushButton('...',self)
        lo.addWidget(self.plotbutton)

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
        except:
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
        except:
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

    def setValue(self,value):
        strvalue = unicode(value)
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

    def createNodeEditor(self,node,parent):
        templatenode = node.templatenode
        nodetype = node.getValueType()
        editor = None
        if nodetype=='string':
            editor = QtGui.QLineEdit(parent)
        elif nodetype=='int':
            editor = QtGui.QSpinBox(parent)
            if templatenode.hasAttribute('minimum'): editor.setMinimum(int(templatenode.getAttribute('minimum')))
            if templatenode.hasAttribute('maximum'): editor.setMaximum(int(templatenode.getAttribute('maximum')))
            if templatenode.hasAttribute('unit'):    editor.setSuffix(' '+templatenode.getAttribute('unit'))
        elif nodetype=='float':
            editor = ScientificDoubleEditor(parent)
            if templatenode.hasAttribute('minimum'): editor.setMinimum(float(templatenode.getAttribute('minimum')))
            if templatenode.hasAttribute('maximum'): editor.setMaximum(float(templatenode.getAttribute('maximum')))
            if templatenode.hasAttribute('unit'):    editor.setSuffix(' '+templatenode.getAttribute('unit'))
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
        elif nodetype=='file':
            editor = LinkedFileEditor(parent)
            self.currenteditor = editor
        elif nodetype=='color':
            editor = ColorEditor(parent)
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
        elif nodetype=='file':
            editor.setNode(node)
        elif nodetype=='color':
            editor.setColor(QtGui.QColor(value))

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
            model.setData(index, QtCore.QVariant(editor.dateTime()))
        elif nodetype=='file':
            node.setValue(editor.datafile)
        elif nodetype=='color':
            model.setData(index,QtCore.QVariant(editor.color()))

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
        self.storeinterface.blockNotifyOfHiddenNodes = (not self.nohide)

        self.storeinterface.addVisibilityChangeHandler(self.beforeNodeVisibilityChange,self.afterNodeVisibilityChange)
        self.storeinterface.addChangeHandler(self.onNodeChanged)
        self.storeinterface.addStoreChangedHandler(self.reset)

        self.inheritingchecks = False
        
    # index (inherited from QtCore.QAbstractItemModel)
    #   Supplies unique index for the node at the given (row,column) position
    #   below the given parent (specified as index).
    def index(self,irow,icolumn,parent):
        if not parent.isValid():
            parentnode = self.typedstore.root
        else:
            parentnode = parent.internalPointer()
        child = self.storeinterface.getChildByIndex(parentnode,irow)
        assert child!=None, 'Cannot find child with index %i below %s.' % (irow,parentnode)
        assert isinstance(child,xmlstore.TypedStore.Node), 'Object returned by getChildByIndex is not of type "Node" (but "%s").' % child
        return self.createIndex(irow,icolumn,child)

    # parent (inherited from QtCore.QAbstractItemModel)
    #   Supplies unique index for the parent of the given node (specified as index).
    def parent(self,index):
        # We must have a valid index
        assert index.isValid(), 'Asked for parent of root node (invalid index), but Qt asker knows it is the root.'

        current = index.internalPointer()
        assert isinstance(current,xmlstore.TypedStore.Node), 'Node data is not a TypedStore.Node, but: %s.' % current
        parent = self.storeinterface.getParent(current)
        assert isinstance(parent,xmlstore.TypedStore.Node), 'Object returned by getParent is not of type "Node" (but "%s").' % (parent,)

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
                for optionnode in optionnodes:
                    text += '\n- '
                    if optionnode.hasAttribute('description'):
                        text += optionnode.getAttribute('description')
                    else:
                        text += optionnode.getAttribute('label')
            elif nodetype=='int' or nodetype=='float':
                if templatenode.hasAttribute('minimum'): text += '\nminimum value: '+templatenode.getAttribute('minimum')
                if templatenode.hasAttribute('maximum'): text += '\nmaximum value: '+templatenode.getAttribute('maximum')
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
                #if fieldtype=='color':
                #    value = node.getValueOrDefault()
                #    pm = QtGui.QPixmap(10,10)
                #    pm.fill(QtGui.QColor(value.red,value.green,value.blue))
                #    #return QtCore.QVariant(QtGui.QIcon(pm))
                #    return QtCore.QVariant(pm)
                return QtCore.QVariant(node.getValueAsString(usedefault=True))
            elif role==QtCore.Qt.EditRole:
                value = node.getValueOrDefault()
                if value==None: return QtCore.QVariant()
                if fieldtype=='datetime':
                    # First convert Python datetime to QDateTime, then cast to variant.
                    return QtCore.QVariant(datetime2qtdatetime(value))
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
            if templatenode.hasAttribute('minimum'):
                if value<int(templatenode.getAttribute('minimum')): return False
            if templatenode.hasAttribute('maximum'):
                if value>int(templatenode.getAttribute('maximum')): return False
        elif fieldtype=='float':
            value,converted = value.toDouble()
            if not converted: return False
            if templatenode.hasAttribute('minimum'):
                if value<float(templatenode.getAttribute('minimum')): return False
            if templatenode.hasAttribute('maximum'):
                if value>float(templatenode.getAttribute('maximum')): return False
        elif fieldtype=='bool':
            value = value.toBool()
        elif fieldtype=='datetime':
            value = qtdatetime2datetime(value.toDateTime())
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
        assert isinstance(node,xmlstore.TypedStore.Node), 'Supplied object is not of type "Node" (but "%s").' % node
        if self.nohide and showhide: return
        irow = self.storeinterface.getOwnIndex(node)
        index = self.createIndex(irow,1,node)
        par = self.parent(index)
        if newvisibility:
            self.beginInsertRows(par,irow,irow)
        else:
            self.beginRemoveRows(par,irow,irow)

    def afterNodeVisibilityChange(self,node,newvisibility,showhide):
        assert isinstance(node,xmlstore.TypedStore.Node), 'Supplied object is not of type "Node" (but "%s").' % node
        if self.nohide and showhide: return self.onNodeChanged(node,headertoo=True)
        if newvisibility:
            self.endInsertRows()
        else:
            self.endRemoveRows()

    def onNodeChanged(self,node,headertoo = False):
        assert isinstance(node,xmlstore.TypedStore.Node), 'Supplied object is not of type "Node" (but "%s").' % node
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
    
    def __init__(self,parent=None,sequence=None,closebutton=False):
        QtGui.QDialog.__init__(self, parent, QtCore.Qt.Window|QtCore.Qt.WindowContextHelpButtonHint)

        layout = QtGui.QVBoxLayout()

        self.pm = QtGui.QPixmap('./logo.png','PNG')
        self.piclabel = QtGui.QLabel(self)
        self.piclabel.setPixmap(self.pm)
        self.piclabel.setScaledContents(True)
        layout.addWidget(self.piclabel)
        layout.setMargin(0)

        bnlayout = QtGui.QHBoxLayout()
        bnlayout.addStretch()

        self.bnHome = QtGui.QPushButton('&Home',self)
        self.connect(self.bnHome, QtCore.SIGNAL("clicked()"), self.onHome)
        bnlayout.addWidget(self.bnHome)

        self.bnBack = QtGui.QPushButton('< &Back',self)
        self.connect(self.bnBack, QtCore.SIGNAL("clicked()"), self.onBack)
        bnlayout.addWidget(self.bnBack)

        self.bnNext = QtGui.QPushButton('&Next >',self)
        self.connect(self.bnNext, QtCore.SIGNAL("clicked()"), self.onNext)
        bnlayout.addWidget(self.bnNext)

        if closebutton:
            self.bnClose = QtGui.QPushButton('&Close',self)
            self.connect(self.bnClose, QtCore.SIGNAL("clicked()"), self.onClose)
            bnlayout.addWidget(self.bnClose)

        bnlayout.setMargin(11)
        layout.addLayout(bnlayout)

        self.setLayout(layout)

        self.shared = {}

        self.settings = settings.SettingsStore()

        self.sequence = sequence
        self.currentpage = None

    def setProperty(self,propertyname,value):
        if propertyname in self.shared and isinstance(self.shared[propertyname],common.referencedobject):
            self.shared[propertyname].release()
        self.shared[propertyname] = value

    def getProperty(self,propertyname):
        if propertyname not in self.shared: return None
        return self.shared[propertyname]

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

    def onClose(self):
        self.close()

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

    def __init__(self,typedstore,live=False):
        self.store = typedstore
        self.changed = False
        self.live = live
        self.editors = []

        if self.live:
            self.storeinterface = xmlstore.TypedStoreInterface(self.store)
            self.storeinterface.addChangeHandler(self.onStoreNodeChanged)
            self.storeinterface.addVisibilityChangeHandler(None,self.onStoreVisibilityChanged)
            self.storeinterface.addStoreChangedHandler(self.onStoreChanged)

    def createEditor(self,location,parent):
        node = self.store.root.getLocation(location)
        assert node!=None, 'Unable to create editor for "%s"; this node does not exist.' % location
        editor = PropertyEditor(node,parent)
        editor.addChangeHandler(self.onNodeEdited)
        self.editors.append(editor)
        return editor

    def updateStore(self):
        for editor in self.editors:
            editor.updateStore()

    def hasChanged(self):
        return self.changed

    def onStoreNodeChanged(self,node):
        for editor in self.editors:
            if editor.node is node:
                editor.updateEditorValue()
                break

    def onStoreVisibilityChanged(self,node,visible,showhide):
        if not showhide: return
        for editor in self.editors:
            editor.updateEditorEnabled()

    def onStoreChanged(self):
        for editor in self.editors:
            editor.node = self.store.root.getLocation(editor.location)
            editor.updateEditorValue()
            editor.updateEditorEnabled()

    def onNodeEdited(self,editor):
        self.changed = True
        if self.live:
            if not editor.updateStore(): editor.updateEditorValue()

class PropertyEditor:

    def __init__(self,node,parent):
        self.node = node
        self.editor = self.createEditor(node,parent)
        self.updateEditorValue()
        self.updateEditorEnabled()
        self.changehandlers = []
        self.suppresschangeevent = False
        self.location = node.location[:]
        
    def createLabel(self):
        text = self.node.getText(detail=1,capitalize=True)+': '
        lab = QtGui.QLabel(text,self.editor.parent())
        return lab

    def updateStore(self):
        return self.setNodeData(self.editor,self.node)

    def updateEditorValue(self):
        self.setEditorData(self.editor,self.node)

    def updateEditorEnabled(self):
        self.editor.setEnabled(not self.node.isHidden())

    def addChangeHandler(self,callback):
        self.changehandlers.append(callback)

    def onChange(self):
        if not self.suppresschangeevent:
            for callback in self.changehandlers:
                callback(self)

    def createEditor(self,node,parent):
        templatenode = node.templatenode
        nodetype = node.getValueType()
        editor = None
        if nodetype=='string':
            editor = QtGui.QLineEdit(parent)
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='int':
            editor = QtGui.QSpinBox(parent)
            if templatenode.hasAttribute('minimum'): editor.setMinimum(int(templatenode.getAttribute('minimum')))
            if templatenode.hasAttribute('maximum'): editor.setMaximum(int(templatenode.getAttribute('maximum')))
            if templatenode.hasAttribute('unit'):    editor.setSuffix(' '+templatenode.getAttribute('unit'))
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='float':
            editor = ScientificDoubleEditor(parent)
            if templatenode.hasAttribute('minimum'): editor.setMinimum(float(templatenode.getAttribute('minimum')))
            if templatenode.hasAttribute('maximum'): editor.setMaximum(float(templatenode.getAttribute('maximum')))
            if templatenode.hasAttribute('unit'):    editor.setSuffix(' '+templatenode.getAttribute('unit'))
            self.currenteditor = editor
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='bool':
            editor = QtGui.QComboBox(parent)
            editor.addItem('True',QtCore.QVariant(True))
            editor.addItem('False',QtCore.QVariant(False))
            editor.connect(editor, QtCore.SIGNAL("currentIndexChanged(int)"), self.onChange)
        elif nodetype=='select':
            editor = QtGui.QComboBox(parent)
            options = common.findDescendantNode(templatenode,['options'])
            if options==None: raise 'Node is of type "select" but lacks "options" childnode.'
            for ch in options.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                    editor.addItem(ch.getAttribute('label'),QtCore.QVariant(int(ch.getAttribute('value'))))
            editor.connect(editor, QtCore.SIGNAL("currentIndexChanged(int)"), self.onChange)
        elif nodetype=='datetime':
            editor = QtGui.QDateTimeEdit(parent)
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        elif nodetype=='file':
            editor = PathEditor(parent,compact=True)
            self.currenteditor = editor
            editor.connect(editor, QtCore.SIGNAL('editingFinished()'), self.onChange)
        else:
            assert False, 'Unknown node type "%s".' % nodetype
        editor.setWhatsThis(node.getText(detail=2,capitalize=True))
        return editor

    def setEditorData(self,editor,node):
        self.suppresschangeevent = True
        value = node.getValueOrDefault()
        nodetype = node.getValueType()
        if value==None:
            if nodetype=='string':
                editor.setText('')
            elif nodetype=='int':
                editor.setValue(0)
            elif nodetype=='float':
                editor.setText(editor.suffix)
            elif nodetype=='bool' or nodetype=='select':
                editor.setCurrentIndex(0)
            elif nodetype=='datetime':
                editor.setDateTime(QtCore.QDateTime())
            elif nodetype=='file':
                editor.setPath('')
        else:
            if nodetype=='string':
                editor.setText(value)
            elif nodetype=='int':
                editor.setValue(value)
            elif nodetype=='float':
                editor.setValue(value)
            elif nodetype=='bool':
                for ioption in range(editor.count()):
                    optionvalue = editor.itemData(ioption).toBool()
                    if optionvalue==value:
                        editor.setCurrentIndex(ioption)
                        break
            elif nodetype=='select':
                for ioption in range(editor.count()):
                    optionvalue,ret = editor.itemData(ioption).toInt()
                    if optionvalue==value:
                        editor.setCurrentIndex(ioption)
                        break
            elif nodetype=='datetime':
                editor.setDateTime(datetime2qtdatetime(value))
            elif nodetype=='file':
                editor.setPath(value)
        self.suppresschangeevent = False

    def setNodeData(self,editor,node):
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
            return node.setValue(editor.itemData(editor.currentIndex()).toBool())
        elif nodetype=='select':
            return node.setValue(editor.itemData(editor.currentIndex()).toInt())
        elif nodetype=='datetime':
            return node.setValue(qtdatetime2datetime(editor.dateTime()))
        elif nodetype=='file':
            return node.setValue(editor.path())

class FigurePanel(QtGui.QWidget):
    
    def __init__(self,parent,detachbutton=True):
        QtGui.QWidget.__init__(self,parent)

        # Create MatPlotLib figure with background and border colors equal to our background color.
        bgcolor = self.palette().window().color()
        mplfigure = matplotlib.figure.Figure(facecolor=(bgcolor.red()/255., bgcolor.green()/255., bgcolor.blue()/255.),edgecolor=(bgcolor.red()/255., bgcolor.green()/255., bgcolor.blue()/255.))

        # Create MatPlotLib canvas (Qt-backend) attached to our MatPlotLib figure.
        self.canvas = FigureCanvas(mplfigure)
        self.canvas.setSizePolicy(QtGui.QSizePolicy.Expanding,QtGui.QSizePolicy.Expanding)
        self.canvas.setMinimumSize(300,250)

        # Create our figure that encapsulates MatPlotLib figure.
        self.figure = plot.Figure(mplfigure)
        self.figure.registerCallback('completeStateChange',self.onFigureStateChanged)

        self.factory = PropertyEditorFactory(self.figure.properties,live=True)

        # Controls for time range.
        self.dateeditStart = self.factory.createEditor(['TimeAxis','Minimum'],self)
        self.dateeditStop  = self.factory.createEditor(['TimeAxis','Maximum'],self)

        # Controls for depth range.
        self.lineeditStartDepth = self.factory.createEditor(['DepthAxis','Minimum'],self)
        self.lineeditStopDepth = self.factory.createEditor(['DepthAxis','Maximum'],self)

        layout = QtGui.QVBoxLayout()
        
        layoutOptions = QtGui.QGridLayout()

        layoutTime = QtGui.QHBoxLayout()
        layoutTime.addWidget(self.dateeditStart.editor)
        self.labelDateTo = QtGui.QLabel(self.tr(' to '),self)
        layoutTime.addWidget(self.labelDateTo)
        layoutTime.addWidget(self.dateeditStop.editor)
        self.labelDateRange = QtGui.QLabel(self.tr('Time range:'),self)
        layoutOptions.addWidget(self.labelDateRange,1,0)
        layoutOptions.addLayout(layoutTime,1,1)
        
        layoutDepth = QtGui.QHBoxLayout()
        layoutDepth.addWidget(self.lineeditStartDepth.editor)
        self.labelDepthTo = QtGui.QLabel(self.tr(' to '),self)
        layoutDepth.addWidget(self.labelDepthTo)
        layoutDepth.addWidget(self.lineeditStopDepth.editor)
        self.labelDepthRange = QtGui.QLabel(self.tr('Depth range:'),self)
        layoutOptions.addWidget(self.labelDepthRange,2,0)
        layoutOptions.addLayout(layoutDepth,2,1)

        self.layoutOptions = layoutOptions

        self.buttonAdvanced = QtGui.QPushButton(self.tr('Advanced...'),self)
        self.buttonAdvanced.setAutoDefault(False)
        self.buttonAdvanced.setDefault(False)
        self.connect(self.buttonAdvanced, QtCore.SIGNAL('clicked()'), self.onAdvancedClicked)
        layoutOptions.addWidget(self.buttonAdvanced,3,0,1,2)

        self.layoutButtons = QtGui.QHBoxLayout()

        # Button for showing/hiding properties
        self.buttonProperties = QtGui.QPushButton(self.tr('Edit plot &properties'),self)
        self.buttonProperties.setAutoDefault(False)
        self.buttonProperties.setDefault(False)
        self.connect(self.buttonProperties, QtCore.SIGNAL('clicked()'), self.onPropertiesClicked)
        self.layoutButtons.addWidget(self.buttonProperties)

        # Button for exporting to file
        self.buttonExport = QtGui.QPushButton(self.tr('&Export to file'),self)
        self.buttonExport.setAutoDefault(False)
        self.buttonExport.setDefault(False)
        self.connect(self.buttonExport, QtCore.SIGNAL('clicked()'), self.onExport)
        self.layoutButtons.addWidget(self.buttonExport)

        # Button for printing
        self.buttonPrint = QtGui.QPushButton(self.tr('&Print'),self)
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

        self.widgetProperties = QtGui.QWidget(self)
        self.widgetProperties.setLayout(layoutOptions)
        self.widgetProperties.setVisible(False)

        layout.addWidget(self.canvas)
        layout.addLayout(self.layoutButtons)
        layout.addWidget(self.widgetProperties)

        self.setLayout(layout)

        self.dialogAdvanced = None

        # Initially disable all controls; we have no plot to configure yet...
        self.setEnabled(False)

        self.detachedfigures = []
        
    def onFigureStateChanged(self,complete):
        self.setEnabled(complete)

    def plot(self,varname,varstore=None):
        ownupdating = self.figure.updating
        if ownupdating: self.figure.setUpdating(False)
        self.figure.clearProperties()
        if isinstance(varstore,basestring) or varstore==None:
            self.figure.addVariable(varname,source=varstore)
        else:
            self.figure.clearSources()
            self.figure.addDataSource('main',varstore)
            self.figure.addVariable(varname)
        if ownupdating: self.figure.setUpdating(True)
        self.figure.resetChanged()

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

    def onExport(self):
        fname = QtGui.QFileDialog.getSaveFileName(self,'Choose location to save plot to','','Portable Network Graphics (*.png);;Encapsulated PostScript (*.eps);;Scalable Vector Graphics (*.svg);;Bitmap (*.bmp)')
        if fname:
            QtGui.qApp.setOverrideCursor(QtCore.Qt.WaitCursor)
            agg = self.canvas.switch_backends(FigureCanvasAgg)
            agg.print_figure(str(fname.toLatin1()),dpi=150, facecolor='w', edgecolor='w', orientation='portrait')
            self.canvas.figure.set_canvas(self.canvas)
            QtGui.qApp.restoreOverrideCursor()

            #self.canvas.print_figure( str(fname.toLatin1()) )
        
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
        QtGui.QWidget.closeEvent(self,ev)

class FigureDialog(QtGui.QDialog):
    
    def __init__(self,parent,sourcefigure):
        QtGui.QDialog.__init__(self,parent,QtCore.Qt.Window | QtCore.Qt.WindowMaximizeButtonHint | QtCore.Qt.WindowSystemMenuHint )

        self.setSizeGripEnabled(True)
        layout = QtGui.QVBoxLayout(self)
        self.panel = FigurePanel(self,detachbutton=False)
        layout.addWidget(self.panel)

        properties = sourcefigure.getPropertiesCopy()
        self.panel.figure.sources = sourcefigure.sources
        self.panel.figure.defaultsource = sourcefigure.defaultsource
        self.panel.plotFromProperties(properties)

        title = self.panel.figure.properties.getProperty(['Title'],usedefault=True)
        self.setWindowTitle(title)

        # Prevent this window from keeping the appliaction alive after the main window was closed.
        self.setAttribute(QtCore.Qt.WA_QuitOnClose,False)
        
        self.resize(500, 500)
