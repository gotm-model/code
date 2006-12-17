#!/usr/bin/python

#$Id: visualizer.py,v 1.4 2006-12-17 20:25:00 jorn Exp $

from PyQt4 import QtGui,QtCore

import common,commonqt

import matplotlib
matplotlib.use('Qt4Agg')
matplotlib.rcParams['numerix'] = 'numeric'

import sys,datetime
import xml.sax
import os.path

def loadResult(path):
    result = common.Result()

    try:
        if path.endswith('.gotmresult'):
            result.load(path)
        elif path.endswith('.nc'):
            result.attach(path)
        else:
            # We do not recognize this file type; try both GOTM result and NetCDF
            done = True
            try:
                result.attach(path)
            except Exception,e:
                done = False
            if not done:
                done = True
                try:
                    result.load(path)
                except Exception,e:
                    done = False
            if (not done):
                raise Exception('The file "'+path+'" is not a GOTM result or a NetCDF file.')
    except:
        result.unlink()
        result = None
        raise

    return result

class OpenWidget(QtGui.QWidget):
    def __init__(self,parent=None):
        QtGui.QWidget.__init__(self,parent)

        self.pathOpen = commonqt.PathEditor(self,header='File to open: ')
        self.pathOpen.filter = 'GOTM result files (*.gotmresult);;NetCDF files (*.nc);;All files (*.*)'

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.pathOpen)
        self.setLayout(layout)
        self.connect(self.pathOpen, QtCore.SIGNAL("onChanged()"), self.completeStateChanged)

    def completeStateChanged(self):
        self.emit(QtCore.SIGNAL('onCompleteStateChanged()'))

    def isComplete(self):
        return self.pathOpen.hasPath()

    def getResult(self):
        return loadResult(self.pathOpen.path())

class PageOpen(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('Specify the location of the result you want to view.',self)
        self.openwidget = OpenWidget(self)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addWidget(self.openwidget)
        layout.addStretch()
        self.setLayout(layout)
        self.connect(self.openwidget, QtCore.SIGNAL('onCompleteStateChanged()'), self.completeStateChanged)

    def isComplete(self):
        return self.openwidget.isComplete()

    def saveData(self,mustbevalid):
        if not mustbevalid: return True
        try:
            result = self.openwidget.getResult()
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
            return False
        self.parent().shared['result'] = result
        self.parent().shared['scenario'] = result.scenario
        return True

class PageSave(commonqt.WizardPage):

    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.result = parent.shared['result']

        self.label = QtGui.QLabel('Do you want to save the result of your simulation?',self)
        self.bngroup     = QtGui.QButtonGroup()
        self.radioNoSave = QtGui.QRadioButton('No, I do not want to save the result.', parent)
        self.radioSave   = QtGui.QRadioButton('Yes, I want to save the result to file.', parent)

        self.pathSave = commonqt.PathEditor(self,header='File to save to: ',save=True)
        self.pathSave.filter = 'GOTM result files (*.gotmresult);;All files (*.*)'
        self.pathSave.forcedextension = '.gotmresult'

        self.bngroup.addButton(self.radioNoSave, 0)
        self.bngroup.addButton(self.radioSave,   1)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addWidget(self.radioNoSave)
        layout.addWidget(self.radioSave)
        layout.addWidget(self.pathSave)
        layout.addStretch()
        self.setLayout(layout)

        self.connect(self.bngroup,  QtCore.SIGNAL("buttonClicked(int)"), self.onSourceChange)
        self.connect(self.pathSave, QtCore.SIGNAL("onChanged()"),        self.completeStateChanged)

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
            try:
                self.result.save(self.pathSave.path())
            except Exception,e:
                print e
                QtGui.QMessageBox.critical(self, 'Unable to save result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
                return False
        return True

class PageFinal(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.label = QtGui.QLabel('You are now done.',self)

        layout = QtGui.QVBoxLayout()
        layout.addWidget(self.label)
        layout.addStretch()
        self.setLayout(layout)

    def isComplete(self):
        return True

class PageVisualize(commonqt.WizardPage):
    
    def __init__(self,parent=None):
        commonqt.WizardPage.__init__(self, parent)

        self.result = parent.shared['result']

        self.treewidgetVars = QtGui.QTreeWidget(self)
        self.treewidgetVars.setColumnCount(1)
        self.treewidgetVars.header().hide()
        self.connect(self.treewidgetVars, QtCore.SIGNAL("itemSelectionChanged()"), self.OnVarSelected)
        self.treewidgetVars.setSizePolicy(QtGui.QSizePolicy.Minimum,QtGui.QSizePolicy.Expanding)
        self.treewidgetVars.setMaximumWidth(250)

        self.figurepanel = commonqt.FigurePanel(self)

        layout = QtGui.QHBoxLayout()
        layout.addWidget(self.treewidgetVars)
        layout.addWidget(self.figurepanel)
        self.setLayout(layout)

        self.BuildVariableTree()

    def OnVarSelected(self):
        if len(self.treewidgetVars.selectedItems())==0: return
        it = self.treewidgetVars.selectedItems()[0]
        varid = it.data(0,QtCore.Qt.UserRole)
        if varid.isNull(): return
        varname = varid.toString()
        self.figurepanel.plot(self.result,varname)

    def BuildVariableTree(self):
        self.treewidgetVars.clear()
        handler = self.TreeContentHandler(self.treewidgetVars,self.result)
        f = file(os.path.join(os.path.dirname(__file__),'varstructure.xml'), 'r')
        xml.sax.parse(f,handler)

    class TreeContentHandler(xml.sax.handler.ContentHandler):

        def __init__(self,treewidget,result):
            xml.sax.handler.ContentHandler.__init__(self)
            self.treewidget = treewidget
            self.dict = result.getVariableLongNames()
            self.curitem = self.treewidget
            self.dictFound = {}

        def startElement(self,name,attrs):
            if name=='variable':
                varid = attrs.getValue('id')
                if self.dict.has_key(varid):
                    varnode = QtGui.QTreeWidgetItem(self.curitem,[self.dict[varid]])
                    varnode.setData(0,QtCore.Qt.UserRole,QtCore.QVariant(varid))
                    self.dictFound[varid] = True
                else:
                    print 'Skipped variable '+varid+' because it is not present in result file.'
            elif name=='folder':
                self.curitem = QtGui.QTreeWidgetItem(self.curitem,[attrs.getValue('name')])
                self.curitem.setFlags(self.curitem.flags() & (~QtCore.Qt.ItemIsSelectable))

        def endElement(self,name):
            if name=='folder':
                self.curitem = self.curitem.parent()
                if self.curitem==None: self.curitem = self.treewidget

        def endDocument(self):
            # Find all variables that have not been added to the tree yet.
            # (because they were not known when the tree file was made)
            add = [];
            for varid in self.dict.keys():
                if not varid in self.dictFound: add.append(varid)

            # Add any left variables to the tree, below the node 'Other'
            if len(add)>0:
                # Add 'other' folder
                folderOther = QtGui.QTreeWidgetItem(self.treewidget,['Other'])
                folderOther.setFlags(folderOther.flags() & (~QtCore.Qt.ItemIsSelectable))

                # Sort variables by long name, case-insensitive.
                add.sort(lambda x,y: cmp(self.dict[x].lower(),self.dict[y].lower()))

                # Add remaining variables.
                for varid in add:
                    varnode = QtGui.QTreeWidgetItem(folderOther,[self.dict[varid]])
                    varnode.setData(0,QtCore.Qt.UserRole,QtCore.QVariant(varid))

    def isComplete(self):
        return True

def main():
    # Debug info
    print 'Python version: '+str(sys.version_info)
    print 'PyQt4 version: '+QtCore.PYQT_VERSION_STR
    print 'Qt version: '+QtCore.qVersion()
    print 'xml version: '+xml.__version__

    # Create the application and enter the main message loop.
    createQApp = QtGui.QApplication.startingUp()
    if createQApp:
        app = QtGui.QApplication([' '])
    else:
        app = QtGui.qApp

    # Create wizard dialog
    wiz = commonqt.Wizard()
    wiz.setWindowTitle('Result visualizer')
    wiz.resize(800, 600)

    seq = [PageOpen,PageVisualize,PageSave,PageFinal]

    # Get NetCDF file to open from command line or from FileOpen dialog.
    if len(sys.argv)>1:
        result = None
        try:
            result = loadResult(sys.argv[1])
        except Exception,e:
            QtGui.QMessageBox.critical(self, 'Unable to load result', str(e), QtGui.QMessageBox.Ok, QtGui.QMessageBox.NoButton)
        if result!=None:
            seq.pop(0)
            wiz.shared['result'] = result
            wiz.shared['scenario'] = result.scenario

    seq = commonqt.WizardSequence(seq)
    wiz.setSequence(seq)
    wiz.show()

    ret = app.exec_()
    page = None

    wiz.unlink()

    sys.exit(ret)

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
