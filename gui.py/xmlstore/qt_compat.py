#!/usr/bin/env python

from __future__ import with_statement
from __future__ import division

_TRY_PYSIDE = True

try:
	if not _TRY_PYSIDE:
		raise ImportError()
	import PySide.QtCore as _QtCore
	QtCore = _QtCore
	USES_PYSIDE = True
except ImportError:
	import sip
	sip.setapi('QString', 2)
	sip.setapi('QVariant', 2)
	import PyQt4.QtCore as _QtCore
	QtCore = _QtCore
	USES_PYSIDE = False


def _pyside_import_module(moduleName):
	pyside = __import__('PySide', globals(), locals(), [moduleName], -1)
	return getattr(pyside, moduleName)


def _pyqt4_import_module(moduleName):
	pyside = __import__('PyQt4', globals(), locals(), [moduleName], -1)
	return getattr(pyside, moduleName)


if USES_PYSIDE:
	import PySide
	import_module = _pyside_import_module
	
	mpl_qt4_backend = 'PySide'
	qt4_backend = 'PySide'
	qt4_backend_version = PySide.__version__
else:
	import_module = _pyqt4_import_module

	QtCore.Signal = QtCore.pyqtSignal
	QtCore.Slot = QtCore.pyqtSlot
	QtCore.Property = QtCore.pyqtProperty

	mpl_qt4_backend = 'PyQt4'
	qt4_backend = 'PyQt4'
	qt4_backend_version = QtCore.PYQT_VERSION_STR

QtGui = import_module('QtGui')

if USES_PYSIDE:
	QtGui.QFileDialog.getOpenFileNamesAndFilter = QtGui.QFileDialog.getOpenFileNames
	QtGui.QFileDialog.getOpenFileNameAndFilter = QtGui.QFileDialog.getOpenFileName
	QtGui.QFileDialog.getSaveFileNameAndFilter = QtGui.QFileDialog.getSaveFileName

if __name__ == "__main__":
	pass

