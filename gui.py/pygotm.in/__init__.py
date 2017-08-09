#!/usr/bin/env python

import sys,os,ctypes,re

# Determine potential names of GOTM dynamic library.
if os.name=='nt':
   dllpaths = ('python_gotm.dll','libpython_gotm.dll')
elif os.name == "posix" and sys.platform == "darwin":
   dllpaths = ('libpython_gotm.dylib',)
else:
   dllpaths = ('libpython_gotm.so',)

# Locate GOTM dynamic library.
searchdir = os.path.dirname(os.path.abspath(__file__))
for dllpath in dllpaths:
   dllpath = os.path.join(searchdir,dllpath)
   if os.path.isfile(dllpath): break
else:
   print 'Unable to locate GOTM dynamic library %s.' % (' or '.join(dllpaths),)
   sys.exit(1)

# Load FABM library.
gotm = ctypes.CDLL(dllpath)

gotm.initialize.argtypes = ()
gotm.initialize.restype = None
gotm.finalize.argtypes = ()
gotm.finalize.restype = None
gotm.run.argtypes = ()
gotm.run.restype = None

gotm.redirect_output.argtypes = (ctypes.c_char_p,ctypes.c_char_p)
gotm.redirect_output.restype = None
gotm.reset_output.argtypes = ()
gotm.reset_output.restype = None

gotm.get_version.argtypes = (ctypes.c_int,ctypes.c_char_p)
gotm.get_version.restype = None

gotm.get_time_bounds.argtypes = (ctypes.POINTER(ctypes.c_int),ctypes.POINTER(ctypes.c_int))
gotm.get_time_bounds.restype = None
gotm.set_time_bounds.argtypes = (ctypes.c_int,ctypes.c_int)
gotm.set_time_bounds.restype = None

initialize = gotm.initialize
finalize = gotm.finalize
run = gotm.run

redirect_output = gotm.redirect_output
reset_output = gotm.reset_output

set_time_bounds = gotm.set_time_bounds

def get_time_bounds():
   imin = ctypes.c_int()
   imax = ctypes.c_int()
   gotm.get_time_bounds(ctypes.byref(imin),ctypes.byref(imax))
   return imin.value,imax.value

def get_version():
   version_length = 256
   strversion = ctypes.create_string_buffer(version_length)
   gotm.get_version(version_length,strversion)
   return strversion.value
