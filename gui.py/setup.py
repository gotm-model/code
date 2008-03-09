import sys

# Windows finds the CRT in the side-by-side assembly store (SxS), but Python does not look there.
# Therefore we add a location of the CRT to the Python path.
sys.path.append('C:\\Program Files\\Microsoft Visual Studio 8\\VC\\redist\\x86\\Microsoft.VC80.CRT');

try:
    import modulefinder
    for extra in ['win32com','win32com.shell']: #,"win32com.mapi"
        __import__(extra)
        m = sys.modules[extra]
        for p in m.__path__[1:]:
            modulefinder.AddPackagePath(extra, p)
except ImportError:
    # no build path setup, no worries.
    pass

# Here we import Scientific. This will automatically add the path with Scientific
# extension modules (Scientific_netcdf) to the Python path.
import Scientific

from distutils.core import setup
import py2exe

from distutils.filelist import findall
import os, os.path
import matplotlib

def adddir(path,localtarget=None):
    if localtarget==None: localtarget = path
    for f in findall(path):
        localname = os.path.join(localtarget, f[len(path)+1:])
        if 'CVS' in localname: continue
        own_data_files.append((os.path.dirname(localname),[f]))

own_data_files = matplotlib.get_py2exe_datafiles()

own_data_files.append(('',['logo.png']))
#own_data_files.append(('',['C:\Program Files\Python24\MSVCP71.dll']))

adddir('defaultscenarios')
adddir('reporttemplates')
adddir('schemas')
adddir('xmlplot/schemas')

setup(
    windows=[{'script':'gotm.py','icon_resources':[(1,'gotmgui.ico')]}],
    console=[{'script':'batch.py'}],
    options={'py2exe': {
                'packages' : ['matplotlib', 'pytz'],
                'includes' : ['sip','PyQt4._qt'],
                'excludes' : ['_gtkagg', '_tkagg', '_wxagg','Tkconstants','Tkinter','tcl','wx','pynetcdf'],
                'dll_excludes': ['libgdk-win32-2.0-0.dll', 'libgobject-2.0-0.dll', 'libgdk_pixbuf-2.0-0.dll','wxmsw26uh_vc.dll','tcl84.dll','tk84.dll'],
            }},
    data_files=own_data_files
)

