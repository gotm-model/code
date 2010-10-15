import sys,os

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
import os, os.path,glob

def adddir(path,localtarget=None):
    if localtarget is None: localtarget = path
    for f in findall(path):
        localname = os.path.join(localtarget, f[len(path)+1:])
        if 'CVS' in localname: continue
        own_data_files.append((os.path.dirname(localname),[f]))

def addtreewithwildcard(sourceroot,path,localtarget):
    cwd = os.getcwd()
    os.chdir(sourceroot)
    for f in glob.glob(path):
        if os.path.isfile(f):
            own_data_files.append((os.path.join(localtarget,os.path.dirname(f)),[os.path.join(sourceroot,f)]))
    os.chdir(cwd)

own_data_files = []

# Let MatPlotLib add its own data files.
import matplotlib
own_data_files += matplotlib.get_py2exe_datafiles()

# Let our xmlplot module add its own data files.
import xmlplot.common
own_data_files += xmlplot.common.get_py2exe_datafiles()

# Add GOTM-GUI data files.
adddir('reporttemplates')
adddir('schemas')
adddir('icons')
own_data_files.append(('',['logo.png']))

addtreewithwildcard('../src','extras/bio/*/metadata/*','gotmsrc')

#own_data_files.append(('',['C:\\Windows\\System32\\MSVCP71.dll']))
#own_data_files.append(('',[os.path.join(os.environ['VS80COMNTOOLS'],'..\\..\\VC\\redist\\x86\\Microsoft.VC80.CRT\\MSVCR80.dll')]))
#own_data_files.append(('',[os.path.join(os.environ['VS80COMNTOOLS'],'..\\..\\VC\\redist\\x86\\Microsoft.VC80.CRT\\Microsoft.VC80.CRT.manifest')]))

setup(
    windows=[{'script':'gotmgui.py','icon_resources':[(1,'gotmgui.ico')]}],
    console=[{'script':'batch.py'}],
    options={'py2exe': {
                'packages' : ['matplotlib', 'pytz'],
#                'includes' : ['sip','PyQt4._qt'],
                'includes' : ['sip','netCDF4_utils','netcdftime'],
                'excludes' : ['_gtkagg', '_tkagg', '_wxagg','Tkconstants','Tkinter','tcl','wx','pynetcdf','cProfile','pstats','modeltest','mpl_toolkits.basemap','xmlplot.georef'],
                'dll_excludes': ['libgdk-win32-2.0-0.dll', 'libgobject-2.0-0.dll', 'libgdk_pixbuf-2.0-0.dll','wxmsw26uh_vc.dll','tcl84.dll','tk84.dll','powrprof.dll'],
            }},
    data_files=own_data_files
)

