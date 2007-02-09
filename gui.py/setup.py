from distutils.core import setup
import py2exe

from distutils.filelist import findall
import os, os.path
import matplotlib
matplotlibdatadir = matplotlib.get_data_path()
matplotlibdata = findall(matplotlibdatadir)
matplotlibdata_files = []
for f in matplotlibdata:
    dirname = os.path.join('matplotlibdata', f[len(matplotlibdatadir)+1:])
    matplotlibdata_files.append((os.path.split(dirname)[0], [f]))

own_data_files = []
own_data_files.append(('',['logo.png','outputtree.xml','figuretemplate.xml']))

def adddir(path):
    for f in findall(path):
        dirname = os.path.join(path, f[len(path)+1:])
        own_data_files.append((os.path.split(dirname)[0],[f]))

adddir('defaultscenarios')
adddir('reporttemplates')
adddir('scenarioschemas')

setup(
    console=['gotm.py'],
    options={'py2exe': {
                'packages' : ['matplotlib', 'pytz'],
                'includes' : ['sip'],
                'excludes' : ['_gtkagg', '_tkagg', '_wxagg', '_ns_nxutils'],
                'dll_excludes': ['libgdk-win32-2.0-0.dll', 'libgobject-2.0-0.dll', 'libgdk_pixbuf-2.0-0.dll','wxmsw26uh_vc.dll']
            }},
    data_files=matplotlibdata_files+own_data_files
)

