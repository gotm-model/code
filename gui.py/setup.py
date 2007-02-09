from distutils.core import setup
import py2exe

from distutils.filelist import findall
import os, os.path
import matplotlib

def adddir(path):
    for f in findall(path):
        localname = os.path.join(path, f[len(path)+1:])
        if 'CVS' in localname: continue
        own_data_files.append((os.path.split(localname)[0],[f]))

own_data_files = []

own_data_files.append(('',['logo.png','outputtree.xml','figuretemplate.xml','settingsschema.xml']))

adddir(matplotlib.get_data_path())
adddir('defaultscenarios')
adddir('reporttemplates')
adddir('scenarioschemas')

setup(
    console=['gotm.py'],
    options={'py2exe': {
                'packages' : ['matplotlib', 'pytz'],
                'includes' : ['sip'],
                'excludes' : ['_gtkagg', '_tkagg', '_wxagg','tcl'],
                'dll_excludes': ['libgdk-win32-2.0-0.dll', 'libgobject-2.0-0.dll', 'libgdk_pixbuf-2.0-0.dll','wxmsw26uh_vc.dll']
            }},
    data_files=own_data_files
)

