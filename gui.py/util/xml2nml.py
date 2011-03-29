#!/usr/bin/python

import sys, os, os.path

gotmguiroot = os.path.join(os.path.dirname(os.path.realpath(__file__)),'..')
sys.path.append(gotmguiroot)
import core.common,core.scenario

def main():
    copydata = not core.common.getSwitchArgument('-nd')
    comments = not core.common.getSwitchArgument('-nc')

    # Check if we have the required arguments.
    # Note: sys.argv[0] contains the path name of the script.
    if len(sys.argv)<4:
        print \
"""
=============================================================================
GOTM-GUI scenario export utility
=============================================================================
This utility allows you to convert existing GOTM-GUI scenarios to namelist-
based scenarios that can be read by command-line GOTM.
-----------------------------------------------------------------------------
This script requires at least the following three arguments:
- the source file (*.gotmscenario, *.xml) or directory containing the
  GOTM-GUI scenario.
- the platform to create namelists for (application-version, e.g.,
  gotm-3.2.4).
- the target directory to which to export the namelist files.

Add the switch -nd to prevent copying of data files (e.g. observations) to
the target directory. Note that data files cannot be copied if you specify
an XML file as source; then the -nd switch must be specified.

Add the switch -nc to prevent variable descriptions from being included in
the namelist files (as comments).

Example:

xml2nml.py ./seagrass.gotmscenario gotm-3.2.4 ./seagrass

Converts the GOTM-GUI scenario file (or directory containing scenario.xml and
the data files) "./seagrass.gotmscenario" to a directory "./seagrass" that
contains namelist and data files suitable for GOTM version 3.2.4.
=============================================================================
"""
        return 1
        
    # Get command line arguments
    src = os.path.abspath(sys.argv[1])
    schemaname = sys.argv[2]
    targetdir = os.path.abspath(sys.argv[3])

    # Check if the source path exists.
    if not os.path.exists(src):
        print 'Error! The source path "%s" does not exist.' % src
        return 1

    # Check if we have an XML schema for the specified target scenario version.
    schemas = core.scenario.Scenario.getSchemaInfo().getSchemas()
    if schemaname not in schemas:
        print 'Error! No XML schema available for specified output version "%s".' % schemaname
        return 1

    # Check if the target directory already exists (currently only produces warning and continues).
    if os.path.isdir(targetdir):
        print 'Warning! The target directory "%s" exists; files in it may be overwritten.' % targetdir

    # Create the scenario object for the specified version.
    scen = core.scenario.Scenario.fromSchemaName(schemaname)

    # Load the scenario from the source path.
    if os.path.isfile(src):
        if   src.endswith('.gotmscenario'):
            scen.loadAll(src)
        elif src.endswith('.xml'):
            if copydata:
                print 'Error! An XML source was specified; this source contains only the scenario settings, not the data files. Therefore, you must use the -nd switch.\n'
                if src=='scenario.xml':
                    print 'If the data files are located in the directory of the specified scenario.xml file, specify the directory rather than scenario.xml as source.'
                return 1
            scen.load(src)
    elif os.path.isdir(src):
        scen.loadAll(src)
    else:
        print 'Error! The source path "%s" does not point to a file or directory.' % src
        return 1

    # Export to namelists.
    scen.writeAsNamelists(targetdir,copydatafiles = copydata,addcomments = comments)

    # Clean-up (delete temporary directories etc.)
    scen.release()
    
    return 0

if (__name__=='__main__'):
    ret = main()
    sys.exit(ret)
