#!/usr/bin/python

import sys, os, os.path

try: 
    path = sys.path[:] 
    sys.path.append(os.path.join(os.path.dirname(os.path.realpath(__file__)),'..'))
    import common
finally: 
    sys.path = path 

def main():
    copydata = True
    comments = True

    # Read optional command line arguments.
    if '-nd' in sys.argv:
        sys.argv.remove('-nd')
        copydata = False
    if '-nc' in sys.argv:
        sys.argv.remove('-nc')
        comments = False

    # Check if we have the required arguments.
    # Note: sys.argv[0] contains the path name of the script.
    if len(sys.argv)<4:
        print '\nInvalid syntax. This script requires at least the following three arguments:\n'
        print '- the source file (*.gotmscenario, *.xml) or directory containing the GOTM-GUI scenario.'
        print '- the platform to create namelists for (application-version, e.g., gotm-3.2.4).'
        print '- the target directory to which to export the namelist files.\n'
        print 'Add the switch -nd to prevent copying of data files (e.g. observations) to the target directory. Note that data files cannot be copied if you specify an XML file as source; then the -nd switch must be specified.\n'
        print 'Add the switch -nc to prevent variable descriptions from being included in the namelist files (as comments).'
        print '\nExample:\n'
        print 'xml2nml.py ./seagrass.gotmscenario gotm-3.2.4 ./seagrass\n'
        print 'Converts the GOTM-GUI scenario file (or directory containing scenario.xml and the data files) "./seagrass.gotmscenario" to a directory "./seagrass" that contains namelist and data files suitable for GOTM version 3.2.4.'
        sys.exit(1)
    src = os.path.abspath(sys.argv[1])
    template = sys.argv[2]
    targetdir = os.path.abspath(sys.argv[3])

    # Check if the source path exists.
    if not os.path.exists(src):
        print 'Error! The source path "%s" does not exist.' % src
        sys.exit(1)

    # Check if we have an XML template for the specified target scenario version.
    tmpls = common.Scenario.getTemplates()
    if template not in tmpls:
        print 'Error! No XML template available for specified output version "%s".' % template
        sys.exit(1)

    # Check if the target directory already exists (currently only produces warning and continues).
    if os.path.isdir(targetdir):
        print 'Warning! The target directory "%s" exists; files in it may be overwritten.' % targetdir

    # Create the scenario object for the specified version.
    scenario = common.Scenario(templatename=template)

    # Load the scenario from the source path.
    if os.path.isfile(src):
        if   src.endswith('.gotmscenario'):
            scenario.loadAll(src)
        elif src.endswith('.xml'):
            if copydata:
                print 'Error! An XML source was specified; this source contains only the scenario settings, not the data files. Therefore, you must use the -nd switch.\n'
                if src=='scenario.xml':
                    print 'If the data files are located in the directory of the specified scenario.xml file, specify the directory rather than scenario.xml as source.'
                sys.exit(1)
            scenario.load(src)
    elif os.path.isdir(src):
        scenario.loadAll(src)
    else:
        print 'Error! The source path "%s" does not point to a file or directory.' % src
        sys.exit(1)

    # Export to namelists.
    scenario.writeAsNamelists(targetdir,copydatafiles = copydata,addcomments = comments)

    # Clean-up (delete temporary directories etc.)
    scenario.unlink()

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
