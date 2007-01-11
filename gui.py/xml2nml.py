#!/usr/bin/python

import common, sys, os, os.path

def main():
    copydata = True
    comments = True

    # Read optional command line argument.
    if '-nd' in sys.argv:
        sys.argv.remove('-nd')
        copydata = False
    if '-nc' in sys.argv:
        sys.argv.remove('-nc')
        comments = False

    # Check if we have the required arguments.
    # Note: sys.argv[0] contains the path name of the script.
    if len(sys.argv)<4:
        print 'Invalid syntax. This script requires three arguments, in the following order:'
        print '- the source file (*.gotmscenario, *.xml) or directory containing the GOTM-GUI scenario.'
        print '- the platform to create namelists for (application-version, e.g., gotm-3.2.4).'
        print '- the target directory to which to export the namelist files.'
        print 'Add the switch -nd to prevent copying of data files (e.g. observations) to the target directory.'
        print 'Add the switch -nc to prevent variable descriptions from being included in the namelist files (as comments).'
        sys.exit(1)
    src = os.path.abspath(sys.argv[1])
    template = sys.argv[2]
    targetdir = os.path.abspath(sys.argv[3])

    # Check if the source path exists.
    if not os.path.exists(src):
        print 'Error! The source path "'+src+'" does not exist.'
        sys.exit(1)

    # Check if we have an XML template for the specified target scenario version.
    tmpls = common.Scenario.getTemplates()
    if template not in tmpls:
        print 'Error! No XML template available for specified output version "'+template+'".'
        sys.exit(1)

    # Check if the target directory already exists (currently only produces warning and continues).
    if os.path.isdir(targetdir):
        print 'Warning! The target directory "'+targetdir+'" exists; files in it may be overwritten.'

    # Create the scenario object for the specified version.
    scenario = common.Scenario(templatename=template)

    # Load the scenario form the source path.
    if os.path.isfile(src):
        if   src.endswith('.gotmscenario'):
            scenario.loadAll(src)
        elif src.endswith('.xml'):
            if copydata:
                print 'Error! An XML source was specified; this source contains only the scenario settings, not the data files. Therefore, the -d switch cannot be used.\n'
                if src=='scenario.xml':
                    print 'If the data files are located in the directory of the specified scenario.xml file, specify the directory rather than scenario.xml as source.'
                sys.exit(1)
            scenario.setStore(src)
    elif os.path.isdir(src):
        scenario.loadAll(src)
    else:
        print 'Error! The source path "'+src+'" is not a file or directory.'
        sys.exit(1)

    # Export to namelists.
    scenario.writeAsNamelists(targetdir,copydatafiles = copydata,addcomments = comments)

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
