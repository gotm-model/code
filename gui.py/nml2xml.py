#!/usr/bin/python

import common, sys, os, os.path

def main():
    targetversion = common.savedscenarioversion
    targetisdir = False

    # Get optional arguments
    protodir = common.getNamedArgument('-p')
    forcedversion = common.getNamedArgument('-v')
    if forcedversion!=None: targetversion = forcedversion
    if '-d' in sys.argv:
        sys.argv.remove('-d')
        targetisdir = True

    # Check if we have the required arguments.
    # Note: sys.argv[0] contains the path name of the script.
    if len(sys.argv)<3:
        print 'Invalid syntax. This script requires at least the followign two arguments:'
        print '- the source file (*.tar.gz) or directory containing the namelist files.'
        print '- the target path to which to save the GOTM-GUI scenario.'
        print 'If namelist data are saved as a .values file, specify the directory with the prototype namelist files (*.proto) as follows: -p "protodir".'
        print 'To force a particular version for the created scenario, add -v platform-version (e.g., -v gotm-3.2.4); by default the scenario is created with the version used by GOTM-GUI to store scenarios.'
        print 'To save to a directory rather than directly to a ZIP archive, add the switch -d.'
        sys.exit(1)
    srcpath = os.path.abspath(sys.argv[1])
    targetpath = os.path.abspath(sys.argv[2])

    # Check if the source path exists.
    if not os.path.exists(srcpath):
        print 'Error! The source path "'+srcpath+'" does not exist.'
        sys.exit(1)

    # Check if we have an XML template for the specified target scenario version.
    tmpls = common.Scenario.getTemplates()
    if targetversion not in tmpls:
        print 'Error! No XML template available for specified output version "'+targetversion+'".'
        sys.exit(1)

    # Check if the target path already exists (currently only produces warning and continues).
    if os.path.exists(targetpath):
        print 'Warning! The target path "'+targetpath+'" exists; it may be overwritten.'

    # Try to parse the namelist files (implicitly converts to the specified target version).
    scenario = common.Scenario.fromNamelists(srcpath,protodir=protodir,targetversion=targetversion)

    # Export to scenario.
    scenario.saveAll(targetpath,targetversion=targetversion,targetisdir=targetisdir)

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
