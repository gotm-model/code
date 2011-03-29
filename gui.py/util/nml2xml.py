#!/usr/bin/python

import sys, os, os.path

gotmguiroot = os.path.join(os.path.dirname(os.path.realpath(__file__)),'..')
sys.path.append(gotmguiroot)

import core.common, core.scenario

# Small function for receiving progress messages when parsing data files.
nextprogress = 0.
progresstep = .05
def printprogress(progress,status):
    global nextprogress
    if progress>=nextprogress:
        print '%i %% done.' % (progress*100,)
        nextprogress += progresstep

def main():
    # Get optional command line arguments
    targetisdir = core.common.getSwitchArgument('-d')
    strict = not core.common.getSwitchArgument('-ns')
    check = core.common.getSwitchArgument('-check')
    protodir = core.common.getNamedArgument('-p')
    targetschema = core.common.getNamedArgument('-v')

    # Default schema
    if targetschema is None: targetschema = core.scenario.savedscenarioversion

    # Check if we have the required arguments.
    # Note: sys.argv[0] contains the path name of the script.
    if len(sys.argv)<3:
        print \
"""
=============================================================================
GOTM-GUI scenario import utility
=============================================================================
This utility allows you to convert existing namelist-based scenarios for
command-line GOTM to the GOTM-GUI scenario format.
-----------------------------------------------------------------------------
This script requires at least the following two arguments:

- the source file (*.tar.gz) or directory containing the namelist files.
- the target path to which to save the GOTM-GUI scenario.

If namelist data are present in one or more .values files, you must also
specify the directory with the prototype namelist files (*.proto) as follows:
-p protodir.

To force a particular version for the created scenario, add the switch
-v platform-version (e.g., -v gotm-3.2.4); by default the scenario is created
with the version that GOTM-GUI uses to store scenarios (currently %s).

To save to a directory rather than directly to a ZIP archive (which would
contain all files in the directory), add the switch -d. If you are saving to
ZIP archive rather than to a directory, use the file extension
".gotmscenario" to ensure the produced file is automatically recognized by
GOTM-GUI.

By default, namelists are parsed in "strict" mode: all variables must be
present once, and in the right order. Add the switch -ns to enable
Fortran-like loose parsing.

To check the validity of the scenario and its data files, add the switch
-check. If the scenario is found to be invalid, the script returns 1
(normally 0).

Examples:

nml2xml.py ./seagrass ./seagrass.gotmscenario

Converts the namelists (plus data files) in the directory "./seagrass" to the
scenario file "./seagrass.gotmscenario" suitable for GOTM-GUI.

nml2xml.py ./v3.2/seagrass ./seagrass.gotmscenario -p ./v3.2/templates

Converts the namelist .values file (plus data files) in the directory
"./v3.2/seagrass" to the scenario file "./seagrass.gotmscenario" suitable for
GOTM-GUI, while using .proto files in directory "./v3.2/templates".
=============================================================================
""" % core.scenario.savedscenarioversion
        return 1
        
    # Get command line arguments
    srcpath = os.path.abspath(sys.argv[1])
    targetpath = os.path.abspath(sys.argv[2])

    # Check if the source path exists.
    if not os.path.exists(srcpath):
        print 'Error! The source path "%s" does not exist.' % srcpath
        return 1

    # Check if we have an XML schema for the specified target scenario version.
    schemas = core.scenario.Scenario.getSchemaInfo().getSchemas()
    if targetschema not in schemas:
        print 'Error! No XML schema available for specified output version "%s".' % targetschema
        return 1

    # Check if the target path already exists (currently only produces warning and continues).
    if os.path.exists(targetpath):
        print 'Warning! The target path "%s" exists; it may be overwritten.' % targetpath

    # Warn for alternative file extension.
    if (not targetisdir) and (not targetpath.endswith('.gotmscenario')):
        print 'Warning! The output file does not have extension .gotmscenario, and will therefore not be recognized automatically by the GUI.'

    # Try to parse the namelist files (implicitly converts to the specified target version).
    try:
        scen = core.scenario.Scenario.fromNamelists(srcpath,prototypepath=protodir,targetversion=targetschema,strict=strict,requireplatform='gotm')
    except Exception,e:
        print '\n\nFailed to load scenario from namelists. Reason:\n'+str(e)
        print '\nYou might try adding the switch -ns. This switch disables strict namelist parsing.'
        return 1
        
    if check:
        print '\n============ checking scenario validity ============'
        errors = scen.validate(callback=printprogress)
        if errors:
            for e in errors: print e
            print '============ validity check failed ============\n'
            return 1
        else:
            print '============ validity check succeeded ============\n'

    # Export to scenario.
    scen.saveAll(targetpath,targetversion=targetschema,targetisdir=targetisdir)

    # Clean-up (delete temporary directories etc.)
    scen.release()
    
    return 0

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'):
    ret = main()
    sys.exit(ret)
