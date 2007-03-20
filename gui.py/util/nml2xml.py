#!/usr/bin/python

import sys, os, os.path

gotmguiroot = os.path.join(os.path.dirname(os.path.realpath(__file__)),'..')

path = sys.path[:] 
sys.path.append(gotmguiroot)
try: 
    import scenario, common
finally: 
    sys.path = path

scenario.Scenario.setRoot(gotmguiroot)

def main():
    # Default values for optional arguments
    targetschema = scenario.savedscenarioversion
    targetisdir = False
    protodir = None
    strict = True

    # Get optional arguments
    protodir = common.getNamedArgument('-p')
    forcedversion = common.getNamedArgument('-v')
    if forcedversion!=None: targetschema = forcedversion
    if '-d' in sys.argv:
        sys.argv.remove('-d')
        targetisdir = True
    if '-ns' in sys.argv:
        sys.argv.remove('-ns')
        strict = False

    # Check if we have the required arguments.
    # Note: sys.argv[0] contains the path name of the script.
    if len(sys.argv)<3:
        print '\nInvalid syntax. This script requires at least the following two arguments:\n'
        print '- the source file (*.tar.gz) or directory containing the namelist files.'
        print '- the target path to which to save the GOTM-GUI scenario.\n'
        print 'If namelist data are present in a .values file, you must also specify the directory with the prototype namelist files (*.proto) as follows: -p protodir.\n'
        print 'To force a particular version for the created scenario, add -v platform-version (e.g., -v gotm-3.2.4); by default the scenario is created with the version that GOTM-GUI uses to store scenarios (currently %s).\n' % scenario.savedscenarioversion
        print 'To save to a directory rather than directly to a ZIP archive (which would contain all files in the directory), add the switch -d.\n'
        print 'if you are saving to ZIP archive rather than to a directory, use the file extension ".gotmscenario" to ensure the produced file is automatically recognized by GOTM-GUI.'
        print 'By default, namelists are parsed in "strict" mode: all variables must be present once, and in the right order. Add the switch -ns to enable Fortran-like loose parsing.\n'
        print '\nExamples:\n'
        print 'nml2xml.py ./seagrass ./seagrass.gotmscenario\n'
        print 'Converts the namelists (plus data files) in the directory "./seagrass" to the scenario file "./seagrass.gotmscenario" suitable for GOTM-GUI.'
        print ''
        print 'nml2xml.py ./v3.2/seagrass ./seagrass.gotmscenario -p ./v3.2/templates\n'
        print 'Converts the namelist .values file (plus data files) in the directory "./v3.2/seagrass" to the scenario file "./seagrass.gotmscenario" suitable for GOTM-GUI, while using .proto files in directory "./v3.2/templates".'
        sys.exit(1)
    srcpath = os.path.abspath(sys.argv[1])
    targetpath = os.path.abspath(sys.argv[2])

    # Check if the source path exists.
    if not os.path.exists(srcpath):
        print 'Error! The source path "%s" does not exist.' % srcpath
        sys.exit(1)

    # Check if we have an XML schema for the specified target scenario version.
    schemas = scenario.Scenario.schemaname2path()
    if targetschema not in schemas:
        print 'Error! No XML schema available for specified output version "%s".' % targetschema
        sys.exit(1)

    # Check if the target path already exists (currently only produces warning and continues).
    if os.path.exists(targetpath):
        print 'Warning! The target path "%s" exists; it may be overwritten.' % targetpath

    # Warn for alternative file extension.
    if (not targetisdir) and (not targetpath.endswith('.gotmscenario')):
        print 'Warning! The output file does not have extension .gotmscenario, and will therefore not be recognized automatically by the GUI.'

    # Try to parse the namelist files (implicitly converts to the specified target version).
    try:
        scen = scenario.Scenario.fromNamelists(srcpath,protodir=protodir,targetversion=targetschema,strict=strict)
    except Exception,e:
        print '\n\nFailed to load scenario form namelists. Reason:\n'+str(e)
        print '\nYou might try adding the switch -ns. This switch disables strict namelist parsing.'
        return 1

    # Export to scenario.
    scen.saveAll(targetpath,targetversion=targetschema,targetisdir=targetisdir)

    # Clean-up (delete temporary directories etc.)
    scen.unlink()

# If the script has been run (as opposed to imported), enter the main loop.
if (__name__=='__main__'): main()
