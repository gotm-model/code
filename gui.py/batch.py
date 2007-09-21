#!/usr/bin/python

# Import standard Python modules
import os,sys

# Debug info
print 'Python version: %s' % unicode(sys.version_info)

# In order to find our custom data files, make sure that we are in the directory
# containing the executable.
oldworkingdir = os.getcwdu()
os.chdir(os.path.abspath(os.path.dirname(sys.argv[0])))

# Import MatPlotLib to configure key parameters
import matplotlib
matplotlib.use('Qt4Agg')
matplotlib.rcParams['numerix'] = 'numpy'

# Now import our custom modules
import common, xmlstore, scenario, data, simulate, report

if len(sys.argv)==1:
    print \
"""
=============================================================================
GOTM-GUI batch simulation command line utility
=============================================================================
This utility allows you to process the scenario and result files of GOTM-GUI
automatically, without ever showing the GUI. It may be used to simulate a
scenario and generate a result file and/or report, or to generate a report
from an existing result.
-----------------------------------------------------------------------------
Syntax (arguments between square brackets are optional):

batch <path> [-writeresult <resultfile> [-cdf]] [-writereport <reportdir>]
    [-gotmoutput]
-----------------------------------------------------------------------------
<path>
    Path to an existing GOTM-GUI scenario or result. This can be a
    .gotmscenario or .gotmresult file created with GOTM-GUI, or a directory
    that contains the extracted contents of one of these files (.gotmscenario
    and .gotmresult are actually ZIP archives). If a scenario is specified,
    it will first be simulated; if a result is specified the existing data
    will be used.

-writeresult <resultfile>
    Specifies that a result file must be written to the path <resultfile>

-cdf
    Specifies that the result must be written in NetCDF format, rather than
    the GOTM-GUI .gotmresult format. Only used if -writeresult <resultfile>
    is specified.

-writereport <reportdir>
    Specifies that a report must be written to the directory <reportdir>.
    If this directory does not exist, it will be created.

-gotmoutput
    Specifies that the original output of GOTM must be shown, rather than
    percentages and time remaining. Only used if a path to a scenario is
    specified as first argument.
=============================================================================
"""
    sys.exit(1)

# Parse command line arguments
cdf = common.getSwitchArgument('-cdf')
gotmoutput = common.getSwitchArgument('-gotmoutput')
resultpath = common.getNamedArgument('-writeresult')
reportpath = common.getNamedArgument('-writereport')
path = os.path.normpath(os.path.join(oldworkingdir, sys.argv[1]))
del sys.argv[1]

# Warn for remaining (i.e., unused) command line arguments.
if len(sys.argv)>1:
    print '\n'
    for arg in sys.argv[1:]:
        print 'WARNING: command line argument "%s" is unknown and will be ignored.' % arg
    print 'Run "batch" without arguments to see a list of accepted arguments.\n'

container = None

try:
    # Open specified path as data container.
    container = xmlstore.DataContainer.fromPath(path)
    
    try:
        if scenario.Scenario.canBeOpened(container):
            # Try to load scenario.
            scen = scenario.Scenario.fromSchemaName(scenario.guiscenarioversion)
            scen.loadAll(container)
            res = None
        elif data.Result.canBeOpened(container):
            # Try to load result.
            res = data.Result()
            res.load(container)
            scen = res.scenario.addref()
        else:
            raise Exception('"%s" does not contain a scenario or result.' % path)
    finally:
        container.release()
        
except Exception,e:
    print 'Cannot open "%s". Error: %s' % (path,e)
    sys.exit(1)

# Callback for simulation progress notifications.
def printprogress(progress,remaining):
    print '%5.1f %% done, %.0f seconds remaining...' % (progress*100,remaining)

# Simulate
if res==None:
    if gotmoutput:
        progcallback = None
    else:
        progcallback = printprogress
    res = simulate.simulate(scen,progresscallback=progcallback,redirect=not gotmoutput)
    if res.returncode==0:
        print 'Simulation completed successfully.'
    elif res.returncode==1:
        print 'Simulation failed. Error: %s.\n\nGOTM output:\n%s' % (res.errormessage,res.stderr)
    elif res.returncode==2:
        print 'Simulation was cancelled by user.'
    else:
        assert False, 'GOTM simulator returned unknown code %i.' % res.returncode

if res.returncode==0:
    # Write result to file, if requested.
    if resultpath!=None:
        resultpath = os.path.normpath(os.path.join(oldworkingdir, resultpath))
        if cdf:
            print 'Writing NetCDF result to "%s".' % resultpath
            res.saveNetCDF(resultpath)
        else:
            print 'Writing result to "%s".' % resultpath
            res.save(resultpath)

    # Generate report, if requested.
    if reportpath!=None:
        def reportprogress(progress,description):
            print '%5.1f %% done, %s' % (progress*100,description)

        reportpath = os.path.normpath(os.path.join(oldworkingdir, reportpath))
        reptemplates = report.Report.getTemplates()
        rep = report.Report()
        rep.store.root.copyFrom(res.store.root['ReportSettings'],replace=True)
        print 'Creating report in "%s".' % reportpath
        rep.generate(res,reportpath,reptemplates['default'],callback=reportprogress)
        rep.release()

# Clean-up        
if scen!=None: scen.release()
if res!=None: res.release()

# Reset previous working directory
os.chdir(os.path.dirname(oldworkingdir))

sys.exit(res.returncode)
