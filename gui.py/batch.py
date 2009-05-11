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
import xmlstore
import core.common, core.scenario, core.result, core.simulator, core.report

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
cdf = core.common.getSwitchArgument('-cdf')
gotmoutput = core.common.getSwitchArgument('-gotmoutput')
resultpath = core.common.getNamedArgument('-writeresult')
reportpath = core.common.getNamedArgument('-writereport')
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
    container = xmlstore.datatypes.DataContainer.fromPath(path)
    
    try:
        if core.scenario.Scenario.canBeOpened(container):
            # Try to load scenario.
            scen = core.scenario.Scenario.fromSchemaName(core.scenario.guiscenarioversion)
            scen.loadAll(container)
            res = None
        elif core.result.Result.canBeOpened(container):
            # Try to load result.
            res = core.result.Result()
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
if res is None:
    if gotmoutput:
        progcallback = None
    else:
        progcallback = printprogress
    res = core.simulator.simulate(scen,progresscallback=progcallback,redirect=not gotmoutput)
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
    if resultpath is not None:
        resultpath = os.path.normpath(os.path.join(oldworkingdir, resultpath))
        if cdf:
            print 'Writing NetCDF result to "%s".' % resultpath
            res.saveNetCDF(resultpath)
        else:
            print 'Writing result to "%s".' % resultpath
            res.save(resultpath)

    # Generate report, if requested.
    if reportpath is not None:
        def reportprogress(progress,description):
            print '%5.1f %% done, %s' % (progress*100,description)

        reportpath = os.path.normpath(os.path.join(oldworkingdir, reportpath))
        reptemplates = core.report.Report.getTemplates()
        rep = core.report.Report()
        
        # Use report settings stored within the result (if any)
        rep.store.root.copyFrom(res.store['ReportSettings'],replace=True)

        # Add all possible output variables
        treestore = res.getVariableTree(os.path.join(core.common.getDataRoot(),'schemas/outputtree.xml'),plottableonly=True)
        selroot = rep.store['Figures/Selection']
        for node in treestore.root.getDescendants():
            if node.canHaveValue() and not node.isHidden():
                ch = selroot.addChild('VariablePath')
                ch.setValue('/'.join(node.location))
        treestore.unlink()
        
        print 'Creating report in "%s".' % reportpath
        rep.generate(res,reportpath,reptemplates['default'],callback=reportprogress)
        rep.release()

# Clean-up        
if scen is not None: scen.release()
if res is not None: res.release()

# Reset previous working directory
os.chdir(os.path.dirname(oldworkingdir))

sys.exit(res.returncode)
