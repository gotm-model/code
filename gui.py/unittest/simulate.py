#!/usr/bin/python

import sys,tempfile,os,hashlib,cPickle
sys.path.append(os.path.join(os.path.dirname(__file__),'..'))
import xmlstore.datatypes,core.scenario,core.simulator

def run(scenariopath):
    # Load scenario.
    scen = core.scenario.Scenario.fromSchemaName(core.simulator.gotmscenarioversion)
    if scenariopath.endswith('.xml'):
        scen.load(scenariopath)
    else:
        scen.loadAll(scenariopath)
    
    # Simulate
    res = core.simulator.simulate(scen,progresscallback=None,redirect=True)
    if res.returncode==1:
        print 'Simulation failed. Error: %s.\n\nGOTM output:\n%s' % (res.errormessage,res.stderr)
        res.unlink()
        sys.exit(1)
        
    # Get text output
    curerr = unicode(res.stderr)
    curout = unicode(res.stdout)
        
    # Get MD5 hash of NetCDF file
    f = open(res.datafile,'rb')
    m = hashlib.md5()
    cursize = 0
    while 1:
        dat = f.read(m.block_size)
        if not dat: break
        cursize += len(dat)
        m.update(dat)
    f.close()
    curhash = m.hexdigest()

    # Clean up result            
    res.unlink()
    scen.unlink()

    return curerr,curout,curhash,cursize
    
results = {}
itest = 0
def test(scenariopath):
    global results,itest
    itest += 1
    print 'Test %i with "%s"...' % (itest,scenariopath),

    curerr,curout,curhash,cursize = run(scenariopath)
    if scenariopath not in results:
        print 'first run'
        results[scenariopath] = (curerr,curout,curhash,cursize)
    else:
        olderr,oldout,oldhash,oldsize = results[scenariopath]
    
        def writelogs():
            log1 = open('log1.txt','w')
            log1.write(olderr)
            log1.write(oldout)
            log1.close()
            log2 = open('log2.txt','w')
            log2.write(curerr)
            log2.write(curout)
            log2.close()
    
        match = True
        if curerr!=olderr:
            if match: print 'FAILED'
            print 'ERROR standard error did not match original run.'
            match = False

        if curout!=oldout:
            if match: print 'FAILED'
            print 'ERROR standard out did not match original run.'
            match = False

        if curhash!=oldhash:        
            if match: print 'FAILED'
            print 'ERROR NetCDF did not match original run (old size = %i, new size: %i).' % (oldsize,cursize)
            match = False
            
        if not match:
            writelogs()
            print '%s result differs.' % (os.path.basename(scenariopath),)
            print 'Output written to log1.txt (original) and log2.txt (new).'
            return False
        else:
            print 'success'
    return True

def linearrun(scenariopaths,nsim=1):
    for scenariopath in scenariopaths:
        for i in range(nsim):
            valid = test(scenariopath)
            if not valid: return False
    return True

def stresstest(scenariopaths):
    import random
    while 1:
        valid = test(random.choice(scenariopaths))
        if not valid: return False
    return True
                            
if __name__=='__main__':
    import optparse,glob

    parser = optparse.OptionParser()
    parser.add_option('-r','--repeat',type='int',help='number of times each individual scenario must be run.')
    parser.add_option('-l','--loop',  action='store_true',help='loop forever testing all provided scenarios.')
    parser.add_option('-s','--stress',action='store_true',help='stress test: continuously run all scenarios in random order. All other arguments except -c/--cache are ignored.')
    parser.add_option('-c','--cache', type='string',help='cache file for results.')
    parser.add_option('-p','--pristine', action='store_true',help='create pristine results by running each scenario once with a clean GOTM library. All other arguments except -c/--cache are ignored.')
    parser.set_defaults(stress=False,loop=False,repeat=1,cache=None,pristine=False)
    (options, args) = parser.parse_args()

    if not args:
        print '%s must be called with one or more paths to .gotmscenario files. These paths may contain wildcards.' % os.path.basename(__file__)
        sys.exit(2)
    
    paths = []
    for p in args:
        curpaths = glob.glob(p)
        if not curpaths:
            print '"%s" was not found.' % p
            sys.exit(2)
        paths += curpaths

    if options.pristine:
        import subprocess
        for path in paths:
            arg = [__file__,path]
            if options.cache:
                arg += ['--cache',options.cache]
            ret = subprocess.call(arg,shell=True)
            if ret!=0:
                print 'Error occured during pristine runs. Exiting...'
                sys.exit(1)
        sys.exit(0)
    
    if options.cache and os.path.isfile(options.cache):
        print 'Loading earlier results from cache file "%s".' % options.cache
        f = open(options.cache,'rb')
        results.update(cPickle.load(f))
        f.close()

    if options.stress:
        print 'Stress testing with %i scenarios.' % len(paths)
        valid = stresstest(paths)
    else:
        print 'Iteratively testing %i scenarios.' % len(paths)
        while 1:
            valid = linearrun(paths,options.repeat)
            if not (valid and options.loop): break
            print 'Looping...'
            
    if options.cache:
        print 'Writing results to cache file "%s".' % options.cache
        f = open(options.cache,'wb')
        cPickle.dump(results,f,cPickle.HIGHEST_PROTOCOL)
        f.close()

    if not valid:
        print 'Exiting...'
        sys.exit(1)
        
    sys.exit(0)
