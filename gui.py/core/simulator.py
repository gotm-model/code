import tempfile,os,time

import common,result,gotm

gotmversion = gotm.gui_util.getversion().rstrip()
gotmscenarioversion = 'gotm-%s' % gotmversion

verbose = False

def simulate(scen,continuecallback=None,progresscallback=None,redirect=True):
    if verbose: print 'enter simulate'
    
    namelistscenario = scen.convert(gotmscenarioversion)
    if verbose: print 'scenario converted'
    simulationdir = common.TempDirManager.create('gotm-')
    namelistscenario['gotmrun/output/out_fmt'].setValue(2)
    namelistscenario['gotmrun/output/out_dir'].setValue('.')
    namelistscenario['gotmrun/output/out_fn' ].setValue('result')
    namelistscenario.writeAsNamelists(simulationdir)
    namelistscenario.release()
    
    if verbose: print 'create result'

    # Create result object.
    res = result.Result()
        
    # Save old working directory
    olddir = os.getcwdu()

    if verbose: print 'switch working directory'

    # Change to directory with GOTM scenario (catch exceptions that can occur,
    # for instance, if the specified directory does not exist).
    try:
        os.chdir(simulationdir)
    except Exception,e:
        res.errormessage = 'Failed to enter temporary simulation directory "%s". %s' % (simulationdir,e)
        res.returncode = 1
        os.chdir(olddir)
        return res

    # Redirect FORTRAN output to (temporary) files.
    if redirect:
        (h,outfile) = tempfile.mkstemp('.txt','gotm')
        os.close(h)
        (h,errfile) = tempfile.mkstemp('.txt','gotm')
        os.close(h)
        gotm.gui_util.redirectoutput(outfile,errfile)

    if verbose: print 'initializing gotm module'

    # Initialize GOTM
    try:
        gotm.gotm.init_gotm()
    except Exception,e:
        res.errormessage = 'Exception thrown while initializing GOTM: %s' % e
        res.returncode = 1
        
    # Only enter the time loop if we succeeded so far.
    if res.returncode==0:
        # Calculate the size of time batches (small enough to respond rapidly to requests
        # for cancellation, and to show sufficiently detailed progress - e.g. in % -
        # but not so small that GUI slows down due to the avalanche of progress notifications)
        visualres = 0.01

        # Get # of first step, last step, number of steps for whole GOTM run.
        start = gotm.time.minn*1    # Multiply by 1 to ensure we have the integer value, not a reference to the attribute
        stop  = gotm.time.maxn*1    # Multiply by 1 to ensure we have the integer value, not a reference to the attribute
        stepcount = stop-start+1

        minslicesize = 2
        maxslicesize = int(round(stepcount/20.))    # Maximum slice: 5 % of complete simulation
        if maxslicesize<minslicesize: maxslicesize = minslicesize
        
        islicestart = start
        islicesize = 100
        
        # if no progress notifications are desired and the simulation cannot be cancelled,
        # simply run the complete simulation at once (slice size = entire simulation)
        if progresscallback==None and continuecallback==None: islicesize=stepcount
        
        time_runstart = time.clock()
        while islicestart<=stop:
            time_slicestart = time.clock()

            # Check if we have to cancel
            if continuecallback!=None and not continuecallback():
                print 'GOTM run was cancelled; exiting thread...'
                res.returncode = 2
                break
            
            # Configure GOTM for new slice.
            gotm.time.minn = islicestart
            islicestop = islicestart + islicesize - 1
            if islicestop>stop: islicestop = stop
            gotm.time.maxn = islicestop
            
            # Process time batch
            try:
                gotm.gotm.time_loop()
            except Exception,e:
                res.errormessage = 'Exception thrown in GOTM time loop: %s' % e
                res.returncode = 1
                break
                
            time_slicestop = time.clock()

            if progresscallback!=None:
                # Send 'progress' event
                prog = (islicestop-start+1)/float(stepcount)
                remaining = (1-prog)*(time_slicestop-time_runstart)/prog
                progresscallback(prog,remaining)

            # Adjust slice size, aiming for slices that take 0.4 seconds to simulate.
            elapsed = time_slicestop-time_slicestart
            if elapsed==0:
                islicesize = maxslicesize
            else:
                islicesize = int(round(islicesize * 0.4/elapsed))
                if islicesize<minslicesize: islicesize = minslicesize
                if islicesize>maxslicesize: islicesize = maxslicesize

            islicestart = islicestop + 1
            
    # GOTM clean-up
    try:
        gotm.gotm.clean_up()
    except Exception,e:
        res.errormessage = 'Exception thrown during GOTM clean-up: %s' % e
        if res.returncode==0: res.returncode = 1
        
    if redirect:
        # Reset FORTRAN output
        gotm.gui_util.resetoutput()

        # Read GOTM output from temporary files, then delete these files.
        f = open(errfile,'r')
        res.stderr = f.read()
        f.close()
        os.remove(errfile)
        f = open(outfile,'r')
        res.stdout = f.read()
        f.close()
        os.remove(outfile)

    # Return to previous working directory.
    os.chdir(olddir)

    if res.returncode==0:    
        # Succeeded: get the result. Note: the result "inherits" the temporary directory,
        # so we so not have to delete it here.
        respath = os.path.join(simulationdir,'result.nc')
        res.tempdir = simulationdir
        res.attach(respath,scen,copy=False)
        res.changed = True
    else:
        # Failed: delete temporary simulation directory
        try:
            common.TempDirManager.delete(simulationdir)
        except Exception,e:
            print 'Unable to completely remove GOTM temporary directory "%s".\nError: %s' % (simulationdir,e)
            
    return res
