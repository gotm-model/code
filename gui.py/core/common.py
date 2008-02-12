#$Id: common.py,v 1.2 2008-02-12 11:42:43 jorn Exp $

import sys, os.path, tempfile, shutil, atexit

dataroot = None
def setDataRoot(path):
    global dataroot
    dataroot = path
def getDataRoot():
    global dataroot
    if dataroot==None:
        dataroot = os.path.realpath(os.path.join(os.path.dirname(__file__),'..'))
    return dataroot

class TempDirManager:
    tempdirs = None

    @staticmethod
    def create(prefix=''):
        path = tempfile.mkdtemp('',prefix)
        if TempDirManager.tempdirs==None:
            TempDirManager.tempdirs = []
            atexit.register(TempDirManager.cleanup)
        TempDirManager.tempdirs.append(path)
        return path
        
    @staticmethod
    def empty(path):
        for f in os.listdir(path): 
            os.remove(os.path.join(path,f))

    @staticmethod
    def delete(path,unregister=True):
        assert path in TempDirManager.tempdirs, 'Attempt to delete temporary directory "%s" that is not in list of registered tempdirs.' % path
        print 'Deleting temporary directory "%s".' % path
        shutil.rmtree(path)
        if unregister: TempDirManager.tempdirs.remove(path)
    
    @staticmethod
    def cleanup():
        for path in TempDirManager.tempdirs:
            TempDirManager.deleteTempDir(path,unregister=False)

# ------------------------------------------------------------------------------------------
# Command line argument utility functions
# ------------------------------------------------------------------------------------------

# getNamedArgument: Get the value of a named command line argument, and removes both name
#   and value from the global list of command line arguments. Returns None if the command
#   line argument was not specified. If the script was called with 'script.py -d hello',
#   getNamedArgument('-d') will return 'hello'.
def getNamedArgument(name,type=None,default=None):
    try:
        iarg = sys.argv.index(name)
    except ValueError:
        return default
    if iarg==len(sys.argv)-1: return default
    val = sys.argv[iarg+1]
    del sys.argv[iarg+1]
    del sys.argv[iarg]
    if type!=None: val = type(val)
    return val

def getSwitchArgument(name):
    if name not in sys.argv: return False
    sys.argv.remove(name)
    return True
    
def getNextArgument(type=None):
    val = None
    if len(sys.argv)>1:
        val = sys.argv.pop(1)
        if type!=None: val = type(val)
    return val
