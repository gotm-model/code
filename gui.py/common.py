#$Id: common.py,v 1.34 2007-10-15 06:45:09 jorn Exp $

import datetime,time,sys,xml.dom.minidom
import matplotlib.dates,matplotlib.numerix,pytz

class referencedobject:
    def __init__(self):
        self.refcount=1
    
    def release(self):
        assert self.refcount>0
        self.refcount -= 1
        if self.refcount==0: self.unlink()

    def addref(self):
        assert self.refcount>0
        self.refcount += 1
        return self
        
    def unlink(self):
        pass

# ------------------------------------------------------------------------------------------
# Date-time parsing variables and functions
# ------------------------------------------------------------------------------------------

# datetime_displayformat: date format used to display datetime objects in the GUI.
datetime_displayformat = '%Y-%m-%d %H:%M:%S'
utc=pytz.timezone('UTC')

def dateTimeFromTuple(tup):
    return datetime.datetime(tup[0],tup[1],tup[2],tup[3],tup[4],tup[5],tzinfo=utc)

def parseDateTime(str,fmt):
    """Convert string to Python datetime object, using specified format.
    Counterpart of datetime.strftime."""
    return dateTimeFromTuple(time.strptime(str,fmt))

date2num = matplotlib.dates.date2num
num2date = matplotlib.dates.num2date

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
    
# ------------------------------------------------------------------------------------------
# Unit string convertor
# ------------------------------------------------------------------------------------------

def convertUnitToUnicode(unit):
    if unit=='celsius': return unichr(176)+'C'
    sup2 = unichr(178)
    sup3 = unichr(179)
    unit = unit.replace('s2','s'+sup2)
    unit = unit.replace('s3','s'+sup3)
    unit = unit.replace('m2','m'+sup2)
    unit = unit.replace('m3','m'+sup3)
    unit = unit.replace('**3',sup3)
    return unit

# ------------------------------------------------------------------------------------------
# XML helper functions
# ------------------------------------------------------------------------------------------

# findDescendantNode: Return the first child XML DOM node with the specified location
#   (location = array of path components) below the specified XML DOM node (root).
#   If create = True, the node will be created if it does not exist yet.
def findDescendantNode(root,location,create=False):
    assert root!=None,'findDescendantNode called on non-existent parent node (parent = None).'
    node = root
    for childname in location:
        if childname=='': continue
        foundchild = None
        for ch in node.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE and ch.localName==childname:
                foundchild = ch
                break
        else:
            if create:
                doc = root
                while doc.parentNode!=None: doc=doc.parentNode
                assert doc.nodeType==doc.DOCUMENT_NODE, 'Could not find DOM document node needed to create %s. Node "%s" does not have a parent.' % (location,doc.tagName)
                foundchild = doc.createElementNS(node.namespaceURI,childname)
                node.appendChild(foundchild)
            else:
                return None
        node = foundchild
    return node

# findDescendantNodes: Return a list of all child XML DOM nodes with the specified location
#   (location = array of path components) below the specified XML DOM node (root).
def findDescendantNodes(root,location):
    parentloc = location[:]
    name = parentloc.pop()
    parent = findDescendantNode(root,parentloc,create=False)
    children = []
    if parent!=None:
        for ch in parent.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE and ch.localName==name:
                children.append(ch)
    return children

def addDescendantNode(parent,location):
    doc = parent
    while doc.parentNode!=None: doc=doc.parentNode
    for name in location:
        node = doc.createElementNS(parent.namespaceURI,name)
        parent.appendChild(node)
        parent = node
    return parent

def getNodeText(node):
    return ''.join([ch.data for ch in node.childNodes if ch.nodeType==ch.TEXT_NODE]).strip()

def setNodeText(node,text,xmldocument=None):
    if xmldocument==None:
        xmldocument = node
        while xmldocument.parentNode!=None: xmldocument=xmldocument.parentNode
    for ch in node.childNodes:
        if ch.nodeType == ch.TEXT_NODE:
            node.removeChild(ch)
            ch.unlink()
    val = xmldocument.createTextNode(text)
    node.insertBefore(val,node.firstChild)
    
def removeNodeChildren(node):
    for ch in node.childNodes:
        node.removeChild(ch)
        ch.unlink()

def copyNode(sourcenode,newparent,targetdoc=None,name=None,before=None):
    # Create new document or find existing one if not provided.
    if newparent==None:
        if targetdoc==None:
            impl = xml.dom.minidom.getDOMImplementation()
            targetdoc = impl.createDocument(None, None, None)
        newparent = targetdoc
    elif targetdoc==None:
        targetdoc = newparent
        while targetdoc.parentNode!=None: targetdoc = targetdoc.parentNode

    # Create new node
    cpy = None
    if sourcenode.nodeType==sourcenode.ELEMENT_NODE:
        if name==None: name = sourcenode.localName
        cpy = targetdoc.createElementNS(newparent.namespaceURI,name)
        for key in sourcenode.attributes.keys():
            cpy.setAttribute(key,sourcenode.getAttribute(key))
    elif sourcenode.nodeType==sourcenode.TEXT_NODE:
        cpy = targetdoc.createTextNode(sourcenode.data)
    else:
        print 'WARNING: do not know how to copy node with type %s. Skipping...' % sourcenode.nodeType
        
    # Insert new node
    if cpy!=None:
        if before==None:
            cpy = newparent.appendChild(cpy)
        else:
            cpy = newparent.insertBefore(cpy,before)
        for ch in sourcenode.childNodes: copyNode(ch,cpy,targetdoc)
        
    # Return new node
    return cpy
    
def stripWhitespace(node):
    # Strip whitespace at start of element contents.
    ch = node.firstChild
    while ch!=None and ch.nodeType==ch.TEXT_NODE and len(ch.data.strip())==0:
        node.removeChild(ch)
        ch = node.firstChild
        
    # Strip whitespace at end of element contents.
    ch = node.lastChild
    while ch!=None and ch.nodeType==ch.TEXT_NODE and len(ch.data.strip())==0:
        node.removeChild(ch)
        ch = node.lastChild
    
    # Process element child nodes.
    for ch in node.childNodes:
        if ch.nodeType==ch.ELEMENT_NODE:
            stripWhitespace(ch)

# ------------------------------------------------------------------------------------------
# Numerical helper utilities
# ------------------------------------------------------------------------------------------

# Look for boundary indices of array based on desired value range.
def findIndices(bounds,data):
    # Zero-based indices!
    start = 0
    stop = len(data)-1
    if bounds!=None:
        if bounds[0]!=None:
            while start<len(data) and data[start]<bounds[0]: start+=1
        if bounds[1]!=None:
            while stop>=0         and data[stop] >bounds[1]: stop-=1

        # Greedy: we want take the interval that fully encompasses the specified range.
        if start>0 and start<len(data):  start-=1
        if stop<len(data)-1 and stop>=0: stop +=1

        # Note: a start beyond the available range, or a stop before it, will now have resulted
        # in start index > stop index, i.e., an invalid range. The calling function must be able
        # to handle this scenario.
        
    return (start,stop)

# 1D linear inter- and extrapolation. Operates on first axis.
def interp1(x,y,X):
    assert x.ndim==1, 'Original coordinates must be supplied as 1D array.'
    assert X.ndim==1, 'New coordinates must be supplied as 1D array.'
    
    # Transpose because it is easiest in numpy to operate on last axis. (this because of
    # upcasting rules)
    y = y.transpose()
    
    # Create array to hold interpolated values
    Y = matplotlib.numerix.empty(y.shape[0:-1]+(X.shape[0],),matplotlib.numerix.typecode(y))
    
    # Find indices of interpolated X in original x.
    iX = x.searchsorted(X)
    
    # Get the bounds of valid indices (index<=0 means point before x-data, index>=x.shape[0]
    # means point beyond x-data)
    bounds = iX.searchsorted((0.5,x.shape[0]-0.5))
    
    # Shortcuts to indices for left and right bounds, and the left bound values.
    iX_high = iX[bounds[0]:bounds[1]]
    iX_low = iX_high-1
    x_low  = x[iX_low]
    y_low = y[...,iX_low]
    
    # Linear interpolation
    Y[...,bounds[0]:bounds[1]] = y_low + ((X[bounds[0]:bounds[1]]-x_low)/(x[iX_high]-x_low))*(y[...,iX_high]-y_low)
    
    # Set points beyond bounds to extreme values.
    Y[...,0:bounds[0]] = y.take((0,),-1)
    Y[...,bounds[1]:] = y.take((-1,),-1)
    
    # Undo the original transpose and return
    return Y.transpose()
    
    # Below: previous code for Python-based linear interpolation. Slow!!
    
    #newshape = [X.shape[0]]
    #for i in y.shape[1:]: newshape.append(i)
    #Y = matplotlib.numerix.zeros(newshape,matplotlib.numerix.typecode(y))
    #icurx = 0
    #for i in range(X.shape[0]):
    #    while icurx<x.shape[0] and x[icurx]<X[i]: icurx+=1
    #    if icurx==0:
    #        Y[i,:] = y[0,:]
    #    elif icurx>=x.shape[0]:
    #        Y[i,:] = y[-1,:]
    #    else:
    #        rc = (y[icurx,:]-y[icurx-1,:])/(x[icurx]-x[icurx-1])
    #        Y[i,:] = y[icurx-1,:] + rc*(X[i]-x[icurx-1])
    #return Y

def getCenters(data,addends=False):
    delta = (data[1:]-data[:-1])/2
    newdata = data[:-1]+delta
    if addends: newdata = matplotlib.numerix.concatenate(([data[0]-delta[0]],newdata,[data[-1]+delta[-1]]),0)
    return newdata

def replicateCoordinates(coords,data,idim):
    assert coords.ndim==1, 'Coordinate array must be one-dimensional.'
    assert coords.shape[0]==data.shape[idim], 'Length of coordinate vector (%i) and specified dimension in data array (%i) must match.' % (coords.shape[0],data.shape[idim])
    newcoords = matplotlib.numerix.empty(data.shape,matplotlib.numerix.typecode(coords))
    tp = range(newcoords.ndim)
    tp.append(tp.pop(idim))
    newcoords = newcoords.transpose(tp)
    newcoords[:] = coords
    tp = range(newcoords.ndim)
    tp.insert(idim,tp.pop())
    return newcoords.transpose(tp)

def argtake(data,ind,axis):
    assert data.ndim==ind.ndim or data.ndim==ind.ndim+1, 'Number of dimensions for indices (%i) must be equal to, or one less than number of data dimensions (%i).' % (ind.ndim,data.ndim)
    reduce = (data.ndim==ind.ndim+1)
    allind = []
    for i in range(data.ndim):
        if i==axis:
            allind.append(ind)
        else:
            curind = matplotlib.numerix.empty(ind.shape,matplotlib.numerix.typecode(ind))
            idim = i
            if reduce and i>axis: idim-=1
            curind.swapaxes(idim,-1)[:] = range(data.shape[i])
            allind.append(curind)
    return data[tuple(allind)]

def getPercentile(data,cumweights,value,axis):
    assert value>=0 and value<=1., 'Percentile value must be between 0 and 1.'
    
    # First get indices where cumulative distribution >= critical value
    cumweights = cumweights.copy()
    matplotlib.numerix.putmask(cumweights,cumweights<value,2.)
    
    # Make sure that the cumulative distribution at the highest index still equals one.
    ind = [slice(0,cumweights.shape[i]) for i in range(cumweights.ndim)]
    ind[axis] = -1
    cumweights[tuple(ind)] = 1.
    
    highindices = cumweights.argmin(axis=axis)
    
    # If the high index if the lowest possible (0), increase it with one
    # because the lower index will be one lower.
    matplotlib.numerix.putmask(highindices,highindices==0,1)
    
    # Now get indices where cumulative distribution < critical value
    lowindices = highindices-1
    
    # Do linear interpolation between low and high indices
    highval = argtake(data,highindices,axis)
    lowval  = argtake(data,lowindices,axis)
    highcoords = argtake(cumweights,highindices,axis)
    lowcoords  = argtake(cumweights,lowindices, axis)
    highweight = (value-lowcoords)/(highcoords-lowcoords)
    return highval*highweight + lowval*(1.-highweight)
    