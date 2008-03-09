#$Id: common.py,v 1.4 2008-03-09 11:30:38 jorn Exp $

# Import modules from standard Python library
import sys,os.path

# Import additional third party modules
import matplotlib.dates,matplotlib.numerix

import xmlstore.xmlstore

# ------------------------------------------------------------------------------------------
# Functions for getting/settings the path to data files
# ------------------------------------------------------------------------------------------
        
dataroot = None
def setDataRoot(path):
    global dataroot
    dataroot = path
def getDataRoot():
    global dataroot
    if dataroot==None:
        if hasattr(sys,'frozen'):
            dataroot = os.path.join(os.path.dirname(unicode(sys.executable, sys.getfilesystemencoding())),'xmlplot')
        else:
            dataroot = os.path.realpath(os.path.dirname(__file__))
    return dataroot
xmlstore.xmlstore.Schema.knownpaths['xmlplot'] = getDataRoot()

# ------------------------------------------------------------------------------------------
# Date-time functions
# ------------------------------------------------------------------------------------------

date2num = matplotlib.dates.date2num
num2date = matplotlib.dates.num2date
    
# ------------------------------------------------------------------------------------------
# Unit string convertor
# ------------------------------------------------------------------------------------------

def convertUnitToUnicode(unit):
    """Uses unicode to replace some common ASCII representations of
    subscript/superscript.
    """
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
# Numerical helper utilities
# ------------------------------------------------------------------------------------------

def findIndices(bounds,data):
    """Look for boundary indices of array based on desired value range.
    """
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

def interp1(x,y,X):
    """1D linear inter- and extrapolation. Operates on first axis.
    """
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

