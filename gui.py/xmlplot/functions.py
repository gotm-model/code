import common,expressions,datetime
import numpy

class statistics(expressions.LazyFunction):
    """Transformation that takes the average of the variable across one dimension.

    Initialization arguments:
    centermeasure:   0 for the mean, 1 for the median.
    boundsmeasure:   0 for mean-sd and mean+sd, 1 for percentiles.
    percentilewidth: distance between lower and upper percentile (fraction), e.g., 0.95 to get
                     2.5 % and 97.5 % percentiles.
    output:          0 for center+bounds, 1 for center only, 2 for lower bound, 3 for upper bound
    """
    def __init__(self,sourceslice,axis,centermeasure=0,boundsmeasure=0,percentilewidth=.5,output=0):
        expressions.LazyFunction.__init__(self,self.__class__.__name__,None,sourceslice,axis,centermeasure=centermeasure,boundsmeasure=boundsmeasure,percentilewidth=percentilewidth,output=output)
        self.setRemovedDimension(1,'axis')
    
    def _getValue(self,resolvedargs,resolvedkwargs,dataonly=False):
        sourceslice,axis = resolvedargs[0],resolvedargs[1]
    
        centermeasure = resolvedkwargs['centermeasure']
        boundsmeasure = resolvedkwargs['boundsmeasure']
        percentilewidth = resolvedkwargs['percentilewidth']
        output = resolvedkwargs['output']
    
        if isinstance(axis,basestring):
            dims = sourceslice.dimensions
            assert axis in dims,'Specified axis "%s" does not exist. Available: %s.' % (axis,', '.join(dims))
            axis = list(dims).index(axis)
            
        # Create new slice to hold source data.
        slc = sourceslice.removeDimension(axis,inplace=False)
        
        # Calculate weights from the mesh widths of the dimension to calculate statistics for.
        weights = sourceslice.coords_stag[axis]
        sourceslice.data = numpy.ma.asarray(sourceslice.data)
        if weights.ndim==1:
            weights = common.replicateCoordinates(numpy.diff(weights),sourceslice.data,axis)
        else:
            print weights.shape
            weights = numpy.diff(weights,axis=axis)
            print weights.shape
            print sourceslice.data.shape
        
        # Normalize weights so their sum over the dimension to analyze equals one
        summedweights = numpy.ma.array(weights,mask=sourceslice.data.mask,copy=False).sum(axis=axis)
        newshape = list(summedweights.shape)
        newshape.insert(axis,1)
        weights /= summedweights.reshape(newshape).repeat(sourceslice.data.shape[axis],axis)
        
        if (output<2 and centermeasure==0) or (output!=1 and boundsmeasure==0):
            # We need the mean and/or standard deviation. Calculate the mean,
            # which is needed for either measure.
            mean = (sourceslice.data*weights).sum(axis=axis)
        
        if (output<2 and centermeasure==1) or (output!=1 and boundsmeasure==1) or (output>1 and centermeasure==1 and boundsmeasure==0):
            # We will need percentiles. Sort the data along dimension to analyze,
            # and calculate cumulative (weigth-based) distribution.
            
            # Sort the data along the dimension to analyze, and sort weights
            # in the same order
            sortedindices = sourceslice.data.argsort(axis=axis,fill_value=numpy.Inf)
            sorteddata    = common.argtake(sourceslice.data,sortedindices,axis=axis)
            sortedweights = common.argtake(weights,sortedindices,axis)
            
            # Calculate cumulative distribution values along dimension to analyze.
            cumsortedweights = sortedweights.cumsum(axis=axis)
            
            # Calculate coordinates for interfaces between data points, to be used
            # as grid for cumulative distribution
            sorteddata = (numpy.ma.concatenate((sorteddata.take((0,),axis=axis),sorteddata),axis=axis) + numpy.ma.concatenate((sorteddata,sorteddata.take((-1,),axis=axis)),axis=axis))/2.
            cumsortedweights = numpy.concatenate((numpy.zeros(cumsortedweights.take((0,),axis=axis).shape,cumsortedweights.dtype),cumsortedweights),axis=axis)
        
        if output<2 or boundsmeasure==0:
            # We need the center measure
            if centermeasure==0:
                # Use mean for center
                center = mean
            elif centermeasure==1:
                # Use median for center
                center = common.getPercentile(sorteddata,cumsortedweights,.5,axis)
            else:
                assert False, 'Unknown choice %i for center measure.' % centermeasure

        if output!=1:
            # We need the lower and upper boundary
            if boundsmeasure==0:
                # Standard deviation will be used as bounds.
                meanshape = list(mean.shape)
                meanshape.insert(axis,1)
                fullmean = mean.reshape(meanshape)
                sd = numpy.sqrt(((sourceslice.data-mean.reshape(meanshape))**2*weights).sum(axis=axis))
                lbound = center-sd
                ubound = center+sd
            elif boundsmeasure==1:
                # Percentiles will be used as bounds.
                lowcrit = (1.-percentilewidth)/2.
                highcrit = 1.-lowcrit
                lbound = common.getPercentile(sorteddata,cumsortedweights, lowcrit,axis)
                ubound = common.getPercentile(sorteddata,cumsortedweights,highcrit,axis)
            else:
                assert False, 'Unknown choice %i for bounds measure.' % boundsmeasure

        if output<2:
            slc.data = center
            if output==0: slc.lbound,slc.ubound = lbound,ubound
        elif output==2:
            slc.data = lbound
        elif output==3:
            slc.data = ubound
        else:
            assert False, 'Unknown choice %i for output variable.' % boundsmeasure
            
        if dataonly: return slc.data
        return slc

class flatten(expressions.LazyFunction):
    def __init__(self,sourceslice,axis,targetaxis=None):
        expressions.LazyFunction.__init__(self,self.__class__.__name__,None,sourceslice,axis,targetaxis=targetaxis)
        self.setRemovedDimension(1,'axis')

    def _getValue(self,resolvedargs,resolvedkwargs,dataonly=False):
        sourceslice,axis = resolvedargs[0],resolvedargs[1]
        targetaxis = resolvedkwargs['targetaxis']
            
        dims = list(sourceslice.dimensions)
        if isinstance(axis,basestring):
            assert axis in dims,'Specified axis "%s" does not exist. Available: %s.' % (axis,', '.join(dims))
            axis = dims.index(axis)
        if isinstance(targetaxis,basestring):
            assert targetaxis in dims,'Specified axis "%s" does not exist. Available: %s.' % (targetaxis,', '.join(dims))
            targetaxis = dims.index(targetaxis)

        if targetaxis is None:
            targetaxis = 0
            if axis==0: targetaxis+=1
        assert axis!=targetaxis,'Source axis and target axis cannot be the same (%i).' % axis

        inewtargetaxis = targetaxis
        if axis<targetaxis: inewtargetaxis -= 1

        assert sourceslice.coords[axis      ].ndim==1,'Currently, the dimension to flatten cannot depend on other dimensions.'
        assert sourceslice.coords[targetaxis].ndim==1,'Currently, the dimension to absorb flattened values cannot depend on other dimensions.'
        
        # Get length of dimension to flatten, and of dimension to take flattened values.
        sourcecount = sourceslice.data.shape[axis]
        targetcount = sourceslice.data.shape[targetaxis]

        # Create new coordinates for dimension that absorbs flattened values.
        newtargetcoords = numpy.empty((targetcount*sourcecount,),sourceslice.coords[targetaxis].dtype)
        
        # Create a new value array.
        newdatashape = list(sourceslice.data.shape)
        newdatashape[targetaxis] *= sourcecount
        del newdatashape[axis]
        newdata = numpy.ma.array(numpy.empty(newdatashape,sourceslice.data.dtype),copy=False)
            
        for i in range(0,targetcount):
            newtargetcoords[i*sourcecount:(i+1)*sourcecount] = sourceslice.coords[targetaxis][i]
            for j in range(0,sourcecount):
                sourceindices = [slice(0,None,1) for k in range(sourceslice.ndim)]
                sourceindices[targetaxis] = slice(i,i+1,1)
                sourceindices[axis] = slice(j,j+1,1)
                targetindices = [slice(0,None,1) for k in range(newdata.ndim)]
                targetindices[inewtargetaxis] = slice(i*sourcecount+j,i*sourcecount+j+1,1)
                newdata[tuple(targetindices)] = sourceslice.data[tuple(sourceindices)]

        del dims[axis]
        newslice = common.Variable.Slice(dims)
        newslice.coords      = [c for i,c in enumerate(sourceslice.coords     ) if i!=axis]
        newslice.coords_stag = [c for i,c in enumerate(sourceslice.coords_stag) if i!=axis]
        newslice.coords[inewtargetaxis] = newtargetcoords
        newslice.data = newdata
        
        if dataonly: return newslice.data
        return newslice

    def getShape(self):
        s = list(LazyOperation.getShape(self))
        s[self.targetaxis] *= s[self.removedim]
        del s[self.removedim]
        return s
        
class interp(expressions.LazyFunction):
    """Function for multidimensional linear interpolation.
    """

    def __init__(self,sourceslice,**kwargs):
        expressions.LazyFunction.__init__(self,self.__class__.__name__,None,sourceslice,outsourceslices=False,**kwargs)
        dims = expressions.LazyFunction.getDimensions(self)
        for d in kwargs.iterkeys():
            assert d in dims, 'Dimension %s does not exist in original slice. Available dimensions: %s' % (d,', '.join(dims))

    def _getValue(self,resolvedargs,resolvedkwargs,dataonly=False):
        assert isinstance(resolvedargs[0],common.Variable.Slice),'The first argument to interpn must be a variable slice.'
        dataslice = resolvedargs[0]
        newslice = dataslice.interp(**resolvedkwargs)
        if dataonly: return newslice.data
        return newslice
                    
    def getShape(self):
        s = expressions.LazyFunction.getShape(self)
        if s is None: return None
        newshape = []
        for n,l in zip(expressions.LazyFunction.getDimensions(self),s):
            if n not in self.kwargs:
                newshape.append(l)
            else:
                argshape = expressions.getShape(self.kwargs[n])
                assert argshape is not None and len(argshape)<=1,'Interpolation locations must be 0d scalars or 1d arrays'
                if not argshape:
                    newshape.append(1)
                else:
                    newshape.append(argshape[0])
        return newshape

class iter(expressions.LazyFunction):
    def __init__(self,target,dimension,stride=1):
        expressions.LazyFunction.__init__(self,self.__class__.__name__,None,target,dimension,stride,outsourceslices=False)
        self.dimension = dimension
        self.stride = stride
        dims = expressions.LazyFunction.getDimensions(self)
        assert dimension in dims,'Dimension to iterate over (%s) is not used by underlying variable, which uses %s.' % (dimension,', '.join(dims))
        self.idimension = list(dims).index(dimension)

    def getValue(self,extraslices=None,dataonly=False):
        if extraslices is None: extraslices = {}
        
        shape = self.getShape()
        dims = self.getDimensions()
        assert shape is not None, 'Unable to iterate over dimension %s because final shape is unknown.' % self.dimension        
        slcs = [extraslices.get(dim,slice(None)) for dim in dims]
        newshape = expressions.LazyExpression.adjustShape(shape,slcs)
        newdims = expressions.LazyExpression.adjustDimensions(dims,slcs)
        
        targetslcs = [slice(None)]*len(newdims)
        targetslcs_stag = list(targetslcs)
        
        if isinstance(slcs[self.idimension],slice):
            start,stop,step = slcs[self.idimension].indices(shape[self.idimension])
            inewdim = list(newdims).index(self.dimension)
        else:
            assert isinstance(slcs[self.idimension],int),'Slice of dimensions to iterate over should be a slice object or an integer (not %s).' % str(slcs[self.idimension])
            start,stop,step = slcs[self.idimension],slcs[self.idimension]+1,1
            inewdim = None
        
        data = numpy.ma.empty(newshape,dtype=numpy.float)
        if not dataonly:
            result = common.Variable.Slice(newdims)
            for j in range(len(newshape)):
                result.coords[j] = numpy.empty(newshape,dtype=numpy.float)
                result.coords_stag[j] = numpy.empty([l+1 for l in newshape],dtype=numpy.float)
            result.data = data
        i = start
        while i<stop:
            ihigh = min(i+self.stride*step,stop)
            print 'Reading %s range %i-%i...' % (self.dimension,i,ihigh-1)
            if inewdim is None:
                extraslices[self.dimension] = i
            else:
                extraslices[self.dimension] = slice(i,ihigh,step)
            resolvedtarget = expressions.LazyExpression.argument2value(self.args[0],extraslices,dataonly=dataonly)

            if inewdim is not None: targetslcs[inewdim] = slice(i,i+(ihigh-i-1)/step+1)
            curdata = resolvedtarget
            if not dataonly:
                curdata = resolvedtarget.data
                if inewdim is not None: targetslcs_stag[inewdim] = slice(targetslcs[inewdim].start,targetslcs[inewdim].stop+1)
                for j in range(len(newshape)):
                    result.coords[j][targetslcs] = resolvedtarget.coords[j]
                    result.coords_stag[j][targetslcs_stag] = resolvedtarget.coords_stag[j]
            data[targetslcs] = curdata
            i = ihigh
        if dataonly: return data
        return result

class coorddelta(expressions.LazyFunction):
    def __init__(self,target,dimension):
        expressions.LazyFunction.__init__(self,self.__class__.__name__,None,target,dimension,outsourceslices=False)
        alldims = self.getDimensions()
        assert dimension in alldims,'Dimension "%s" does not appear in source data (available: %s).' % (dimension,', '.join(alldims))
        self.dimension = dimension

    def getValue(self,extraslices=None,dataonly=False):
        resolvedtarget = expressions.LazyExpression.argument2value(self.args[0],extraslices,dataonly=False)
        dims = list(resolvedtarget.dimensions)
        assert self.dimension in dims,'Dimension "%s" does not appear in source data after slices are applied (remaining: %s).' % (self.dimension,', '.join(dims))
        idimension = dims.index(self.dimension)
        vals = numpy.diff(resolvedtarget.coords_stag[idimension],axis=idimension)
        destagdims = range(len(dims))
        destagdims.pop(idimension)
        resolvedtarget.data = common.center(vals,destagdims)
        if dataonly: return resolvedtarget.data
        return resolvedtarget
        
