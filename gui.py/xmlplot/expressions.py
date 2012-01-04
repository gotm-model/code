import UserDict,types
import numpy
import common

def getShape(obj):
    if isinstance(obj,(int,float)):
        return ()
    elif isinstance(obj,LazyExpression):
        return obj.getShape()
    elif isinstance(obj,numpy.ndarray):
        return obj.shape
    elif isinstance(obj,(list,tuple)):
        return (len(obj),)
    else:
        return None

class ExpressionNamespace(UserDict.DictMixin):
    """Encapsulates a list of dictionary-like objects, that are query from start
    to finish when an item is requested from the containing object. The first match is
    returned.
    """

    def __init__(self,firsttable=None):
        self.tables = []
        if firsttable is not None: self.append(firsttable)
        
    def append(self,object):
        self.tables.append(object)
        
    def keys(self):
        res = set()
        for table in self.tables: res |= set(table.keys())
        return list(res)
    
    def __getitem__(self,name):
        for table in self.tables:
            if name in table:
                return table[name]
        raise KeyError('"%s" does not exist in namespace' % name)
        
class LazyStore(UserDict.DictMixin):
    """The light-weight object encapsulates a VariableStore, from which children
    (variables and child stores) can be obtained as attributes (__getattr__) or by indexing
    (__getitem__). Children are in turn retrieved as "lazy" objects, i.e., LazyVariable and
    LazyStore objects.
    
    These lazy objects are meant to be used in expressions that can then be evaluated without
    any expensive operation (e.g., actual data retrieval).

    Note that LazyStore objects should never remain in a evaluated expression: they
    should always be followed by an attribute- or index-based variable access, that returns
    a LazyVariable object. LazyStore objects therefore do not support operations of any kind.
    """

    def __init__(self,store,name=None):
        assert isinstance(store,common.VariableStore),'First argument must be of type Store.'
        self.store = store
        self.name = name
        
    def __getattr__(self,name):
        try:
            return self.__getitem__(name)
        except KeyError:
            raise AttributeError(name)

    def __getitem__(self,name):
        result = self.store.getVariable(name)
        if result is None: result = self.store.children.get(name,None)
        if result is None: raise KeyError(name)
        if isinstance(result,common.VariableStore):
            result = LazyStore(result,name)
            if self.name is not None: result.name = '%s[\'%s\']' % (self.name,name)
        else:
            result = LazyVariable(result)
            if self.name is not None: name = '%s[\'%s\']' % (self.name,name)
            result.name = name
        return result
        
    def keys(self):
        res = set()
        res |= set(self.store.getVariableNames())
        res |= set(self.store.children.keys())
        return list(res)

class LazyExpression(object):
    """The light-weight class is the base class for objects that are meant to be used in
    expressions. It supports all mathematical operators, as well as slicing.
    
    Expressions built from these lazy objects can be evaluated without any expensive operation
    (e.g., actual data retrieval). From the resulting lazy object, one can subsequently
    retrieve several properties, such as name, long name, unit, shape, and the actual data.
    These secondary operation may be expensive (particularly data retrieval).
    """
    class NamedFunction(object):
        def __init__(self,name,func,useslices=False):
            self.name = name
            self.func = func
            self.useslices = useslices
            self.preserveunit = False
            self.usefirstunit = False
            self.removedim = None
            
        def __call__(self,*args,**kwargs):
            aware = False
            try:
                if issubclass(self.func,LazyFunction): aware = True
            except TypeError: pass
            if aware:
                f = self.func(*args,**kwargs)
            else:
                f = LazyFunction(self.name,self.func,*args,**kwargs)
                f.preserveunit = self.preserveunit
                f.usefirstunit = self.usefirstunit
            if self.removedim is not None: f.setRemovedDimension(argindex=self.removedim[0],argname=self.removedim[1])
            f.useslices = self.useslices
            return f

    globalfuncs = None

    @staticmethod
    def getFunctions():
        if LazyExpression.globalfuncs is None:
            LazyExpression.globalfuncs = {}
            def addFromModule(mod):
                for name in dir(mod):
                    item = getattr(mod,name)
                    if callable(item):
                        LazyExpression.globalfuncs[name] = LazyExpression.NamedFunction(name,item)
                    else:
                        LazyExpression.globalfuncs[name] = item
            addFromModule(numpy)
            addFromModule(numpy.ma)
            LazyExpression.globalfuncs['average'].removedim = (1,'axis')
            LazyExpression.globalfuncs['mean'].removedim = (1,'axis')
            LazyExpression.globalfuncs['min' ].removedim = (1,'axis')
            LazyExpression.globalfuncs['max' ].removedim = (1,'axis')
            LazyExpression.globalfuncs['sum' ].removedim = (1,'axis')
            LazyExpression.globalfuncs['prod'].removedim = (1,'axis')

            LazyExpression.globalfuncs['average'].usefirstunit = True
            LazyExpression.globalfuncs['mean'].usefirstunit = True
            LazyExpression.globalfuncs['min' ].usefirstunit = True
            LazyExpression.globalfuncs['max' ].usefirstunit = True
            LazyExpression.globalfuncs['sum' ].usefirstunit = True

            import functions
            for name in dir(functions):
                LazyExpression.globalfuncs[name] = LazyExpression.NamedFunction(name,getattr(functions,name),useslices=True)
                
        return LazyExpression.globalfuncs

    @staticmethod
    def adjustShape(shape,slic):
        if shape is None: return None
        slic = common.processEllipsis(slic,len(shape))
        assert len(shape)==len(slic), 'Number of slices (%i) does not match number of dimensions (%i).' % (len(slic),len(shape))
        baseshape = list(shape)
        for i in range(len(baseshape)-1,-1,-1):
            if isinstance(slic[i],slice):
                # For non-integer slices we do not know the resulting shape
                if not (isinstance(slic[i].start,(int,types.NoneType)) and isinstance(slic[i].stop,(int,types.NoneType))): return None

                start,stop,step = slic[i].indices(baseshape[i])
                baseshape[i] = (stop-start-1)/step+1
            else:
                curshape = getShape(slic[i])
                if curshape is None or len(curshape)>0:
                    # Slice object will be an array or has unknown shape - final shape is unknown
                    return None
                del baseshape[i]

        return baseshape
        
    @staticmethod
    def adjustDimensions(dimnames,slic):
        slic = common.processEllipsis(slic,len(dimnames))
        assert len(dimnames)==len(slic), 'Number of slices (%i) does not match number of dimensions (%i).' % (len(slic),len(dimnames))
        dimnames = list(dimnames)
        for i in range(len(dimnames)-1,-1,-1):
            if not isinstance(slic[i],slice):
                curshape = getShape(slic[i])
                if curshape is not None and len(curshape)==0: del dimnames[i]
        return dimnames
        
    @staticmethod
    def slice2string(slic):
        """This function takes a single slice object and converts it to a Python slice
        specification string.
        """
        if slic is Ellipsis:
            return '...'
        elif isinstance(slic,LazyExpression):
            return slic.getText(addparentheses=False)
        elif not isinstance(slic,slice):
            return str(slic)
        result = ''
        start,stop,step = slic.start,slic.stop,slic.step
        if start is not None: result += LazyExpression.argument2text(start)
        result += ':'
        if stop is not None: result += LazyExpression.argument2text(stop)
        if step is not None: result += ':'+LazyExpression.argument2text(step)
        return result

    @staticmethod
    def slices2string(slices):
        """This function takes a slice object and converts it to a Python slice
        specification string.
        """
        slicestrings = [LazyExpression.slice2string(slic) for slic in slices]
        if len(slicestrings)==0: return ''
        return '[%s]' % ','.join(slicestrings)

    @staticmethod
    def slices2prettystring(slices,dimnames):
        """This function takes a slice object and converts it to a descriptive slice
        specification string.
        """
        slices = common.processEllipsis(slices,len(dimnames))
        slicestrings = []
        for dimname,slic in zip(dimnames,slices):
            res = LazyExpression.slice2string(slic)
            if res!=':' and res!='...': slicestrings.append('%s=%s' % (dimname,res))
        return '[%s]' % ','.join(slicestrings)

    @staticmethod
    def combineSlices(baseslices,extraslices):
        newslices = []
        iextra = 0
        for sl in baseslices:
            if isinstance(sl,slice):
                start,stop,step = sl.start,sl.stop,sl.step
                assert iextra<len(extraslices),'Number of dimensions of secondary slice %s does not match shape of data returned by base slice %s' % (str(extraslices),str(baseslices))
                if isinstance(extraslices[iextra],slice):
                    start = start + extraslices[iextra].start*step
                    stop  = start + extraslices[iextra].stop*step
                    step *= extraslices[iextra].step
                    sl = slice(start,stop,step)
                else:
                    sl = start + extraslices[iextra]*step
                iextra += 1
            newslices.append(sl)
        return tuple(newslices)

    @staticmethod
    def argument2value(arg,slic=None,dataonly=False):
        if isinstance(arg,LazyExpression):
            dims = list(arg.getDimensions())
            procslic = {}
            if slic:
                # Separate sliced dimensions in those that can be processed by the argument,
                # and those that should be applied afterwards.
                slic = dict([(k,v) for k,v in slic.iteritems() if k in dims])
                for idim in range(len(dims)-1,-1,-1):
                    dim = dims[idim]
                    if dim in slic and arg.canProcessSlice(dim):
                        procslic[dim] = slic[dim]
                        if not isinstance(slic[dim],slice): del dims[idim]
                        del slic[dim]
                    
            # Obtain the data
            if procslic:
                res = arg.getValue(extraslices=procslic,dataonly=dataonly)
            else:
                res = arg.getValue(dataonly=dataonly)
                
            # Apply remaining slices (if any)
            if slic:
                slic = [slic.get(dim,slice(None)) for dim in dims]
                res = res.__getitem__(slic)
                
            return res
        elif isinstance(arg,tuple):
            return tuple([LazyExpression.argument2value(subarg,slic,dataonly) for subarg in arg])
        elif isinstance(arg,list):
            return [LazyExpression.argument2value(subarg,slic,dataonly) for subarg in arg]
        elif isinstance(arg,slice):
            start,stop,step = [LazyExpression.argument2value(obj,slic,dataonly) for obj in (arg.start,arg.stop,arg.step)]
            return slice(start,stop,step)
        else:
            return arg

    @staticmethod
    def argument2text(arg,type=0):
        if isinstance(arg,LazyExpression):
            return arg.getText(type)
        elif isinstance(arg,basestring):
            return '\'%s\'' % arg
        elif isinstance(arg,(tuple,list)):
            subargs = [LazyExpression.argument2text(subarg,type) for subarg in arg]
            if isinstance(arg,list):
                # list
                return '[%s]' % ','.join(subargs)
            else:
                # tuple
                if len(subargs)==1:
                    return '(%s,)' % subargs[0]
                else:
                    return '(%s)' % ','.join(subargs)
        else:
            return str(arg)

    def __init__(self,*args,**kwargs):
        self.canprocessslice = False
        self.useslices = False
        self.preserveunit = False
        self.usefirstunit = False
        self.args = args
        self.kwargs = kwargs
        
    def canProcessSlice(self,dimension):
        return self.canprocessslice
        
    def debugPrint(self,indent=''):
        print indent+'%s, dims=(%s), shape=%s' % (str(self),','.join(self.getDimensions()),str(self.getShape()))
        indent = ' '*len(indent)
        for arg in self.args:
            if isinstance(arg,LazyExpression):
                arg.debugPrint(indent+'  ')
            else:
                print indent+'  '+str(arg)
        for name,arg in self.kwargs.iteritems():
            if isinstance(arg,LazyExpression):
                arg.debugPrint(indent+'  '+name+'=')
            else:
                print indent+'  '+name+'='+str(arg)

    def getVariables(self):
        vars = []
        for arg in self.args:
            if isinstance(arg,LazyExpression): vars += arg.getVariables()
        for arg in self.kwargs.itervalues():
            if isinstance(arg,LazyExpression): vars += arg.getVariables()
        return vars
        
    def getText(self,type=0,addparentheses=True):
        resolvedargs = [LazyExpression.argument2text(arg,type) for arg in self.args]
        resolvedkwargs = dict([(name,LazyExpression.argument2text(arg,type)) for name,arg in self.kwargs.iteritems()])
        if type==3:
            if self.usefirstunit:
                return resolvedargs[0]
            elif self.preserveunit:
                units = [arg for arg in resolvedargs if arg!='']
                units.sort()
                if units[0]==units[-1]: return units[0]
        result = self._getText(resolvedargs,resolvedkwargs,type=type,addparentheses=addparentheses)
        if addparentheses: result = '(%s)' % result
        return result

    def _getText(self,type=0,addparentheses=True):
        assert False, 'Method "getText" or "_getText" must be implemented by derived class.'

    def getValue(self,extraslices=None,dataonly=False):
        if extraslices is not None:
            # Filter out slices for dimensions that we do not support.
            dims = self.getDimensions()
            extraslices = dict([(k,v) for k,v in extraslices.iteritems() if k in dims])
        resolvedargs = [LazyExpression.argument2value(arg,extraslices,dataonly=(dataonly and not self.useslices)) for arg in self.args]
        resolvedkwargs = dict([(name,LazyExpression.argument2value(arg,extraslices,dataonly=(dataonly and not self.useslices))) for name,arg in self.kwargs.iteritems()])
        return self._getValue(resolvedargs,resolvedkwargs,dataonly=dataonly)

    def _getValue(self,args,kwargs):
        assert False, 'Method "getValue" or "_getValue" must be implemented by derived class.'

    def getShape(self):
        assert False, 'Method "getShape" must be implemented by derived class.'

    def getDimensions(self):
        assert False, 'Method "getDimensions" must be implemented by derived class.'
            
    def __add__(self,other):
        return LazyOperator('__add__','+',self,other,preserveunit=True)

    def __sub__(self,other):
        return LazyOperator('__sub__','-',self,other,preserveunit=True)

    def __mul__(self,other):
        return LazyOperator('__mul__','*',self,other)

    def __div__(self,other):
        return LazyOperator('__div__','/',self,other)

    def __mod__(self,other):
        return LazyOperator('__mod__','%',self,other,preserveunit=True)

    def __truediv__(self,other):
        return LazyOperator('__truediv__','/',self,other)
        
    def __pow__(self,other):
        return LazyOperator('__pow__','**',self,other)

    def __radd__(self,other):
        return LazyOperator('__radd__','+',self,other,preserveunit=True)

    def __rsub__(self,other):
        return LazyOperator('__rsub__','-',self,other,preserveunit=True)

    def __rmul__(self,other):
        return LazyOperator('__rmul__','*',self,other)

    def __rdiv__(self,other):
        return LazyOperator('__rdiv__','/',self,other)

    def __rmod__(self,other):
        return LazyOperator('__rmod__','%',self,other,preserveunit=True)

    def __rtruediv__(self,other):
        return LazyOperator('__rtruediv__','/',self,other)
        
    def __rpow__(self,other):
        return LazyOperator('__rpow__','**',self,other)

    def __neg__(self):
        return LazyOperator('__neg__','-',self,preserveunit=True)

    def __pos__(self):
        return LazyOperator('__pos__','+',self,preserveunit=True)

    def __gt__(self,other):
        return LazyOperator('__gt__','>',self,other)

    def __ge__(self,other):
        return LazyOperator('__ge__','>=',self,other)

    def __lt__(self,other):
        return LazyOperator('__lt__','<',self,other)

    def __le__(self,other):
        return LazyOperator('__le__','<=',self,other)

    def __eq__(self,other):
        return LazyOperator('__eq__','==',self,other)

    def __ne__(self,other):
        return LazyOperator('__ne__','!=',self,other)

    def __len__(self):
        return 1

    def __getitem__(self,slices):
        return LazySlice(self,slices)
        
class LazyVariable(LazyExpression):
    """This light-weight object encapsulates a Variable.
    """

    def __init__(self,*args):
        LazyExpression.__init__(self,*args)
        self.name = None
        self.canprocessslice = True
        
    def getVariables(self):
        return [self.args[0]]

    def getValue(self,extraslices=None,dataonly=False):
        # Return the data slice of the encapsulated object.
        if extraslices is None:
            extraslices = {}
        else:
            # Make sure any lazy objects in the slice specification are resolved
            for k in extraslices.keys():
                extraslices[k] = LazyExpression.argument2value(extraslices[k],dataonly=True)
        slics = [extraslices.get(dim,slice(None)) for dim in self.args[0].getDimensions()]
        return self.args[0].getSlice(slics,dataonly=dataonly)
        
    def getText(self,type=0,addparentheses=True):
        assert type>=0 and type<=3, 'Argument "type" must be 0 (identifier), 1 (short name) 2 (long name), or 3 (unit).'
        if type==0 or type==1:
            # Return the short name of the variable.
            if type==0 and self.name is not None: return self.name
            return self.args[0].getName()
        elif type==2:
            # Return the long name of the variable.
            return self.args[0].getLongName()
        else:
            # Return the unit of the variable.
            return self.args[0].getUnit()
        
    def getShape(self):
        return self.args[0].getShape()

    def getDimensions(self):
        return self.args[0].getDimensions()

class LazyOperation(LazyExpression):
    """The light-weight class serves as base for any mathematical operation, which includes
    actions by the normal mathematical operators as well as NumPy functions.
    """

    def __init__(self,*args,**kwargs):
        kw = kwargs.copy()
        self.outsourceslices = kw.pop('outsourceslices',False)
        LazyExpression.__init__(self,*args,**kw)
        self.canprocessslice = True
                
    @staticmethod
    def getData(args,useslices=False):
        """Takes a list of arguments, finds the first contained Variable.Slice object,
        and converts each Variable.Slice to a NumPy array if specified. The former allows
        the caller to find an exisitng array to store the return value in, and the latter
        serves for functions that operate on NumPy arrays and do not know about
        Variable.Slice.
        """
        firstslice = None
        for i in range(len(args)):
            if isinstance(args[i],common.Variable.Slice):
                # This argument is a Slice object; just take the values from it and use that.
                # Coordinates will still be available in the returned firstslice object, which
                # can be used to save the result of the expression to.
                if firstslice is None: firstslice = args[i]
                if not useslices: args[i] = args[i].data
        return args,firstslice

    def getShape(self):
        shape = []
        for arg in self.args:
            if not isinstance(arg,LazyExpression): continue

            # Retrieve the argument's shape
            curshape = arg.getShape()
            
            # If the argument's shape is unknown, the final shape will be unknown too.
            if curshape is None: return None
            
            # If this is the first argument, just store its shape and continue.
            if not shape:
                shape = list(curshape)
                continue

            # Check whether the number of dimensions of arguments match.
            # If not, apply NumPy broadcasting rule.
            if len(curshape)!=len(shape):
                if len(curshape)<len(shape):
                    curshape = [1]*(len(shape)-len(curshape))+list(curshape)
                else:
                    shape = [1]*(len(curshape)-len(shape))+shape
                
            # Compare dimension lengths one by one.
            for idim in range(len(curshape)):
                if shape[idim]!=curshape[idim]:
                    # Dimension lengths do not match
                    if shape[idim]==1:
                        # Broadcasting will be applied: no problem.
                        shape[idim] = curshape[idim]
                    elif curshape[idim]!=1:
                        # Dimensions lengths of the arguments do not match, and broadcasting cannot fix it.
                        # (neither has length 1). This will often cause a fatal error, but it may be for some
                        # operations (e.g., certain NumPy functions). However, the final shape will be unknown.
                        return None
                        
        return shape

    def getDimensions(self):
        # Return the largest set of dimensions from any of the arguments.
        dims = ()
        for arg in self.args:
            if isinstance(arg,LazyExpression):
                curdims = arg.getDimensions()
                if len(dims)<len(curdims): dims = curdims
        return dims

    def __getitem__(self,slices):
        shape = self.getShape()
        if self.outsourceslices and shape is not None:
            # We are allowed to outsource the slicing to the arguments of the expression.
            # This takes away the need of first getting all data to operate on, and then
            # take a slice, which is time-consuming and memory-intensive.
            slices = common.processEllipsis(slices,len(shape))
            newargs = []
            for arg in self.args:
                if isinstance(arg,LazyExpression):
                    # Argument is an expression: outsource the slicing
                    argshape = arg.getShape()
                    nmissing = len(shape)-len(argshape)
                    if nmissing: argshape = [1]*nmissing+list(argshape)
                    curslices = []
                    for i,s in enumerate(slices):
                        if argshape[i]==1 and shape[i]>1:
                            # Argument dimension length = 1, but target dimension length > 1
                            # NumPy will perform broadcasting: intelligently adjust the slice for the argument.
                            if isinstance(s,slice):
                                curslices.append(slice(0,1))
                            else:
                                curslices.append(0)
                        else:
                            # Argument and target dimension lengths match: use the slice as given.
                            curslices.append(s)
                            
                    # Slice the argument
                    arg = arg.__getitem__(tuple(curslices[nmissing:]))
                    
                newargs.append(arg)

            import copy
            copy = copy.copy(self)
            copy.args = tuple(newargs)
            return copy
        else:
            # No outsourcing of slicing: apply the slice only after this expression is done.
            return LazyExpression.__getitem__(self,slices)

class LazyFunction(LazyOperation):
    """The light-weight class encapsulates a NumPy function.
    """
    
    def __init__(self,name,func,*args,**kwargs):
        self.name = name
        self.func = func
        self.removedim = None
        kwargs.setdefault('outsourceslices',True)
        LazyOperation.__init__(self,*args,**kwargs)
        self.canprocessslice = True
    
    def setRemovedDimension(self,argindex,argname):
        """Called by derived classes to specify through which function arguments (index
        and name) the dimension that will be removed is specified. Only used if the function
        actually removes a dimension (e.g., mean,min,max,...).
        
        For numpy functions, the name of the argument is typically "axis".
        """
        self.removedim = -1
        if argname in self.kwargs:
            self.removedim = self.kwargs[argname]
        elif argindex<len(self.args):
            self.args = list(self.args)
            self.removedim = self.args[argindex]
        
        self.outsourceslices = False
            
        if isinstance(self.removedim,basestring):
            dims = LazyOperation.getDimensions(self)
            assert self.removedim in dims,'"%s" is not an existing dimension. Available: %s.' % (self.removedim,', '.join(dims))
            self.removedim = list(dims).index(self.removedim)
            if argname in self.kwargs:
                self.kwargs[argname] = self.removedim
            else:
                self.args[argindex] = self.removedim
        
    def _getValue(self,resolvedargs,resolvedkwargs,dataonly=False):
        resolvedargs,targetslice = LazyOperation.getData(resolvedargs,useslices=self.useslices)

        data = self.func(*resolvedargs,**resolvedkwargs)
        
        # If no NumPy array is returned, do nothing
        if dataonly or (not isinstance(data,numpy.ndarray)) or targetslice is None: return data

        # If the function has removed a dimension, make sure it is removed from the coordinate array as well.
        if self.removedim is not None:
            if self.removedim==-1:
                targetslice = common.Variable.Slice()
            else:
                targetslice.removeDimension(self.removedim)

        targetslice.data = data
        targetslice.debugCheck('Invalid result of function "%s"' % self.name)
        return targetslice
        
    def getShape(self):
        # Get the shape with the default implementation, but
        # slice out the dimension that this function will remove (if any)
        if self.removedim==-1: return ()
        startshape = LazyOperation.getShape(self)
        if startshape is None: return None
        s = list(startshape)
        if self.removedim is not None: del s[self.removedim]
        return s

    def getDimensions(self):
        # Get the dimensions with the default implementation, but
        # slice out the dimension that this function will remove (if any)
        if self.removedim==-1: return []
        dims = list(LazyOperation.getDimensions(self))
        if self.removedim is not None: del dims[self.removedim]
        return dims

    def _getText(self,resolvedargs,resolvedkwargs,type=0,addparentheses=False):
        strargs = list(resolvedargs)
        for k,v in resolvedkwargs.iteritems(): strargs.append('='.join((k,v)))
        return '%s(%s)' % (self.name,','.join(strargs))

class LazyOperator(LazyOperation):
    """The light-weight class encapsulates a unary of binary mathematical operator, as
    recognized by Python.
    """

    def __init__(self,name,symbol,*args,**kwargs):
        self.name = name
        self.symbol = symbol
        preserveunit = kwargs.pop('preserveunit',False)
        kwargs['outsourceslices'] = True
        LazyOperation.__init__(self,*args,**kwargs)
        self.preserveunit = preserveunit

    def _getValue(self,resolvedargs,resolvedkwargs,dataonly=False):
        resolvedargs,targetslice = LazyOperation.getData(resolvedargs)
        data = getattr(resolvedargs[0],self.name)(*resolvedargs[1:])
        if dataonly: return data
        targetslice.data = data
        return targetslice

    def _getText(self,resolvedargs,resolvedkwargs,type=0,addparentheses=False):
        if len(resolvedargs)==1:
            # Unary operator: just prepend the symbol.
            return self.symbol+resolvedargs[0]
        else:
            # Binary operator: join strings with the specified symbol in between.
            return self.symbol.join(resolvedargs)

class LazySlice(LazyOperation):
    """The light-weight class encapsulates a slicing operation.
    """

    def __init__(self,variable,slic):
        assert isinstance(variable,LazyExpression),'LazySlice must be initialized with a LazyExpression object to take the slice from.'
        LazyOperation.__init__(self,variable)
        if not isinstance(slic,(list,tuple)): slic = (slic,)
        self.slice = slic
        self.simpleslices = True
        for sl in self.slice:
            if not (sl is Ellipsis or isinstance(sl,int) or (isinstance(sl,slice) and isinstance(sl.start,(int,types.NoneType)) and isinstance(sl.stop,(int,types.NoneType)) and isinstance(sl.step,(int,types.NoneType)))):
                self.simpleslices = False
                break
        self.canprocessslice = self.simpleslices and variable.getShape() is not None

    def getValue(self,extraslices=None,dataonly=False):
        # Resolve any lazy objects in our slice specification.
        slices = LazyExpression.argument2value(self.slice,dataonly=True)
        slices = common.processEllipsis(slices,len(self.args[0].getDimensions()))
        if self.simpleslices:
            # Slice specification is integer-based - determine the final indices based on the shape of the data.
            # This is *required* if this slice is to be combined with a higher level slice (extraslices argument).
            shape = self.args[0].getShape()
            if shape is not None:
                newslices = []
                for sl,l in zip(slices,shape):
                    if isinstance(sl,slice):
                        start,stop,step = sl.indices(l)
                        sl = slice(start,stop,step)
                    newslices.append(sl)
                slices = tuple(newslices)
                
        if extraslices is not None:
            # Add the additional slices
            assert self.simpleslices,'LazySlice variable receives additional slice, but does not support slice combination.'
            shape = self.getShape()
            extraslices = [extraslices.get(d,slice(0,shape[i],1)) for i,d in enumerate(self.getDimensions())]
            slices = LazyExpression.combineSlices(slices,extraslices)
            
        # Convert list of slices to dictionary linking dimension name to corresponding slice.
        slices = dict(zip(self.args[0].getDimensions(),slices))
            
        return LazyExpression.argument2value(self.args[0],slices,dataonly=dataonly)
        
    def _getText(self,resolvedargs,resolvedkwargs,type=0,addparentheses=False):
        if type==2:
            # Long variable name
            return resolvedargs[0] + LazyExpression.slices2prettystring(self.slice,self.args[0].getDimensions())
        elif type==3:
            # Variable unit -  slicing does not affect the unit.
            return resolvedargs[0]
        else:
            return resolvedargs[0] + LazyExpression.slices2string(self.slice)
    def getShape(self):
        return LazyExpression.adjustShape(self.args[0].getShape(),self.slice)
    def getDimensions(self):
        return LazyExpression.adjustDimensions(self.args[0].getDimensions(),self.slice)

class VariableExpression(common.Variable):
    @staticmethod
    def resolve(expression,namespace):
        assert isinstance(namespace,ExpressionNamespace),'The namespace must be provided as ExpressionNamespace object.'
        namespace.append(LazyExpression.getFunctions())
        root = eval(expression,{},namespace)
        if isinstance(root,LazyStore):
            root.store.namespacename = root.name
            return root.store
        elif isinstance(root,LazyVariable):
            root.args[0].namespacename = root.name
            return root.args[0]
        return VariableExpression(root)

    def __init__(self,root):
        common.Variable.__init__(self,None)
        self.root = root
        if not isinstance(self.root,(list,tuple)): self.root = [self.root]
        
        #for v in self.root: v.debugPrint()
        
        assert self.root, 'Expression returns an empty list.'
        self.variables = []
        for entry in self.root:
            if isinstance(entry,LazyExpression): self.variables += entry.getVariables()
        assert self.variables, 'Expression does not reference any variable.'
        
    def __getitem__(self,slices):
        newroot = []
        for entry in self.root:
            newentry = entry
            if isinstance(entry,LazyExpression): newentry = entry[slices]
            newroot.append(newentry)
        return VariableExpression(newroot)
        
    def buildExpression(self):
        result = ','.join([node.getText(type=0,addparentheses=False) for node in self.root])
        if len(self.root)>1: result = '['+result+']'
        return result

    def getItemCount(self):
        return len(self.root)

    def getName_raw(self):
        return ', '.join([node.getText(type=1,addparentheses=False) for node in self.root])
        
    def getLongName(self):
        return ', '.join([node.getText(type=2,addparentheses=False) for node in self.root])

    def getUnit(self):
        return ', '.join([node.getText(type=3,addparentheses=False) for node in self.root])

    def getSlice(self,bounds=None,dataonly=False):
        ndim = len(self.getDimensions())
        if bounds is None: bounds = (Ellipsis,)
        bounds = common.processEllipsis(bounds,ndim)
        s = [node[bounds].getValue(dataonly=dataonly) for node in self.root]
        return s

    def getShape(self):
        return self.root[0].getShape()
        
    def getDimensions(self):
        return self.root[0].getDimensions()

    def hasReversedDimensions(self):
        return self.variables[0].hasReversedDimensions()

    def getDimensions_raw(self):
        return self.variables[0].getDimensions_raw()
                
    def getDimensionInfo_raw(self,dimname):
        return self.variables[0].getDimensionInfo_raw(dimname)