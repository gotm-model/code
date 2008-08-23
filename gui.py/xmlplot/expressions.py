import UserDict
import numpy
import common

class ExpressionNamespace(UserDict.DictMixin):
    """Encapsulates a list of dictionary-like objects, that are query from start
    to finish when an item is requested from the containing object. The first match is
    returned.
    """

    def __init__(self,firsttable=None):
        self.tables = []
        if firsttable!=None: self.append(firsttable)
        
    def append(self,object):
        self.tables.append(object)
        
    def keys(self):
        res = set()
        for table in self.tables: res |= set(table.keys())
        return list(res)
    
    def __getitem__(self,name):
        for table in self.tables:
            if name in table: return table[name]
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
        if result==None: result = self.store.children.get(name,None)
        if result==None: raise KeyError(name)
        if isinstance(result,common.VariableStore):
            result = LazyStore(result,name)
            if self.name!=None: result.name = '%s[\'%s\']' % (self.name,name)
        else:
            result = LazyVariable(result)
            if self.name!=None: result.name = '%s[\'%s\']' % (self.name,name)
        return result
        
    def keys(self):
        res = set()
        res |= set(self.store.getVariableNames())
        res |= set(self.store.children.keys())
        return list(res)

class LazyExpression:
    """The light-weight class is the base class for objects that are meant to be used in
    expressions. It supports all mathematical operators, as well as slicing.
    
    Expressions built from these lazy objects can be evaluated without any expensive operation
    (e.g., actual data retrieval). From the resulting lazy object, one can subsequently
    retrieve several properties, such as name, long name, unit, shape, and the actual data.
    These secondary operation may be expensive (particularly data retrieval).
    """

    class NamedFunction:
        def __init__(self,name,func,useslices=False):
            self.name = name
            self.func = func
            self.useslices = useslices
            self.removedim = None
            
        def __call__(self,*args,**kwargs):
            f = None
            try:
                if issubclass(self.func,LazyFunction): f = self.func(*args,**kwargs)
            except Exception,e: pass
            if f==None:
                f = LazyFunction(self.name,self.func,*args,**kwargs)
            if self.removedim!=None: f.setRemovedDimension(argindex=self.removedim[0],argname=self.removedim[1])
            f.useslices = self.useslices
            return f

    globalfuncs = None

    @staticmethod
    def getFunctions():
        if LazyExpression.globalfuncs==None:
            LazyExpression.globalfuncs = {}
            for name in dir(numpy):
                LazyExpression.globalfuncs[name] = LazyExpression.NamedFunction(name,getattr(numpy,name))
            for name in dir(numpy.ma):
                LazyExpression.globalfuncs[name] = LazyExpression.NamedFunction(name,getattr(numpy.ma,name))
            LazyExpression.globalfuncs['average'].removedim = (1,'axis')
            LazyExpression.globalfuncs['mean'].removedim = (1,'axis')
            LazyExpression.globalfuncs['min' ].removedim = (1,'axis')
            LazyExpression.globalfuncs['max' ].removedim = (1,'axis')
            LazyExpression.globalfuncs['sum' ].removedim = (1,'axis')
            LazyExpression.globalfuncs['prod'].removedim = (1,'axis')
            
            import functions
            for name in dir(functions):
                LazyExpression.globalfuncs[name] = LazyExpression.NamedFunction(name,getattr(functions,name),useslices=True)
        return LazyExpression.globalfuncs

    @staticmethod
    def adjustShape(shape,slic):
        baseshape = list(shape)
        for i in range(len(baseshape)-1,-1,-1):
            if isinstance(slic[i],(int,float)):
                del baseshape[i]
            else:
                assert isinstance(slic[i],slice), 'Fancy indexing is not yet supported.'
                start,stop,step = slic[i].indices(baseshape[i])
                assert step==1, 'Slices with step>1 are not yet supported.'
                baseshape[i] = stop-start
        return baseshape
        
    @staticmethod
    def adjustDimensions(dimnames,slic):
        assert len(dimnames)==len(slic), 'Number of slices (%i) does not match number of dimensions (%i).' % (len(slic),len(dimnames))
        dimnames = list(dimnames)
        for i in range(len(dimnames)-1,-1,-1):
            if isinstance(slic[i],(int,float)): del dimnames[i]
        return dimnames

    @staticmethod
    def slice2string(slic):
        """This function takes a single slice object and converts it to a Python slice
        specification string.
        """
        result = ''
        start,stop,step = slic.start,slic.stop,slic.step
        if start!=None: result += str(start)
        result += ':'
        if stop!=None: result += str(stop)
        if step!=None: result += ':'+str(step)
        return result

    @staticmethod
    def slices2string(slices):
        """This function takes a slice object and converts it to a Python slice
        specification string.
        """
        slicestrings = []
        for slic in slices:
            if isinstance(slic,slice):
                slicestrings.append(LazyExpression.slice2string(slic))
            else:
                slicestrings.append(str(slic))
        return '[%s]' % ','.join(slicestrings)

    @staticmethod
    def slices2prettystring(slices,dimnames):
        """This function takes a slice object and converts it to a descriptive slice
        specification string.
        """
        slicestrings = []
        for dimname,slic in zip(dimnames,slices):
            if isinstance(slic,slice):
                res = LazyExpression.slice2string(slic)
            else:
                res = str(slic)
            if res!=':': slicestrings.append('%s=%s' % (dimname,res))
        return '[%s]' % ','.join(slicestrings)

    def __init__(self,*args):
        self.args = args

    def getVariables(self):
        vars = []
        for arg in self.args:
            if isinstance(arg,LazyExpression): vars += arg.getVariables()
        return vars
        
    def getText(self,type=0,addparentheses=True):
        assert False, 'Method "getText" must be implemented by derived class.'

    def getValue(self):
        assert False, 'Method "getValue" must be implemented by derived class.'

    def getShape(self):
        assert False, 'Method "getShape" must be implemented by derived class.'

    def getDimensions(self):
        assert False, 'Method "getDimensions" must be implemented by derived class.'
            
    def __add__(self,other):
        return LazyOperator('__add__','+',self,other)

    def __sub__(self,other):
        return LazyOperator('__sub__','-',self,other)

    def __mul__(self,other):
        return LazyOperator('__mul__','*',self,other)

    def __div__(self,other):
        return LazyOperator('__div__','/',self,other)

    def __truediv__(self,other):
        return LazyOperator('__truediv__','/',self,other)
        
    def __pow__(self,other):
        return LazyOperator('__pow__','**',self,other)

    def __neg__(self):
        return LazyOperator('__neg__','-',self)

    def __pos__(self):
        return LazyOperator('__pos__','+',self)
                
    def __len__(self):
        return 1

    def __getitem__(self,slices):
        return LazySlice(self,slices)
        
class LazyVariable(LazyExpression):
    """This light-weight object encapsulates a Variable.
    """

    def __init__(self,*args):
        LazyExpression.__init__(self,*args)
        self.slice = None
        self.name = None
        
    def getVariables(self):
        return [self.args[0]]

    def getValue(self):
        # Return the data slice of the encapsulated object.
        slic = self.slice
        if slic==None: slic = len(self.args[0].getDimensions())*[slice(None)]
        return self.args[0].getSlice(slic)
        
    def getText(self,type=0,addparentheses=True):
        assert type>=0 and type<=3, 'Argument "type" must be 0 (identifier), 1 (short name) 2 (long name), or 3 (unit).'
        if type==0 or type==1:
            # Return the short name of the object (optionally with slice specification).
            if type==0 and self.name!=None:
                res = self.name
            else:
                res = self.args[0].getName()
            if self.slice!=None:
                res += LazyExpression.slices2string(self.slice)
            return res
        elif type==2:
            # Return the long name of the object (optionally with slice specification).
            res = self.args[0].getLongName()
            if self.slice!=None:
                res += LazyExpression.slices2prettystring(self.slice,self.args[0].getDimensions())
            return res
        else:
            return self.args[0].getUnit()
        
    def getShape(self):
        baseshape = self.args[0].getShape()
        if self.slice!=None: baseshape = LazyExpression.adjustShape(baseshape,self.slice)
        return baseshape

    def getDimensions(self):
        basedims = self.args[0].getDimensions()
        if self.slice!=None: basedims = LazyExpression.adjustDimensions(basedims,self.slice)
        return basedims

    def __getitem__(self,slices):
        # The first slice operation can be transferred to the Variable class directly,
        # so we do not need to create a separate slice object.
        # If the variable already has a slice specification, this is an additional slice
        # operation that can only be handled by the default class, which does create a
        # slice object.
        if self.slice!=None: return LazyExpression.__getitem__(self,slices)
        
        if not isinstance(slices,(list,tuple)): slices = (slices,)
        newvar = LazyVariable(*self.args)
        newvar.slice = slices
        newvar.name = self.name
        return newvar

class LazyOperation(LazyExpression):
    """The light-weight class serves as base for any mathematical operation, which includes
    actions by the normal mathethematical operators as well as NumPy functions.
    """

    def __init__(self,*args):
        LazyExpression.__init__(self,*args)

    def getValue(self):
        resolvedargs = []
        for arg in self.args:
            if isinstance(arg,LazyExpression):
                resolvedargs.append(arg.getValue())
            else:
                resolvedargs.append(arg)
        return self._getValue(resolvedargs)
        
    @staticmethod
    def getData(args,useslices=False):
        firstslice = None
        for i in range(len(args)):
            if isinstance(args[i],common.Variable.Slice):
                # This argument is a Slice object; just take the values from it and use that.
                # Coordinates will still be available in the returned firstslice object, which
                # can be used to save the result of the expression to.
                if firstslice==None: firstslice = args[i]
                if not useslices: args[i] = args[i].data
        return args,firstslice

    def getText(self,type=0,addparentheses=True):
        resolvedargs = []
        for arg in self.args:
            if isinstance(arg,LazyExpression):
                resolvedargs.append(arg.getText(type))
            elif isinstance(arg,basestring):
                resolvedargs.append('\'%s\'' % arg)
            else:
                resolvedargs.append(str(arg))
        result = self._getText(resolvedargs,type=type)
        if addparentheses: result = '(%s)' % result
        return result

    def getShape(self):
        for arg in self.args:
            if isinstance(arg,LazyExpression):
                return arg.getShape()
        return ()

    def getDimensions(self):
        for arg in self.args:
            if isinstance(arg,LazyExpression):
                return arg.getDimensions()
        return ()

    def _getValue(self,resolvedargs,targetslice):
        assert False, 'Method "_getValue" must be implemented by derived class.'

    def _getText(self,type=0):
        assert False, 'Method "_getText" must be implemented by derived class.'

class LazyFunction(LazyOperation):
    """The light-weight class encapsulates a NumPy function.
    """
    
    def __init__(self,name,func,*args,**kwargs):
        self.name = name
        self.func = func
        self.kwargs = kwargs
        self.removedim = None
        self.useslices = False
        LazyOperation.__init__(self,*args)
    
    def setRemovedDimension(self,argindex,argname):
        self.removedim = None
        if argname in self.kwargs:
            self.removedim = self.kwargs[argname]
        elif argindex<len(self.args):
            self.args = list(self.args)
            self.removedim = self.args[argindex]
        if self.removedim==None: return
            
        if isinstance(self.removedim,basestring):
            dims = self.getVariables()[0].getDimensions()
            assert self.removedim in dims,'"%s" is not an existing dimension. Available: %s.' % (self.removedim,', '.join(dims))
            self.removedim = list(dims).index(self.removedim)
            if argname in self.kwargs:
                self.kwargs[argname] = self.removedim
            else:
                self.args[argindex] = self.removedim
        
    def _getValue(self,resolvedargs):
        resolvedargs,targetslice = LazyOperation.getData(resolvedargs,useslices=self.useslices)

        data = self.func(*resolvedargs,**self.kwargs)
        
        # If a slice or single value is returned, pass it directly.
        if isinstance(data,(int,float,common.Variable.Slice)): return data

        # If the function has removed a dimension, make sure it is removed from the coordinate array as well.
        if self.removedim!=None: targetslice.removeDimension(self.removedim)

        targetslice.data = data
        targetslice.debugCheck('Invalid result of function "%s"' % self.name)
        return targetslice
        
    def getShape(self):
        s = list(LazyOperation.getShape(self))
        if self.removedim!=None: del s[self.removedim]
        return s

    def getDimensions(self):
        dims = list(LazyOperation.getDimensions(self))
        if self.removedim!=None: del dims[self.removedim]
        return dims

    def _getText(self,resolvedargs,type=0):
        strkw = []
        for k,v in self.kwargs.iteritems():
            if isinstance(v,basestring): v = '\'%s\'' % v
            strkw.append('='.join((k,str(v))))
        resolvedargs += strkw
        return '%s(%s)' % (self.name,','.join(resolvedargs))

class LazyOperator(LazyOperation):
    """The light-weight class encapsulates a unary of binary mathematical operator, as
    recognized by Python.
    """

    def __init__(self,name,symbol,*args):
        self.name = name
        self.symbol = symbol
        LazyOperation.__init__(self,*args)

    def _getValue(self,resolvedargs):
        resolvedargs,targetslice = LazyOperation.getData(resolvedargs)
        targetslice.data = getattr(resolvedargs[0],self.name)(*resolvedargs[1:])
        return targetslice

    def _getText(self,resolvedargs,type=0):
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
    def _getValue(self,resolvedargs):
        return resolvedargs[0].__getitem__(self.slice)
    def _getText(self,resolvedargs,type=0):
        if type==2:
            return resolvedargs[0] + LazyExpression.slices2prettystring(self.slice,self.args[0].getDimensions())
        else:
            return resolvedargs[0] + LazyExpression.slices2string(self.slice)
    def getShape(self):
        return LazyExpression.adjustShape(self.args[0].getShape(),self.slice)
    def getDimensions(self):
        return LazyExpression.adjustDimensions(self.args[0].getDimensions(),self.slice)

class VariableExpression(common.Variable):
    def __init__(self,expression,namespace):
        assert isinstance(namespace,ExpressionNamespace),'The namespace must be provided as ExpressionNamespace object.'
        common.Variable.__init__(self,None)
        namespace.append(LazyExpression.getFunctions())
        self.root = eval(expression,{},namespace)
        if not isinstance(self.root,(list,tuple)): self.root = [self.root]
        assert self.root, 'Expression "%s" returns an empty list.'
        self.variables = []
        for entry in self.root:
            if isinstance(entry,LazyExpression): self.variables += entry.getVariables()
        assert self.variables, 'Expression "%s" does not reference any variable.' % expression
        
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

    def getSlice(self,bounds):
        return [node.getValue() for node in self.root]

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