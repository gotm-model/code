import numpy
import xmlstore.util
import xmlplot.common

class HDF4Store(xmlplot.common.VariableStore,xmlstore.util.referencedobject):

    class Variable(xmlplot.common.Variable):
        def __init__(self,store,hdfvar):
            xmlplot.common.Variable.__init__(self,store)
            self.hdfvar = hdfvar
            self.info = self.hdfvar.info()
            
        def getName_raw(self):
            return self.info[0]
            
        def getDimensions_raw(self):
            dimnames = []
            for idim in range(self.info[1]):
                dim = self.hdfvar.dim(idim)
                dimnames.append(dim.info()[0])
            return dimnames
            
        def getLongName(self):
            atts = self.getProperties()
            if 'long_name' in atts: return atts['long_name']
            return xmlplot.common.Variable.getLongName(self)

        def getUnit(self):
            atts = self.getProperties()
            if 'units' in atts: return atts['units']
            return xmlplot.common.Variable.getUnit(self)
            
        def getShape(self):
            return self.hdfvar.info()[2]
            
        def getDataType(self):
            return self.hdfvar.info()[3]
            
        def getProperties(self):
            return self.hdfvar.attributes()
            
        def getSlice(self,bounds,dataonly=False):
            dat = numpy.asarray(self.hdfvar.get())
            fillvalue = self.hdfvar.attributes().get('Fill',None)
            if fillvalue is not None:
                dat = numpy.ma.array(dat,mask=(dat==fillvalue),copy=False)
                
            if dataonly: return dat

            dims = self.getDimensions_raw()
            varslice = self.Slice(dims)
            varslice.data = dat
            for idim,length in enumerate(dat.shape):
                dimname = dims[idim]
                coords = numpy.arange(0,length-0.999,1.,dtype=numpy.float)
                coords_stag = numpy.arange(-0.5,length-0.499,1.,dtype=numpy.float)

                # Insert data dimensions where they are lacking in coordinate
                coords      = xmlplot.common.broadcastSelective(coords,     (dimname,),dat.shape,               dims)
                coords_stag = xmlplot.common.broadcastSelective(coords_stag,(dimname,),[l+1 for l in dat.shape],dims)

                # Assign coordinate values
                varslice.coords     [idim] = coords
                varslice.coords_stag[idim] = coords_stag

            return varslice

    def __init__(self,path):
        xmlplot.common.VariableStore.__init__(self)
        xmlstore.util.referencedobject.__init__(self)
        from pyhdf.SD import SD, SDC
        self.file = SD(str(path),SDC.READ)

    def getVariable_raw(self,varname):
        """Returns a Variable object for the given original short variable name.
        The method must be implemented by derived classes.
        """
        return self.Variable(self,self.file.select(varname))

    def getVariableNames_raw(self):
        """Returns a list of original short names for all variables present in the store.
        The method must be implemented by derived classes.
        """
        return self.file.datasets().keys()

    def unlink(self):
        self.file.end()