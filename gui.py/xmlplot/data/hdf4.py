import numpy
import xmlstore.util
import xmlplot.common

datatypes = {3:numpy.ubyte,
             4:numpy.byte,
             5:numpy.float32,
             6:numpy.float64,
             20:numpy.int8,
             21:numpy.uint8,
             22:numpy.int16,
             23:numpy.uint16,
             24:numpy.int32,
             25:numpy.uint32}

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
            shape = self.info[2]
            if isinstance(shape,int): shape = (shape,)
            return shape
            
        def getDataType(self):
            return datatypes.get(self.info[3],None)
            
        def getProperties(self):
            return self.hdfvar.attributes()
            
        def getSlice(self,bounds=None,dataonly=False,transfercoordinatemask=True):
            dimnames = self.getDimensions_raw()
            shape = self.getShape()

            # Determine final slice
            if bounds is None: bounds = (Ellipsis,)
            newbounds = []
            for bound,dimlen,dimname in zip(xmlplot.common.processEllipsis(bounds,shape),shape,dimnames):
                if isinstance(bound,int):
                    # Integer value provided as index.
                    assert bound>=-dimlen, 'Slice index %i lies below the lowest possible index for dimension %s (%i).' % (bound,dimname,-dimlen  )
                    assert bound<  dimlen, 'Slice index %i exceeds the highest possible index for dimension %s (%i).'   % (bound,dimname, dimlen-1)
                    if bound<0: bound += dimlen
                elif isinstance(bound,slice):
                    start,stop,step = bound.indices(dimlen)
                    bound = slice(start,stop,step)
                newbounds.append(bound)
            bounds = tuple(newbounds)

            # Get data
            dat = numpy.asarray(self.hdfvar[bounds])

            # Mask fill value
            fillvalue = self.hdfvar.attributes().get('_FillValue',None)
            if fillvalue is None: fillvalue = self.hdfvar.attributes().get('Fill',None)
            if fillvalue is not None: dat = numpy.ma.array(dat,mask=(dat==fillvalue),copy=False)

            # Determine scale factor and offset, and cast data to acommodating type if needed.
            scale  = self.hdfvar.attributes().get('scale_factor',None)
            offset = self.hdfvar.attributes().get('add_offset',  None)
            if scale is not None or offset is not None and dat.dtype!=numpy.float:
                dat = dat.astype(numpy.float)
            if scale  is not None: dat *= scale
            if offset is not None: dat += offset

            if dataonly: return dat

            newdimnames = [d for d,b in zip(dimnames,bounds) if not isinstance(b,int)]
            varslice = self.Slice(newdimnames)
            varslice.data = dat
            inewdim = 0
            for dimname,bound in zip(dimnames,bounds):
                 # Get the coordinate variable          
                coordvar = self.store.getVariable_raw(dimname)
                
                if coordvar is None:
                    # No coordinate variable available: use indices
                    if not isinstance(bound,slice): continue
                    coorddims = [dimname]
                    coords = numpy.arange(bound.start,bound.stop,bound.step,dtype=numpy.float)
                else:
                    # Coordinate variable present: use it.
                    coorddims = list(coordvar.getDimensions())

                    # Debug check: see if all coordinate dimensions are also used by the variable.
                    for cd in coorddims:
                        assert cd in dimnames, 'Coordinate dimension %s is not used by this variable (it uses %s).' % (cd,', '.join(dimnames))

                    # Get coordinate values
                    coordslice = [bounds[dimnames.index(cd)] for cd in coorddims]
                    coords = coordvar.getSlice(coordslice, dataonly=True)

                # Get the list of coordinate dimensions after the ones with single index have been sliced out.
                newcoorddims = [cd for cd in coorddims if isinstance(bounds[dimnames.index(cd)],slice)]

                # Transfer the coordinate mask to the data if desired.
                coordmask = numpy.ma.getmask(coords)
                if transfercoordinatemask and coordmask is not numpy.ma.nomask:
                    coordmask = xmlplot.common.broadcastSelective(coordmask,newcoorddims,dat.shape,newdimnames)
                    if datamask is numpy.ma.nomask:
                        datamask = coordmask
                    else:
                        datamask |= coordmask

                # If we take a single index for this dimension, it will not be included in the output.
                if not isinstance(bound,slice): continue
                
                # Coordinates should not have a mask - undo the masking.
                if coordmask is not numpy.ma.nomask:
                    coords = numpy.ma.getdata(coords)

                # Auto-generate staggered coordinates
                coords_stag = xmlplot.common.stagger(coords)

                # Insert data dimensions where they are lacking in coordinate
                coords      = xmlplot.common.broadcastSelective(coords,     (dimname,),dat.shape,               newdimnames)
                coords_stag = xmlplot.common.broadcastSelective(coords_stag,(dimname,),[l+1 for l in dat.shape],newdimnames)

                # Assign coordinate values
                varslice.coords     [inewdim] = coords
                varslice.coords_stag[inewdim] = coords_stag

                inewdim += 1

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
        if varname not in self.file.datasets().keys(): return None
        return self.Variable(self,self.file.select(varname))

    def getVariableNames_raw(self):
        """Returns a list of original short names for all variables present in the store.
        The method must be implemented by derived classes.
        """
        return self.file.datasets().keys()

    def getProperties(self):
        return self.file.attributes()

    def unlink(self):
        self.file.end()