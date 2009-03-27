#!/usr/bin/python

import sys, os, os.path
import numpy

gotmguiroot = os.path.join(os.path.dirname(os.path.realpath(__file__)),'..')

path = sys.path[:] 
sys.path.append(gotmguiroot)
try: 
    import xmlplot.data
except Exception,e:
    print 'Unable to load xmlplot.data module. Reported error: %s.' % str(e) 
    sys.exit(1)

def extractncdata(path,varname,pathout=None,fix='',plot=False,verbose=True,debug=False):
    global xmlplot
        
    if plot:
        # Import GUI libraries
        from PyQt4 import QtGui,QtCore
        import xmlplot.gui_qt4
        
        # Create Qt4 application object if needed.
        createQApp = QtGui.QApplication.startingUp()
        if createQApp:
            app = QtGui.QApplication([' '])
        else:
            app = QtGui.qApp
            
        dialogs = []
    
    # Open the NetCDF file
    store = xmlplot.data.NetCDFStore.loadUnknownConvention(path)
    
    # Get the variable and its dimensions
    var = store.getVariable(varname)
    if var==None:
        print 'Variable %s was not found in NetCDF file. Available: %s.' % (varname,', '.join(store.getVariableNames()))
        return None
    dims = list(var.getDimensions())
    longname = var.getLongName()
    
    # Parse the string that specifies which dimensions to fix.
    slices = [slice(None)]*len(dims)
    fixeddiminds = []
    for assignment in fix.split(','):
        if assignment=='': continue
        dimname,value = assignment.split('=')
        value = float(value)
        if dimname not in dims:
            print 'Dimension %s is not used by variable %s. Available dimensions: %s. Exiting...' % (dimname,varname,', '.join(dims))
            return None
        idim = dims.index(dimname)
        fixeddiminds.append(idim)
        slices[idim] = slice(value,value)
        
    # Test if there is indeed only one dimension left after interpolation.
    dimsleft = [dims[i] for i in range(len(dims)) if i not in fixeddiminds]
    if len(dimsleft)>1:
        print 'More than one dimension is left (namely %s) after applying %s, but this script can currently extract only one-dimensional data series. Exiting...' % (', '.join(dimsleft),fix)
        return None
        
    # Before retrieving data, first select the range that encapsulates the requested coordinates.
    # Translate float-based slice specification to integer-based slice specification.
    newslices = var.translateSliceSpecification(slices)
    for i in fixeddiminds:
        dimname,coord,newcoord = dims[i],slices[i].start,newslices[i]
        if debug: print 'Variable %s: dimension %s = %.6g mapped to %s.' % (varname,dimname,coord,newcoord)
        
    # Get the data.
    if verbose: print 'Retrieving values for %s...' % longname
    varslice = var.getSlice(newslices)
    
    # Show current ranges for all dimensions
    if debug:
        for d,c in zip(varslice.dimensions,varslice.coords_stag):
            print '%s min=%.8g, max=%.8g' % (d,c.min(),c.max())
        print 'mean data value=%.8g' % (varslice.data.mean())
    
    # Linearly interpolate
    if verbose: print 'Linearly interpolating %s...' % longname
    dim2value = dict([(dims[i],slices[i].start) for i in fixeddiminds])
    varslice = varslice.interp(**dim2value).squeeze()
    diminfo = dict([(d,var.getDimensionInfo(d)) for d in varslice.dimensions])
    
    # Show current ranges for all dimensions
    if debug:
        for d,c in zip(varslice.dimensions,varslice.coords):
            print '%s min=%.8g, max=%.8g' % (d,c.min(),c.max())

    if plot:
        # Create a dialog with figure of the current variable.
        v = xmlplot.common.CustomVariable(varslice,name=varname,hasreverseddimensions=True,longname=longname,unit=var.getUnit(),dimensioninfo=diminfo)
        d = xmlplot.gui_qt4.FigureDialog(quitonclose=True)
        fig = d.panel.figure
        fig.source.addChild(v)
        fig.addVariable(varname)
        d.setWindowTitle(longname)
        d.show()
        dialogs.append(d)
        
    if pathout!=None:
        # Create observations file
        mat = xmlplot.data.LinkedMatrix(dimensions=diminfo,dimensionorder=varslice.dimensions,variables=[(varname,longname,var.getUnit())])
        mat.setData([varslice.coords[0],varslice.data.reshape((-1,1))])
        if verbose: print 'Saving %s to %s...' % (longname,pathout)
        mat.saveToFile(pathout)

    # Release the NetCDF file.
    store.unlink()
        
    if plot and createQApp:
        # Enter the main message loop.
        ret = app.exec_()
        
    return (varslice.coords[0],varslice.data.reshape((-1,1)))

if (__name__=='__main__'):
    import optparse
    parser = optparse.OptionParser()
    parser.add_option('-p','--plot',action='store_true',help='plot the extracted data series on-screen')
    parser.add_option('-q','--quiet',action='store_false',dest='verbose',help='suppress progress messages')
    parser.add_option('-d','--debug',action='store_true',help='show debug information')
    parser.add_option('-f','--fix',type='string',help='comma-separated list of dimension=value items, specifying which coordinates to fix')
    parser.set_defaults(plot=False,verbose=True,debug=False,fix='')
    (options, args) = parser.parse_args()
    
    # Get required arguments
    if len(args)<2:
        print 'At least 2 arguments must be provided: the path to the NetCDF file and the name of the NetCDF variable. Additionally you can provide the file to write the data to as third argument.'
        sys.exit(1)
    path,varname = args[:2]
    pathout = None
    if len(args)>3: pathout = args[2]

    ret = extractncdata(path,varname,pathout,fix=options.fix,plot=options.plot,verbose=options.verbose,debug=options.debug)
    if ret==None: sys.exit(1)
    sys.exit(0)

def extractncdata_parts(paths,*args,**kwargs):
    coords,data = [],[]
    basepath = kwargs.pop('basepath','')
    for i,path in enumerate(paths):
        res = extractncdata(os.path.join(basepath,path),*args,**kwargs)
        if res==None: return None
        c,d = res
        c.shape = (-1,)
        d.shape = (-1,)
        if coords:
            ilast = coords[-1].searchsorted(c[0])
            print 'Overlap between files %s and %i is %i rows.' % (i-1,i,len(coords[-1])-ilast)
            coords[-1] = coords[-1][:ilast]
            data  [-1] = data  [-1][:ilast]
        coords.append(c)
        data.append(d)
    return numpy.concatenate(coords),numpy.concatenate(data)