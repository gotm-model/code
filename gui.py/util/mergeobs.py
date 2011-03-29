#!/usr/bin/python

import sys, os, os.path
import numpy

gotmguiroot = os.path.join(os.path.dirname(os.path.realpath(__file__)),'..')
sys.path.append(gotmguiroot)
try: 
    import xmlstore.util,xmlplot.common,xmlplot.data
except Exception,e:
    print 'Unable to load xmlplot.data module. Reported error: %s.' % str(e) 
    sys.exit(1)

def main():
    import optparse
    global xmlplot
    
    parser = optparse.OptionParser()
    parser.add_option('-q','--quiet',action='store_false',dest='verbose',help='suppress progress messages')
    parser.set_defaults(verbose=True)
    (options, args) = parser.parse_args()
    
    pathout = args.pop(-1)
    
    minc,maxc,data,vars = None,None,[],[]
    for path in args:
        varname = os.path.splitext(os.path.basename(path))[0]
        dimname = 'dimension1'
        diminfo = {dimname:{'datatype':'datetime'}}
        vars.append((varname,varname,''))
    
        # Create observations file
        mat = xmlplot.data.LinkedMatrix(dimensions=diminfo,dimensionorder=(dimname,),variables=[vars[-1]])
        if options.verbose: print 'Reading %s...' % (path)
        mat.loadFromFile(path)
        curdata = mat.getData()
        
        data.append(curdata)
        
        curmin,curmax = curdata[0].min(),curdata[0].max()
        if minc is None or curmin>minc: minc = curmin
        if maxc is None or curmax<maxc: maxc = curmax
        
        if options.verbose:
            if diminfo[dimname]['datatype']=='datetime':
                if diminfo[dimname]['datatype']=='datetime': curmin,curmax = xmlplot.common.num2date(curmin),xmlplot.common.num2date(curmax)
                print '"%s" ranges between %s and %s.' % (varname,xmlstore.util.formatDateTime(curmin),xmlstore.util.formatDateTime(curmax))
            else:
                print '"%s" ranges between %.8g and %.8g.' % (varname,curmin,curmax)
        
    if maxc<minc:
        print 'The data series from the different files do not overlap. Unable to combine them into a usable observation file.'
        sys.exit(1)
        
    coords = numpy.unique(numpy.concatenate([d[0] for d in data],0))
    coords.sort()
    newdata = numpy.empty((coords.shape[0],sum([d[1].shape[1] for d in data])),numpy.float)
    pos = 0
    for d in data:
        newdata[:,pos:pos+d[1].shape[1]] = xmlplot.common.interp1(d[0],d[1],coords)
        pos += d[1].shape[1]

    mat = xmlplot.data.LinkedMatrix(dimensions=diminfo,dimensionorder=(dimname,),variables=vars)
    mat.setData([coords,newdata])
    if options.verbose: print 'Saving %s...' % (pathout,)
    mat.saveToFile(pathout)
        
    return 0

if (__name__=='__main__'):
    ret = main()
    sys.exit(ret)
