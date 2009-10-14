# Current GOTM/namelist version used by (1) the GUI, (2) saved GUI scenario files.
# Currently (2) differs from (1) because (1) is still in development, while saved files must use a frozen
# scenario version in order to be usable later too.
guiscenarioversion = 'gotmgui-0.5.0'
savedscenarioversion = 'gotm-4.0.0'

# Import modules from standard Python library
import os, shutil, re, datetime, sys

# Import our own custom modules
import xmlstore.xmlstore, xmlstore.util
import common, namelist

# Some parts of the schemas will be loaded from the GOTM source directory.
# For the developer's version, the source directory can be found one level below the GUI.
# For the frozen version (py2exe build), the required files are present in the gotmsrc
# subdirectory directly below the GUI directory.
if hasattr(sys,'frozen'):
    srcdir = os.path.abspath(os.path.join(common.getDataRoot(),'gotmsrc'))
else:
    srcdir = os.path.abspath(os.path.join(common.getDataRoot(),'../src'))
if os.path.isdir(srcdir): xmlstore.xmlstore.Schema.knownpaths['gotmsrc'] = srcdir

class NamelistStore(xmlstore.xmlstore.TypedStore):

    def __init__(self,*args,**kwargs):
        xmlstore.xmlstore.TypedStore.__init__(self,*args,**kwargs)

        self.namelistextension = self.root.templatenode.getAttribute('namelistextension')

    @classmethod
    def fromNamelists(cls,path,protodir=None,targetversion=None,strict = False,requireplatform=None):
        # Get a list of available schema versions and check if the target version is available.
        sourceids = cls.getSchemaInfo().getSchemas().keys()
        if targetversion is not None and targetversion not in sourceids:
            raise Exception('No schema available for desired version "%s".' % targetversion)
            
        # Rank the source versions according to correspondence with target version and version number (higher is better).
        sourceids = cls.rankSources(sourceids,targetversion,requireplatform=requireplatform)
        
        # Try the available schemas one by one, and see if they match the namelists.
        scenario = None
        failures = ''
        for sourceid in sourceids:
            if common.verbose: print 'Trying scenario format "%s"...' % sourceid
            scenario = cls.fromSchemaName(sourceid)
            try:
                scenario.loadFromNamelists(path,strict=strict,protodir=protodir)
            except namelist.NamelistParseException,e:
                failures += 'Path "%s" does not match template "%s".\nReason: %s\n' % (path,sourceid,e)
                scenario.release()
                scenario = None
            if scenario is not None:
                #print 'Path "'+path+'" matches template "'+template+'".'
                break
                
        # Check if we found a schema that matches the namelists.
        if scenario is None:
            raise Exception('The path "%s" does not contain a supported scenario. Details:\n%s' % (path,failures))
            
        # Convert the store to the desired version, if specified.
        if targetversion is not None and scenario.version!=targetversion:
            newscenario = scenario.convert(targetversion)
            scenario.release()
            return newscenario
        else:
            return scenario

    def loadFromNamelists(self, srcpath, strict=False, protodir=None):
        if common.verbose: print 'Importing scenario from namelist files...'

        # Try to open the specified path (currently can be zip, tar/gz or a directory)
        try:
            container = xmlstore.datatypes.DataContainer.fromPath(srcpath)
        except Exception,e:
            raise Exception('Unable to load specified path. ' + unicode(e))

        # Start with empty scenario
        self.setStore(None)

        globalsubs = []
        if protodir is not None:
            # Namelist are specified as .proto files plus one or more .values files.
            # Load the substitutions specified in the main .values file.
            nmlcontainer = xmlstore.datatypes.DataContainerDirectory(protodir)
            df = container.getItem(os.path.basename(srcpath)+'.values')
            df_file = df.getAsReadOnlyFile()
            globalsubs.append(namelist.NamelistSubstitutions(df_file))
            df_file.close()
            df.release()
        else:
            nmlcontainer = container.addref()

        # Build a list of files in the namelist directory
        # (these are the same, unless prototype namelist files are used)
        nmlfilelist = nmlcontainer.listFiles()
        datafilecontext = {'container':container}
        
        interface = self.getInterface(omitgroupers=True,interfacetype='nml')

        try:
            for mainchild in interface.getChildren(self.root):
                # If we are using prototypes, all namelist files are available, but not all contain
                # values; then, just skip namelist files that are disabled by settings in the preceding
                # namelists.
                if protodir is not None and mainchild.isHidden(): continue
                
                # Get name (excl. extension) for the namelist file.
                nmlfilename = mainchild.getId()

                assert not mainchild.canHaveValue(), 'Found non-folder node with id %s below root, where only folders are expected.' % nmlfilename

                cursubs = globalsubs
                if protodir is None:
                    # Normal namelist file
                    ext = self.namelistextension
                    if mainchild.templatenode.hasAttribute('namelistextension'):
                        ext = mainchild.templatenode.getAttribute('namelistextension')
                    fullnmlfilename = nmlfilename+ext
                else:
                    # Prototype namelist in which values will be substituted.
                    fullnmlfilename = nmlfilename+'.proto'

                    # Load the relevant value substitutions (if any).
                    df = container.getItem(nmlfilename+'.values')
                    if df is not None:
                        df_file = df.getAsReadOnlyFile()
                        cursubs = [namelist.NamelistSubstitutions(df_file)]
                        df_file.close()
                        df.release()

                # Find and parse the namelist file.
                for fn in nmlfilelist:
                    if fn==fullnmlfilename or fn.endswith('/'+fullnmlfilename):
                        fullnmlfilename = fn
                        break
                else:
                    if mainchild.templatenode.getAttribute('optional')=='True':
                        # This namelist file is missing but not required. Use default values and continue
                        if self.defaultstore is not None:
                            mainchild.copyFrom(self.defaultstore.mapForeignNode(mainchild))
                        continue
                    elif mainchild.isHidden():
                        # This namelist file is missing but will not be used. Thus no worries: continue
                        continue
                    else:
                        raise namelist.NamelistParseException('Namelist file "%s" is not present.' % fullnmlfilename,None,None,None)
                        
                # Obtain the namelist file, open it, parse it, and close it.
                df = nmlcontainer.getItem(fullnmlfilename)
                df_file = df.getAsReadOnlyFile()
                nmlfile = namelist.NamelistFile(df_file,cursubs)
                df_file.close()
                df.release()

                # Loop over all nodes below the root (each node represents a namelist file)
                for filechild in interface.getChildren(mainchild):
                    # Get name of the expected namelist.
                    listname = filechild.getId()
                    
                    # Get a list with all child nodes (i.e., namelist variables)
                    listchildren = interface.getChildren(filechild)

                    assert not filechild.canHaveValue(), 'Found non-folder node with id %s below branch %s, where only folders are expected.' % (listname,nmlfilename)

                    # Parse the next namelist.
                    nmlist = nmlfile.parseNextNamelist(expectedlist=listname)

                    # Index of next expected child node [used only in "strict" parsing mode]
                    childindex = 0

                    for (foundvarname,slic,vardata) in nmlist:

                        if strict:
                            # Strict parsing: all variables must appear once and in predefined order.
                            if childindex>=len(listchildren):
                                raise namelist.NamelistParseException('Encountered variable "%s" where end of namelist was expected.' % (foundvarname,),fullnmlfilename,listname,None)
                            listchild = listchildren[childindex]
                            varname = listchild.getId()
                            if varname.lower()!=foundvarname.lower():
                                raise namelist.NamelistParseException('Found variable "%s" where "%s" was expected.' % (foundvarname,varname),fullnmlfilename,listname,varname)
                            childindex += 1
                        else:
                            # Loose parsing: variables can appear multiple times or not at all, and do not need to appear in order.
                            # This is how FORTRAN operates.
                            for listchild in listchildren:
                                varname = listchild.getId()
                                if varname.lower()==foundvarname.lower(): break
                            else:
                                raise namelist.NamelistParseException('Encountered variable "%s", which should not be present in this namelist.' % (foundvarname,),fullnmlfilename,listname,varname)
                                
                        # If no value was provided, skip to the next assignment.
                        if vardata is None: continue

                        # Retrieve the value (in the correct data type) from the namelist string.
                        vartype = listchild.getValueType(returnclass=True)
                        if slic is None:
                            # No slice specification - assign to entire variable.
                            try:
                                val = vartype.fromNamelistString(vardata,datafilecontext,listchild.templatenode)
                            except Exception,e:
                                raise namelist.NamelistParseException('%s Variable data: %s' % (e,vardata),fullnmlfilename,listname,varname)
                        else:
                            # Slice specification provided - assign to subset of variable.
                            val = listchild.getValue()
                            if val is None: val = vartype(template=listchild.templatenode)
                            val.setItemFromNamelist(slic,vardata,datafilecontext,listchild.templatenode)

                        # Transfer the value to the store.
                        listchild.setValue(val)
                        
                        # Release the value object.
                        if isinstance(val,xmlstore.util.referencedobject):
                            val.release()
                        
                    # If we are in "strict" mode, check if there are any remaining variables that were not assigned to.
                    if strict and childindex<len(listchildren):
                        lcnames = ['"%s"' % lc.getId() for lc in listchildren[childindex:]]
                        raise namelist.NamelistParseException('Variables %s are missing' % ', '.join(lcnames),fullnmlfilename,listname,None)
        finally:
            container.release()
            nmlcontainer.release()
            self.disconnectInterface(interface)
            if 'linkedobjects' in datafilecontext:
                for v in datafilecontext['linkedobjects'].itervalues():
                    v.release()

    def writeAsNamelists(self, targetpath, copydatafiles=True, addcomments=False, allowmissingvalues=False, callback=None):
        if common.verbose: print 'Exporting scenario to namelist files...'

        # If the directory to write to does not exist, create it.
        createddir = False
        if (not os.path.isdir(targetpath)):
            try:
                os.mkdir(targetpath)
                createddir = True
            except Exception,e:
                raise Exception('Unable to create target directory "%s". Error: %s' %(targetpath,str(e)))
                
        context = {}
        if copydatafiles:
            context['targetcontainer'] = xmlstore.datatypes.DataContainerDirectory(targetpath)
            
        def encode_error_handler(exc):
            assert isinstance(exc, UnicodeEncodeError), 'do not know how to handle %r' % exc
            l = []
            for c in exc.object[exc.start:exc.end]:
                l.append(xmlstore.util.unicodechar2ascii(c))
            return (u', '.join(l), exc.end)

        import codecs
        codecs.register_error('namelist', encode_error_handler)

        interface = self.getInterface(omitgroupers=True,interfacetype='nml')
        try:
            try:
                if addcomments:
                    # Import and configure text wrapping utility.
                    import textwrap
                    linelength = 80
                    wrapper = textwrap.TextWrapper(subsequent_indent='  ')
                
                rootchildren = interface.getChildren(self.root)
                progslicer = xmlstore.util.ProgressSlicer(callback,len(rootchildren))
                for mainchild in rootchildren:
                    assert not mainchild.canHaveValue(), 'Found a variable below the root node, where only folders are expected.'

                    nmlfilename = mainchild.getId()
                    progslicer.nextStep(nmlfilename)

                    if mainchild.isHidden(): continue

                    # Create the namelist file.
                    ext = self.namelistextension
                    if mainchild.templatenode.hasAttribute('namelistextension'):
                        ext = mainchild.templatenode.getAttribute('namelistextension')
                    nmlfilepath = os.path.join(targetpath, nmlfilename+ext)
                    nmlfile = open(nmlfilepath,'w')

                    try:
                        for filechild in interface.getChildren(mainchild):
                            assert not filechild.canHaveValue(), 'Found a variable directly below branch "%s", where only folders are expected.' % nmlfilename
                            listname = filechild.getId()
                            listchildren = interface.getChildren(filechild)

                            if addcomments:
                                nmlfile.write('!'+(linelength-1)*'-'+'\n')
                                title = filechild.getText(detail=2).encode('ascii','namelist')
                                nmlfile.write(textwrap.fill(title,linelength-2,initial_indent='! ',subsequent_indent='! '))
                                nmlfile.write('\n!'+(linelength-1)*'-'+'\n')

                                comments = []
                                varnamelength = 0
                                for listchild in listchildren:
                                    comment = self.getNamelistVariableDescription(listchild)
                                    if len(comment[0])>varnamelength: varnamelength = len(comment[0])
                                    comments.append(comment)
                                wrapper.width = linelength-varnamelength-5
                                for (varid,vartype,lines) in comments:
                                    wrappedlines = []
                                    lines.insert(0,'['+vartype+']')
                                    for line in lines:
                                        line = line.encode('ascii','namelist')
                                        wrappedlines += wrapper.wrap(line)
                                    firstline = wrappedlines.pop(0)
                                    nmlfile.write('! %-*s %s\n' % (varnamelength,varid,firstline))
                                    for line in wrappedlines:
                                        nmlfile.write('! '+varnamelength*' '+'   '+line+'\n')
                                if len(comments)>0:
                                    nmlfile.write('!'+(linelength-1)*'-'+'\n')
                                nmlfile.write('\n')

                            nmlfile.write('&'+listname+'\n')
                            for listchild in listchildren:
                                if listchild.hasChildren():
                                    raise Exception('Found a folder ("%s") below branch %s/%s, where only variables are expected.' % (listchild.getId(),nmlfilename,listname))
                                varname = listchild.getId()
                                varval = listchild.getValue(usedefault=True)
                                if varval is None:
                                    # If the variable value is not set while its node is hidden,
                                    # the variable will not be used, and we skip it silently.
                                    if allowmissingvalues or listchild.isHidden(): continue
                                    raise Exception('Value for variable "%s" in namelist "%s" not set.' % (varname,listname))
                                varstring = varval.toNamelistString(context,listchild.templatenode)
                                if isinstance(varstring,(list,tuple)):
                                    for ind,value in varstring:
                                        nmlfile.write('   %s(%s) = %s,\n' % (varname,ind,value.encode('ascii','namelist')))
                                else:
                                    nmlfile.write('   %s = %s,\n' % (varname,varstring.encode('ascii','namelist')))
                                if isinstance(varval,xmlstore.util.referencedobject): varval.release()
                            nmlfile.write('/\n\n')
                    finally:
                        nmlfile.close()
            except:
                if createddir: shutil.rmtree(targetpath)
                raise
        finally:
            self.disconnectInterface(interface)
            if 'targetcontainer' in context: context['targetcontainer'].release()

    @staticmethod
    def getNamelistVariableDescription(node):
        varid = node.getId()
        datatype = node.getValueType()
        description = node.getText(detail=2)
        lines = [description]
        
        if node.templatenode.hasAttribute('hasoptions'):
            # Create list of options.
            options = xmlstore.util.findDescendantNode(node.templatenode,['options'])
            assert options is not None, 'Node is of type "select" but lacks "options" childnode.'
            for ch in options.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                    lab = ch.getAttribute('description')
                    if lab=='': lab = ch.getAttribute('label')
                    lines.append(ch.getAttribute('value')+': '+lab)

        # Create description of data type and range.
        isarray = datatype.startswith('array(') and datatype.endswith(')')
        if isarray: datatype = datatype[6:-1]
        if datatype=='gotmdatafile':
            datatype = 'file path'
        elif datatype=='int':
            datatype = 'integer'
        elif datatype=='datetime':
            datatype = 'string, format = "yyyy-mm-dd hh:mm:ss"'
        if isarray:
            datatype += ' array'
            if node.templatenode.hasAttribute('shape'):
                datatype += ' with shape (%s)' % node.templatenode.getAttribute('shape')
        if node.templatenode.hasAttribute('minInclusive'):
            datatype += ', minimum = ' + node.templatenode.getAttribute('minInclusive')
        if node.templatenode.hasAttribute('maxInclusive'):
            datatype += ', maximum = ' + node.templatenode.getAttribute('maxInclusive')
        unit = node.getUnit()
        if unit is not None:
            datatype += ', unit = ' + unit

        # Get description of conditions (if any).
        condition = xmlstore.util.findDescendantNode(node.templatenode,['condition'])
        if condition is not None:
            condline = NamelistStore.getNamelistConditionDescription(condition)
            lines.append('This variable is used only if '+condline)

        return (varid,datatype,lines)

    @staticmethod
    def getNamelistConditionDescription(node):
        condtype = node.getAttribute('type')
        if condtype=='eq' or condtype=='ne':
            var = node.getAttribute('variable')
            val = node.getAttribute('value')
            if var.startswith('./'): var=var[2:]
            if condtype=='eq':
                return var+' = '+val
            else:
                return var+' != '+val
        elif condtype=='and' or condtype=='or':
            conds = xmlstore.util.findDescendantNodes(node,['condition'])
            conddescs = map(NamelistStore.getNamelistConditionDescription,conds)
            return '('+(' '+condtype+' ').join(conddescs)+')'
        else:
            raise Exception('Unknown condition type "%s".' % condtype)
            
class Scenario(NamelistStore):

    # Name to be used for the main XML file if the store is saved to an archive.
    storefilename = 'scenario.xml'
    
    # Descriptive name for the store to be used when communicating with the user.
    storetitle = 'GOTM scenario'

    @classmethod
    def getSchemaInfo(cls):
        return xmlstore.xmlstore.schemainfocache[os.path.join(common.getDataRoot(),'schemas/scenario')]

    def __init__(self,schema,valueroot=None,adddefault = True):
        NamelistStore.__init__(self,schema,valueroot,adddefault=adddefault)

    @classmethod
    def getCustomDataTypes(ownclass):
        dt = xmlstore.xmlstore.TypedStore.getCustomDataTypes()
        import xmlplot.data
        dt['gotmdatafile'] = xmlplot.data.LinkedFileVariableStore
        return dt

    def load(self,path):
        xmlstore.xmlstore.TypedStore.load(self,path)

        # If the scenario was stored in the official 'save' version, we should not consider it changed.
        # (even though we had to convert it to the 'display' version). Therefore, reset the 'changed' status.
        if self.originalversion==savedscenarioversion: self.resetChanged()

    def saveAll(self,path,targetversion=None,*args,**kwargs):
        if targetversion is None: targetversion = savedscenarioversion
        kwargs['fillmissing'] = True
        xmlstore.xmlstore.TypedStore.saveAll(self,path,targetversion=targetversion,*args,**kwargs)

    def loadAll(self,path,*args,**kwargs):
        xmlstore.xmlstore.TypedStore.loadAll(self,path,*args,**kwargs)

        # If the scenario was stored in the official 'save' version, we should not consider it changed.
        # (even though we had to convert it to the 'display' version). Therefore, reset the 'changed' status.
        if self.originalversion==savedscenarioversion: self.resetChanged()
        
    def _validate(self,nodes,usedefault=True,validatedatafiles=True,callback=None,repair=0,usehistory=True):
        # Call base implementation of validate.
        errors,validity = xmlstore.xmlstore.TypedStore._validate(self,nodes,usedefault=usedefault,repair=repair,callback=callback,usehistory=usehistory)
        
        # We only know how to validate one scenario version;
        # return base result if version does not match.
        if self.version!='gotmgui-0.5.0': return errors,validity

        # Retrieve validation history (this is a set containing the nodes that
        # have been found valid in previous calls to "validate")
        if usehistory:
            oldvalids = self.validnodes
        else:
            oldvalids = set()

        # Validate the time range
        startnode,stopnode = self['/time/start'],self['/time/stop']
        if validity.get(startnode,False) and validity.get(stopnode,False):
            start = startnode.getValue(usedefault=usedefault)
            stop = stopnode.getValue(usedefault=usedefault)
            if start is not None and stop is not None and start>=stop:
                validity[startnode] = False
                validity[stopnode ] = False
                if start>stop:
                    errors.append('The end of the simulated period lies before its beginning.')
                elif start==stop:
                    errors.append('The begin and end time of the simulated period are equal.')

        # Validate the time step for calculations and output
        dtnode,dtsavenode = self['/timeintegration/dt'],self['/output/dtsave']
        if validity.get(dtnode,False) and validity.get(dtsavenode,False):
            dt = dtnode.getValue(usedefault=usedefault)
            dtsave = dtsavenode.getValue(usedefault=usedefault)
            if dt>dtsave:
                validity[dtnode]     = False
                validity[dtsavenode] = False
                errors.append('The time step for calculations must be smaller than the output resolution.')

        # Validate the sum of custom layer thicknesses.
        gridmethod = self['/grid/grid_method'].getValue(usedefault=usedefault)
        if gridmethod==1:
            # Sigma grid: validate sum of thicknesses only.
            gridfilenode = self['/grid/grid_file']
            if validity.get(gridfilenode,False):
                val = gridfilenode.getValue(usedefault=usedefault)
                depth = val.getData()[0].sum()
                if abs(depth-1.)>1.e-8:
                    errors.append('The sum of the relative layer thicknesses must equal 1, but it currently equals %.6g.' % depth)
                val.release()
        elif gridmethod==2:
            # Cartesian grid: validate sum of thicknesses in combination with column depth.
            gridfilenode,depthnode = self['/grid/grid_file'],self['/station/depth']
            if ((gridfilenode in oldvalids and validity.get(depthnode,   False)) or
                (depthnode    in oldvalids and validity.get(gridfilenode,False)) or
                (validity.get(gridfilenode,False) and validity.get(depthnode,False))):
                val = gridfilenode.getValue(usedefault=usedefault)
                if abs(val.getData()[0].sum()-depthnode.getValue(usedefault=usedefault))>1.e-5:
                    errors.append('The sum of the custom layer thicknesses does not match the specified column depth.')
                val.release()
                
        # Validate the short-wave radiation method, given the (lack of) use of
        # meteorological data
        fluxsourcenode,swrmethodnode = self['/airsea/flux_source'],self['/airsea/swr_method']
        if ((fluxsourcenode in oldvalids and validity.get(swrmethodnode,False)) or
            (swrmethodnode  in oldvalids and validity.get(fluxsourcenode,False)) or
            (validity.get(fluxsourcenode,False) and validity.get(swrmethodnode,False))):
            fluxsource = fluxsourcenode.getValue(usedefault=usedefault)
            swrmethod = swrmethodnode.getValue(usedefault=usedefault)
            if fluxsource!=0 and swrmethod==3:
                if fluxsourcenode in validity: validity[fluxsourcenode] = False
                if swrmethodnode  in validity: validity[swrmethodnode]  = False
                errors.append('The short-wave radiation cannot be calculated from location, time and cloud cover because you do not provide meteorological observations.')

        # Validate the time extent of input data series, provided
        # the end of the simulation has been set.
        if validatedatafiles and (stopnode in oldvalids or validity.get(stopnode,False)):
            stop = stopnode.getValue(usedefault=usedefault)
            if stop is not None:
                for node in nodes:
                    if node.getValueType()!='gotmdatafile' or node.isHidden() or not validity[node]:
                        continue
                    value = node.getValue(usedefault=usedefault)
                    if value.validate(node.templatenode):
                        for dimname in value.getDimensionNames():
                            if value.getDimensionInfo(dimname)['datatype']=='datetime':
                                dimrange = value.getDimensionRange(dimname)
                                if dimrange is None:
                                    validity[node] = False
                                    errors.append('Data series "%s" does not contain any value.' % (node.getText(detail=1),))
                                    continue
                                mintime,maxtime = dimrange
                                if stop>maxtime and maxtime!=mintime:
                                    validity[node] = False
                                    errors.append('Data series "%s" finishes at %s, before the simulation is set to end (%s).' % (node.getText(detail=1),xmlstore.util.formatDateTime(maxtime),xmlstore.util.formatDateTime(stop)))
                    value.release()
                
        return errors,validity
