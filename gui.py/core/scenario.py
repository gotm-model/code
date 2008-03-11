# Current GOTM/namelist version used by (1) the GUI, (2) saved GUI scenario files.
# Currently (2) differs from (1) because (1) is still in development, while saved files must use a frozen
# scenario version in order to be usable later too.
guiscenarioversion = 'gotmgui-0.5.0'
savedscenarioversion = 'gotm-4.0.0'

# Import modules from standard Python library
import os, shutil, re, datetime

# Import our own custom modules
import xmlstore.xmlstore, xmlstore.util, xmlplot.data, xmlplot.common
import common, namelist

class Scenario(xmlstore.xmlstore.TypedStore):

    # Name to be used for the main XML file if the store is saved to an archive.
    storefilename = 'scenario.xml'
    
    # Descriptive name for the store to be used when communicating with the user.
    storetitle = 'GOTM scenario'

    def __init__(self,schemadom,valueroot=None,adddefault = True):
        xmlstore.xmlstore.TypedStore.__init__(self,schemadom,valueroot,adddefault=adddefault)

        self.namelistextension = self.root.templatenode.getAttribute('namelistextension')

    schemadict = None
    @staticmethod
    def getDefaultSchemas():
        if Scenario.schemadict==None:
            Scenario.schemadict = xmlstore.xmlstore.ShortcutDictionary.fromDirectory(os.path.join(common.getDataRoot(),'schemas/scenario'))
        return Scenario.schemadict

    defaultdict = None
    @staticmethod
    def getDefaultValues():
        if Scenario.defaultdict==None:
            Scenario.defaultdict = xmlstore.xmlstore.ShortcutDictionary.fromDirectory(os.path.join(common.getDataRoot(),'defaultscenarios'))
        return Scenario.defaultdict

    @staticmethod
    def fromNamelists(path,protodir=None,targetversion=None,strict = True):
        if targetversion==None: targetversion=guiscenarioversion
        
        sourceids = Scenario.rankSources(targetversion,Scenario.getDefaultSchemas().keys(),requireplatform='gotm')
        scenario = None
        failures = ''
        for sourceid in sourceids:
            if common.verbose: print 'Trying scenario format "%s"...' % sourceid
            scenario = Scenario.fromSchemaName(sourceid)
            try:
                scenario.loadFromNamelists(path,strict=strict,protodir=protodir)
            except namelist.NamelistParseException,e:
                failures += 'Path "%s" does not match template "%s".\nReason: %s\n' % (path,sourceid,e)
                scenario.release()
                scenario = None
            if scenario!=None:
                #print 'Path "'+path+'" matches template "'+template+'".'
                break
        if scenario==None:
            raise Exception('The path "%s" does not contain a supported GOTM scenario. Details:\n%s' % (path,failures))
        if scenario.version!=targetversion:
            newscenario = scenario.convert(targetversion)
            scenario.release()
            return newscenario
        else:
            return scenario

    @classmethod
    def getCustomTypes(ownclass):
        return {'gotmdatafile':xmlplot.data.LinkedFileVariableStore}

    def loadFromNamelists(self, srcpath, strict = False, protodir = None):
        if common.verbose: print 'Importing scenario from namelist files...'

        # Try to open the specified path (currently can be zip, tar/gz or a directory)
        try:
            container = xmlstore.xmlstore.DataContainer.fromPath(srcpath)
        except Exception,e:
            raise Exception('Unable to load specified path. ' + unicode(e))

        # Start with empty scenario
        self.setStore(None)

        globalsubs = []
        if protodir!=None:
            # Namelist are specified as .proto files plus one or more .values files.
            # Load the substitutions specified in the main .values file.
            nmlcontainer = xmlstore.xmlstore.DataContainerDirectory(protodir)
            df = container.getItem(os.path.basename(srcpath)+'.values')
            df_file = df.getAsReadOnlyFile()
            globalsubs.append(namelist.NamelistSubstitutions(df_file))
            df_file.close()
            df.release()
        else:
            nmlcontainer = container.addref()

        # Build a list of files in the namelist directory and the data directory
        # (these are the same, unless prototype namelist files are used)
        filelist = container.listFiles()
        nmlfilelist = nmlcontainer.listFiles()
        datafilecontext = {'container':container}

        # Commonly used regular expressions (for parsing strings and datetimes).
        strre = re.compile('^([\'"])(.*?)\\1$')
        datetimere = re.compile('(\d\d\d\d)[/\-](\d\d)[/\-](\d\d) (\d\d):(\d\d):(\d\d)')

        try:
            for mainchild in self.root.children:
                # If we are using prototypes, all namelist files are available, but not all contain
                # values; then, just skip namelist files that are disabled by settings in the preceding
                # namelists.
                if protodir!=None and mainchild.isHidden(): continue
                
                # Get name (excl. extension) for the namelist file.
                nmlfilename = mainchild.getId()

                assert not mainchild.canHaveValue(), 'Found non-folder node with id %s below root, where only folders are expected.' % nmlfilename

                cursubs = globalsubs
                if protodir==None:
                    # Normal namelist file
                    fullnmlfilename = nmlfilename+self.namelistextension
                else:
                    # Prototype namelist in which values will be substituted.
                    fullnmlfilename = nmlfilename+'.proto'

                    # Load the relevant value substitutions (if any).
                    df = container.getItem(nmlfilename+'.values')
                    if df!=None:
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
                    if mainchild.templatenode.hasAttribute('optional'):
                        # This namelist file is missing but not required. Use default values and continue
                        if self.defaultstore!=None:
                            mainchild.copyFrom(self.defaultstore.mapForeignNode(mainchild))
                        continue
                    elif mainchild.isHidden():
                        # This namelist file is missing but will not be used. Thus no worries: continue
                        continue
                    else:
                        raise namelist.NamelistParseException('Namelist file "%s" is not present.' % fullnmlfilename,None,None,None)
                df = nmlcontainer.getItem(fullnmlfilename)
                df_file = df.getAsReadOnlyFile()
                nmlfile = namelist.NamelistFile(df_file,cursubs)
                df_file.close()
                df.release()

                # Loop over all nodes below the root (each node represents a namelist file)
                for filechild in mainchild.children:
                    # Get name of the expected namelist.
                    listname = filechild.getId()

                    assert not filechild.canHaveValue(), 'Found non-folder node with id %s below branch %s, where only folders are expected.' % (listname,nmlfilename)

                    # Parse the next namelist.
                    nmlist = nmlfile.parseNextNamelist(expectedlist=listname)

                    childindex = 0

                    for (foundvarname,vardata) in nmlist:

                        if strict:
                            # Strict parsing: all variables must appear once and in predefined order.
                            if childindex>=len(filechild.children):
                                raise namelist.NamelistParseException('Encountered variable "%s" where end of namelist was expected.' % (foundvarname,),fullnmlfilename,listname,None)
                            listchild = filechild.children[childindex]
                            varname = listchild.getId()
                            if varname.lower()!=foundvarname.lower():
                                raise namelist.NamelistParseException('Found variable "%s" where "%s" was expected.' % (foundvarname,varname),fullnmlfilename,listname,varname)
                            childindex += 1
                        else:
                            # Loose parsing: variables can appear multiple times or not at all, and do not need to appear in order.
                            for listchild in filechild.children:
                                varname = listchild.getId()
                                if varname.lower()==foundvarname.lower(): break
                            else:
                                raise namelist.NamelistParseException('Encountered variable "%s", which should not be present in this namelist.' % (foundvarname,),fullnmlfilename,listname,varname)

                        vartype = listchild.getValueType()

                        if vartype=='string' or vartype=='datetime' or vartype=='gotmdatafile':
                            strmatch = strre.match(vardata)
                            if strmatch==None:
                                raise namelist.NamelistParseException('Variable is not a string. Data: %s' % vardata,fullnmlfilename,listname,varname)
                            val = strmatch.group(2)
                        elif vartype=='int':
                            try:
                                val = int(vardata)
                            except ValueError:
                                raise namelist.NamelistParseException('Variable is not an integer. Data: "%s"' % vardata,fullnmlfilename,listname,varname)
                        elif vartype=='float':
                            try:
                                val = float(vardata)
                            except ValueError:
                                raise namelist.NamelistParseException('Variable is not a floating point value. Data: "%s"' % vardata,fullnmlfilename,listname,varname)
                        elif vartype=='bool':
                            if   vardata[0].lower()=='f' or vardata[0:2].lower()=='.f':
                                val = False
                            elif vardata[0].lower()=='t' or vardata[0:2].lower()=='.t':
                                val = True
                            else:
                                raise namelist.NamelistParseException('Variable is not a boolean. Data: "%s"' % vardata,fullnmlfilename,listname,varname)
                        elif vartype=='select':
                            try:
                                val = int(vardata)
                            except ValueError:
                                raise namelist.NamelistParseException('Variable is not an integer. Data: "%s"' % vardata,fullnmlfilename,listname,varname)
                        else:
                            raise Exception('Unknown variable type. I do not know how to parse a variable with type "%s" from namelists.' % vartype)
                        
                        if vartype=='datetime':
                            datetimematch = datetimere.match(val)
                            if datetimematch==None:
                                raise namelist.NamelistParseException('Variable is not a date + time. String contents: "'+val+'"',fullnmlfilename,listname,varname)
                            refvals = map(int,datetimematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                            val = xmlstore.util.dateTimeFromTuple(refvals)
                        elif vartype=='gotmdatafile':
                            for fn in filelist:
                                if fn==val or fn.endswith('/'+val):
                                    df = container.getItem(fn)
                                    break
                            else:
                                df = xmlstore.xmlstore.DataFile()
                            val = xmlplot.data.LinkedFileVariableStore.fromNode(listchild,datafile=df,context=datafilecontext)
                            df.release()

                        listchild.setValue(val)
                        if isinstance(val,xmlstore.util.referencedobject): val.release()
                        
                    if strict and childindex<len(filechild.children):
                        lcnames = ['"%s"' % lc.getId() for lc in filechild.children[childindex:]]
                        raise namelist.NamelistParseException('Variables %s are missing' % ', '.join(lcnames),fullnmlfilename,listname,None)
        finally:
            container.release()
            nmlcontainer.release()
            if 'linkedobjects' in datafilecontext:
                for v in datafilecontext['linkedobjects'].itervalues():
                    v.release()

    def writeAsNamelists(self, targetpath, copydatafiles=True, addcomments = False, callback=None):
        if common.verbose: print 'Exporting scenario to namelist files...'

        # If the directory to write to does not exist, create it.
        createddir = False
        if (not os.path.isdir(targetpath)):
            try:
                os.mkdir(targetpath)
                createddir = True
            except Exception,e:
                raise Exception('Unable to create target directory "%s". Error: %s' %(targetpath,str(e)))

        try:
            if addcomments:
                # Import and configure text wrapping utility.
                import textwrap
                linelength = 80
                wrapper = textwrap.TextWrapper(subsequent_indent='  ')
            
            progslicer = xmlstore.util.ProgressSlicer(callback,len(self.root.children))
            for mainchild in self.root.children:
                assert not mainchild.canHaveValue(), 'Found a variable below the root node, where only folders are expected.'

                nmlfilename = mainchild.getId()
                progslicer.nextStep(nmlfilename)

                if mainchild.isHidden(): continue

                # Create the namelist file.
                nmlfilepath = os.path.join(targetpath, nmlfilename+self.namelistextension)
                nmlfile = open(nmlfilepath,'w')

                try:
                    for filechild in mainchild.children:
                        assert not filechild.canHaveValue(), 'Found a variable directly below branch "%s", where only folders are expected.' % nmlfilename
                        listname = filechild.getId()

                        if addcomments:
                            nmlfile.write('!'+(linelength-1)*'-'+'\n')
                            title = filechild.getText(detail=2).encode('ascii','xmlcharrefreplace')
                            nmlfile.write(textwrap.fill(title,linelength-2,initial_indent='! ',subsequent_indent='! '))
                            nmlfile.write('\n!'+(linelength-1)*'-'+'\n')

                            comments = []
                            varnamelength = 0
                            for listchild in filechild.children:
                                comment = self.getNamelistVariableDescription(listchild)
                                if len(comment[0])>varnamelength: varnamelength = len(comment[0])
                                comments.append(comment)
                            wrapper.width = linelength-varnamelength-5
                            for (varid,vartype,lines) in comments:
                                wrappedlines = []
                                lines.insert(0,'['+vartype+']')
                                for line in lines:
                                    line = line.encode('ascii','xmlcharrefreplace')
                                    wrappedlines += wrapper.wrap(line)
                                firstline = wrappedlines.pop(0)
                                nmlfile.write('! %-*s %s\n' % (varnamelength,varid,firstline))
                                for line in wrappedlines:
                                    nmlfile.write('! '+varnamelength*' '+'   '+line+'\n')
                            if len(comments)>0:
                                nmlfile.write('!'+(linelength-1)*'-'+'\n')
                            nmlfile.write('\n')

                        nmlfile.write('&'+listname+'\n')
                        for listchild in filechild.children:
                            if listchild.hasChildren():
                                raise Exception('Found a folder ("%s") below branch %s/%s, where only variables are expected.' % (listchild.getId(),nmlfilename,listname))
                            varname = listchild.getId()
                            varval = listchild.getValue(usedefault=True)
                            if varval==None:
                                # If the variable value is not set while its node is hidden,
                                # the variable will not be used, and we skip it silently.
                                if listchild.isHidden(): continue
                                raise Exception('Value for variable "%s" in namelist "%s" not set.' % (varname,listname))
                            vartype = listchild.getValueType()
                            if vartype=='string':
                                varval = '\''+varval+'\''
                            elif vartype=='gotmdatafile':
                                filename = listchild.getId()+'.dat'
                                if not listchild.isHidden() and copydatafiles:
                                    if not varval.validate():
                                        raise Exception('No valid custom data set for variable "%s" in namelist "%s".' % (varname,listname))
                                    varval.saveToFile(os.path.join(targetpath,filename))
                                varval.release()
                                varval = '\''+filename+'\''
                            elif vartype=='int' or vartype=='select':
                                varval = str(varval)
                            elif vartype=='float':
                                varval = str(varval)
                            elif vartype=='bool':
                                if varval:
                                    varval = '.true.'
                                else:
                                    varval = '.false.'
                            elif vartype=='datetime':
                                varval = '\''+varval.strftime('%Y-%m-%d %H:%M:%S')+'\''
                            else:
                                raise Exception('Unknown variable type %s in scenario template.' % str(vartype))
                            nmlfile.write('   '+varname+' = '+varval+',\n')
                        nmlfile.write('/\n\n')
                finally:
                    nmlfile.close()
        except:
            if createddir: shutil.rmtree(targetpath)
            raise

    @staticmethod
    def getNamelistVariableDescription(node):
        varid = node.getId()
        datatype = node.getValueType()
        description = node.getText(detail=2)
        lines = [description]
        
        if datatype == 'select':
            # Create list of options.
            options = xmlstore.util.findDescendantNode(node.templatenode,['options'])
            assert options!=None, 'Node is of type "select" but lacks "options" childnode.'
            for ch in options.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                    lab = ch.getAttribute('description')
                    if lab=='': lab = ch.getAttribute('label')
                    lines.append(ch.getAttribute('value') + ': ' + lab)

        # Create description of data type and range.
        if datatype=='gotmdatafile':
            datatype = 'file path'
        elif datatype=='int' or datatype=='select':
            datatype = 'integer'
        elif datatype=='datetime':
            datatype = 'string, format = "yyyy-mm-dd hh:mm:ss"'
        if node.templatenode.hasAttribute('minInclusive'):
            datatype += ', minimum = ' + node.templatenode.getAttribute('minInclusive')
        if node.templatenode.hasAttribute('maxInclusive'):
            datatype += ', maximum = ' + node.templatenode.getAttribute('maxInclusive')
        unit = node.getUnit()
        if unit!=None:
            datatype += ', unit = ' + unit

        # Get description of conditions (if any).
        condition = xmlstore.util.findDescendantNode(node.templatenode,['condition'])
        if condition!=None:
            condline = Scenario.getNamelistConditionDescription(condition)
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
            conddescs = map(Scenario.getNamelistConditionDescription,conds)
            return '('+(' '+condtype+' ').join(conddescs)+')'
        else:
            raise Exception('Unknown condition type "%s".' % condtype)

    def load(self,path):
        xmlstore.xmlstore.TypedStore.load(self,path)

        # If the scenario was stored in the official 'save' version, we should not consider it changed.
        # (even though we had to convert it to the 'display' version). Therefore, reset the 'changed' status.
        if self.originalversion==savedscenarioversion: self.resetChanged()

    def saveAll(self,path,targetversion=None,*args,**kwargs):
        if targetversion==None: targetversion = savedscenarioversion
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
            if start!=None and stop!=None and start>=stop:
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
            if stop!=None:
                for node in nodes:
                    if node.getValueType()!='gotmdatafile' or node.isHidden() or not validity[node]:
                        continue
                    value = node.getValue(usedefault=usedefault)
                    if value.validate():
                        for dimname in value.getDimensionNames():
                            if value.getDimensionInfo(dimname)['datatype']=='datetime':
                                dimrange = value.getDimensionRange(dimname)
                                if dimrange==None: continue
                                mintime,maxtime = dimrange
                                if stop>maxtime and maxtime!=mintime:
                                    validity[node] = False
                                    errors.append('Data series "%s" finishes at %s, before the simulation is set to end (%s).' % (node.getText(detail=1),maxtime.strftime(xmlstore.util.datetime_displayformat),stop.strftime(xmlstore.util.datetime_displayformat)))
                    value.release()
                
        return errors,validity

# ========================================================================================
# Here start custom convertors!
# ========================================================================================

class Convertor_gotm_3_2_4_to_gotm_3_3_2(xmlstore.xmlstore.Convertor):
    fixedsourceid = 'gotm-3.2.4'
    fixedtargetid = 'gotm-3.3.2'
    
    def registerLinks(self):
        self.links = [('/gotmmean/meanflow/charnok',    '/gotmmean/meanflow/charnock'),
                      ('/gotmmean/meanflow/charnok_val','/gotmmean/meanflow/charnock_val')]
        self.defaults = ['/obs/o2_profile']
Scenario.addConvertor(Convertor_gotm_3_2_4_to_gotm_3_3_2,addsimplereverse=True)

class Convertor_gotm_3_3_2_to_gotm_4_0_0(xmlstore.xmlstore.Convertor):
    fixedsourceid = 'gotm-3.3.2'
    fixedtargetid = 'gotm-4.0.0'

    def registerLinks(self):
        self.links = [('/obs/ext_pressure/PressMethod','/obs/ext_pressure/ext_press_mode')]
        self.defaults = ['/obs/wave_nml','/bio','/bio_npzd','/bio_iow','/bio_sed','/bio_fasham']
Scenario.addConvertor(Convertor_gotm_3_3_2_to_gotm_4_0_0,addsimplereverse=True)

class Convertor_gotm_4_0_0_to_gotm_4_1_0(xmlstore.xmlstore.Convertor):
    fixedsourceid = 'gotm-4.0.0'
    fixedtargetid = 'gotm-4.1.0'

    def registerLinks(self):
        self.links = [('/airsea/airsea/wet_mode','/airsea/airsea/hum_method'),
                      ('/airsea/airsea/p_e_method','/airsea/airsea/precip_method'),
                      ('/airsea/airsea/const_p_e','/airsea/airsea/const_precip'),
                      ('/airsea/airsea/p_e_flux_file','/airsea/airsea/precip_file')]
        self.defaults = ['/obs/bioprofiles',
                         '/airsea/airsea/fluxes_method',
                         '/airsea/airsea/back_radiation_method',
                         '/airsea/airsea/rain_impact',
                         '/airsea/airsea/swr_method',
                         '/airsea/airsea/swr_file',
                         '/airsea/airsea/swr_factor',
                         '/airsea/airsea/calc_evaporation',
                         '/airsea/airsea/precip_factor']

    def convert(self,source,target,callback=None):
        xmlstore.xmlstore.Convertor.convert(self,source,target)
        
        if source['airsea/airsea/calc_fluxes'].getValue():
            target['airsea/airsea/swr_method'].setValue(3)
        else:
            heatmethod = source['airsea/airsea/heat_method'].getValue()
            if heatmethod==1:
                # Constant heat flux specified
                target['airsea/airsea/swr_method'].setValue(1)
            elif heatmethod==2:
                # Variable heat flux specified: split heat flux data into times, shortwave radiation values
                # and actual heat flux values.
                progslicer = xmlstore.util.ProgressSlicer(callback,3)
                progslicer.nextStep('parsing %s' % source['airsea/airsea/heatflux_file'].getText(detail=1))
                oldfile = source['airsea/airsea/heatflux_file'].getValue()
                datold = oldfile.getData(progslicer.getStepCallback())
                times,swr,heat = datold[0],datold[1].take((0,),1),datold[1].take((1,),1)
                oldfile.release()
                
                # Save shortwave radiation
                progslicer.nextStep('saving %s' % target['airsea/airsea/swr_file'].getText(detail=1))
                if (swr==swr[0]).all():
                    # All shortwave radiation values are equal: set shortwave radiation method to constant
                    target['airsea/airsea/swr_method'].setValue(1)
                    target['airsea/airsea/const_swr'].setValue(swr[0])
                else:
                    # Variable shortwave radiation: set shortwave radiation method to custom (i.e. from file)
                    # and supply the data.
                    target['airsea/airsea/swr_method'].setValue(2)
                    swrnew = target['airsea/airsea/swr_file'].getValue()
                    swrnew.setData([times,swr])
                    target['airsea/airsea/swr_file'].setValue(swrnew)
                    swrnew.release()
                    
                # Save heat flux
                progslicer.nextStep('saving %s' % target['airsea/airsea/heatflux_file'].getText(detail=1))
                if (heat==heat[0]).all():
                    # All heat flux values are equal: set heat flux method to constant
                    target['airsea/airsea/heat_method'].setValue(1)
                    target['airsea/airsea/const_heat'].setValue(heat[0])
                    target['airsea/airsea/heatflux_file'].setValue(None)
                else:
                    # Variable heat flux: set heat flux method to custom (i.e. from file) and supply the data.
                    target['airsea/airsea/heat_method'].setValue(2)
                    heatnew = target['airsea/airsea/heatflux_file'].getValue()
                    heatnew.setData([times,heat])
                    target['airsea/airsea/heatflux_file'].setValue(heatnew)
                    heatnew.release()
Scenario.addConvertor(Convertor_gotm_4_0_0_to_gotm_4_1_0)

class Convertor_gotmgui_4_1_0_to_gotm_4_0_0(xmlstore.xmlstore.Convertor):
    fixedsourceid = 'gotm-4.1.0'
    fixedtargetid = 'gotm-4.0.0'

    def registerLinks(self):
        self.links = Convertor_gotm_4_0_0_to_gotm_4_1_0().reverseLinks()
    
    def convert(self,source,target,callback=None):
        xmlstore.xmlstore.Convertor.convert(self,source,target)

        if not source['airsea/airsea/calc_fluxes'].getValue():
            swr_method = source['airsea/airsea/swr_method'].getValue()
            heat_method = source['airsea/airsea/heat_method'].getValue()
            if swr_method==2 or heat_method==2:
                # Variable shortwave radiation and/or variable heat flux was specified.
                import numpy
                
                # Load all values for shortwave radiation and the heat flux.
                swrdata,heatdata = None,None
                nsteps = 2
                if swr_method==2 and heat_method==2: nsteps += 1
                progslicer = xmlstore.util.ProgressSlicer(callback,nsteps)
                if swr_method ==2:
                    progslicer.nextStep('parsing %s' % source['airsea/airsea/swr_file'].getText(detail=1))
                    swrdata  = source['airsea/airsea/swr_file'].getValue()
                    swrdata.getData(callback=progslicer.getStepCallback())
                if heat_method==2:
                    progslicer.nextStep('parsing %s' % source['airsea/airsea/heatflux_file'].getText(detail=1))
                    heatdata = source['airsea/airsea/heatflux_file'].getValue()
                    heatdata.getData(callback=progslicer.getStepCallback())

                # Create vectors for time, shortwave radiation and the heat flux.
                if swrdata==None:
                    # No variable shortwave radiation, i.e., we only have a variable heat flux.
                    times = heatdata.data[0]
                    heat = heatdata.data[1]
                    swr = numpy.empty(heat.shape)
                    swr.fill(source['airsea/airsea/const_swr'].getValue())
                elif heatdata==None:
                    # No variable heat flux, i.e., we only have a variable shortwave radiation.
                    times = swrdata.data[0]
                    swr = swrdata.data[1]
                    heat = numpy.empty(swr.shape)
                    heat.fill(source['airsea/airsea/const_heat'].getValue())
                else:
                    # We have both a variable shortwave radiation and a variable heat flux.
                    # Combine both, and interpolate where values of either variable are not available.
                    times = numpy.unique(numpy.concatenate((swrdata.data[0],heatdata.data[0]),0))
                    times.sort()
                    swr = xmlplot.common.interp1(swrdata.data[0],swrdata.data[1],times)
                    heat = xmlplot.common.interp1(heatdata.data[0],heatdata.data[1],times)
                    
                # Store merged shortwave radation/heat flux data.
                progslicer.nextStep('writing %s' % target['airsea/airsea/heatflux_file'].getText(detail=1))
                mergeddata  = target['airsea/airsea/heatflux_file'].getValue()
                mergeddata.setData([times,numpy.concatenate((swr,heat),1)])
                target['airsea/airsea/heat_method'].setValue(2)
                target['airsea/airsea/heatflux_file'].setValue(mergeddata)

                # Release data files.
                mergeddata.release()
                if swrdata !=None: swrdata.release()
                if heatdata!=None: heatdata.release()
                    
Scenario.addConvertor(Convertor_gotmgui_4_1_0_to_gotm_4_0_0)

class Convertor_gotm_4_1_0_to_gotmgui_0_5_0(xmlstore.xmlstore.Convertor):
    fixedsourceid = 'gotm-4.1.0'
    fixedtargetid = 'gotmgui-0.5.0'

    def registerLinks(self):
        self.links = [('/gotmrun/model_setup/title',      '/title'),
                      ('/gotmrun/model_setup/cnpar',      '/timeintegration/cnpar'),
                      ('/gotmrun/station',                '/station'),
                      ('/gotmrun/time',                   '/time'),
                      ('/gotmrun/output',                 '/output'),
                      ('/gotmrun/model_setup/buoy_method','/meanflow/buoy_method'),
                      ('/gotmrun/model_setup/nlev',       '/grid/nlev'),
                      ('/gotmrun/eqstate',                '/meanflow'),
                      ('/gotmrun/eqstate',                '/meanflow/eq_state_method'),
                      ('/gotmmean/meanflow/grid_method',  '/grid/grid_method'),
                      ('/gotmmean/meanflow/ddu',          '/grid/ddu'),
                      ('/gotmmean/meanflow/ddl',          '/grid/ddl'),
                      ('/gotmmean/meanflow/grid_file',    '/grid/grid_file'),
                      ('/gotmmean/meanflow',              '/meanflow'),
                      ('/airsea/airsea',                  '/airsea'),
                      ('/gotmturb/turbulence',            '/gotmturb'),
                      ('/gotmturb/scnd',                  '/gotmturb/scnd/scnd_coeff'),
                      ('/kpp/kpp',                        '/gotmturb/kpp'),
                      ('/obs/sprofile/s_prof_method',     '/obs/sprofile'),
                      ('/obs/sprofile',                   '/obs/sprofile/SRelax'),
                      ('/obs/tprofile/t_prof_method',     '/obs/tprofile'),
                      ('/obs/tprofile',                   '/obs/tprofile/TRelax'),
                      ('/obs/ext_pressure/ext_press_method','/obs/ext_pressure'),
                      ('/obs/int_pressure/int_press_method','/obs/int_pressure'),
                      ('/obs/extinct/extinct_method',     '/obs/extinct'),
                      ('/obs/w_advspec/w_adv_method',     '/obs/w_advspec'),
                      ('/obs/zetaspec/zeta_method',       '/obs/zetaspec'),
                      ('/obs/wave_nml/wave_method',       '/obs/wave_nml'),
                      ('/obs/velprofile/vel_prof_method', '/obs/velprofile'),
                      ('/obs/eprofile/e_prof_method',     '/obs/eprofile'),
                      ('/obs/o2_profile/o2_prof_method',  '/obs/o2_profile'),
                      ('/obs/bioprofiles/bio_prof_method','/obs/bioprofiles'),
                      ('/bio/bio_nml',                    '/bio'),
                      ('/bio_npzd/bio_npzd_nml',          '/bio/bio_model/bio_npzd'),
                      ('/bio_iow/bio_iow_nml',            '/bio/bio_model/bio_iow'),
                      ('/bio_sed/bio_sed_nml',            '/bio/bio_model/bio_sed'),
                      ('/bio_fasham/bio_fasham_nml',      '/bio/bio_model/bio_fasham')]
    
    def convert(self,source,target,callback=None):
        xmlstore.xmlstore.Convertor.convert(self,source,target)
        
        # ===============================================
        #  gotmrun
        # ===============================================

        # Convert absolute time interval to relative time interval.
        dt = source['gotmrun/model_setup/dt'].getValue()
        target['timeintegration/dt'].setValue(xmlstore.xmlstore.StoreTimeDelta(seconds=dt))
        target['output/dtsave'].setValue(xmlstore.xmlstore.StoreTimeDelta(seconds=dt*source['gotmrun/output/nsave'].getValue()))

        # ===============================================
        #  meanflow
        # ===============================================

        target['meanflow/z0s'].setValue(target['meanflow/z0s_min'].getValue())
        
        # ===============================================
        #  airsea
        # ===============================================

        # Convert calc_fluxes from boolean into integer.
        if source['airsea/airsea/calc_fluxes'].getValue():
            target['airsea/flux_source'].setValue(0)
        else:
            target['airsea/flux_source'].setValue(1)
        
        # If heat fluxes are effectively disabled, set the heat flux method to "none"
        if source['airsea/airsea/heat_method'].getValue()==1 and target['airsea/const_heat'].getValue()==0.:
            target['airsea/heat_method'].setValue(0)

        # If short-wave radiation is effectively disabled, set the swr method to "none"
        if source['airsea/airsea/swr_method'].getValue()==1 and target['airsea/const_swr'].getValue()==0.:
            target['airsea/swr_method'].setValue(0)

        # If momentum fluxes are effectively disabled, set the momentum flux method to "none"
        if source['airsea/airsea/momentum_method'].getValue()==1 and target['airsea/const_tx'].getValue()==0. and target['airsea/const_ty'].getValue()==0.:
            target['airsea/momentum_method'].setValue(0)
        
        # ===============================================
        #  obs: salinity
        # ===============================================

        # Merge analytical salinity profile setting into main salinity settings.
        if source['obs/sprofile/s_prof_method'].getValue()==1:
            target['obs/sprofile'].setValue(10+source['obs/sprofile/s_analyt_method'].getValue())

        # Copy constant salinity, surface salinity from shared top layer salinity.
        target['obs/sprofile/s_const'].setValue(target['obs/sprofile/s_1'].getValue())
        target['obs/sprofile/s_surf' ].setValue(target['obs/sprofile/s_1'].getValue())

        # Determine type of salinity relaxation.        
        relaxbulk = source['obs/sprofile/SRelaxTauM'].getValue()<1e+15
        relaxbott = source['obs/sprofile/SRelaxTauB'].getValue()<1e+15 and source['obs/sprofile/SRelaxBott'].getValue()>0
        relaxsurf = source['obs/sprofile/SRelaxTauS'].getValue()<1e+15 and source['obs/sprofile/SRelaxSurf'].getValue()>0
        target['obs/sprofile/SRelax'].setValue(relaxbulk or relaxbott or relaxsurf)
        
        # ===============================================
        #  obs: temperature
        # ===============================================

        # Merge analytical temperature profile setting into main temperature settings.
        if source['obs/tprofile/t_prof_method'].getValue()==1:
            target['obs/tprofile'].setValue(10+source['obs/tprofile/t_analyt_method'].getValue())

        # Copy constant temperature, surface temperature from shared top layer temperature.
        target['obs/tprofile/t_const'].setValue(target['obs/tprofile/t_1'].getValue())
        target['obs/tprofile/t_surf' ].setValue(target['obs/tprofile/t_1'].getValue())

        # Determine type of temperature relaxation.        
        relaxbulk = source['obs/tprofile/TRelaxTauM'].getValue()<1e+15
        relaxbott = source['obs/tprofile/TRelaxTauB'].getValue()<1e+15 and source['obs/tprofile/TRelaxBott'].getValue()>0
        relaxsurf = source['obs/tprofile/TRelaxTauS'].getValue()<1e+15 and source['obs/tprofile/TRelaxSurf'].getValue()>0
        target['obs/tprofile/TRelax'].setValue(relaxbulk or relaxbott or relaxsurf)

        # ===============================================
        #  obs: external pressure
        # ===============================================

        target['obs/ext_pressure/PressUOffset' ].setValue(source['obs/ext_pressure/PressConstU'].getValue())
        target['obs/ext_pressure/PressVOffset' ].setValue(source['obs/ext_pressure/PressConstV'].getValue())

        # ===============================================
        #  obs: sea surface elevation
        # ===============================================

        ref = source['obs/zetaspec/zeta_0'].getValue()
        target['obs/zetaspec/zeta_const' ].setValue(ref)
        target['obs/zetaspec/zeta_offset'].setValue(ref)

        # Note: we implicitly lose output settings out_fmt, out_dir and out_fn; the GUI scenario
        # does not support (or need) these.
Scenario.addConvertor(Convertor_gotm_4_1_0_to_gotmgui_0_5_0)

class Convertor_gotmgui_0_5_0_to_gotm_4_1_0(xmlstore.xmlstore.Convertor):
    fixedsourceid = 'gotmgui-0.5.0'
    fixedtargetid = 'gotm-4.1.0'

    def registerLinks(self):
        self.links = Convertor_gotm_4_1_0_to_gotmgui_0_5_0().reverseLinks()
    
    def convert(self,source,target,callback=None):
        xmlstore.xmlstore.Convertor.convert(self,source,target)

        # ===============================================
        #  gotmrun
        # ===============================================

        # Move from absolute time interval between outputs to relative intervals (number of simulation steps)
        dt = source['timeintegration/dt'].getValue().getAsSeconds()
        target['gotmrun/model_setup/dt'].setValue(dt)
        relinterval = int(source['output/dtsave'].getValue().getAsSeconds()/dt)
        if relinterval<1: relinterval=1
        target['gotmrun/output/nsave'].setValue(relinterval)

        # If we use a custom grid, take the number of layers from the grid file.
        if source['/grid/grid_method'].getValue()>0:
            val = source['/grid/grid_file'].getValue()
            nlev = len(val.getData()[0])
            target['gotmrun/model_setup/nlev'].setValue(nlev)

        # Add output path and type (not present in GUI scenarios)
        target['gotmrun/output/out_fmt'].setValue(2)
        target['gotmrun/output/out_dir'].setValue('.')
        target['gotmrun/output/out_fn' ].setValue('result')

        # ===============================================
        #  meanflow
        # ===============================================

        # Choose between constant and minimum surface roughness value, based on use of Charnock adaptation.
        if not source['meanflow/charnock'].getValue():
            target['gotmmean/meanflow/z0s_min'].setValue(source['meanflow/z0s'].getValue())

        # ===============================================
        #  airsea
        # ===============================================

        # Convert flux source from "select" to "bool".
        target['airsea/airsea/calc_fluxes'].setValue(source['airsea/flux_source'].getValue()==0)

        # ===============================================
        #  obs: salinity
        # ===============================================

        # If an analytical salinity profile is used, extract the analytical method from the main salinity setting.
        sprofile = source['obs/sprofile'].getValue()
        if sprofile>10:
            target['obs/sprofile/s_prof_method'].setValue(1)
            target['obs/sprofile/s_analyt_method'].setValue(sprofile-10)

        # Choose between constant and surface salinity based on chosen analytical method.
        s_analyt_method = target['obs/sprofile/s_analyt_method'].getValue()
        if s_analyt_method==1:
            target['obs/sprofile/s_1'].setValue(source['obs/sprofile/s_const'].getValue())
        elif s_analyt_method==3:
            target['obs/sprofile/s_1'].setValue(source['obs/sprofile/s_surf'].getValue())

        # Disable salinity relaxation where needed.
        if not source['obs/sprofile/SRelax'].getValue():
            target['obs/sprofile/SRelaxTauM'].setValue(1.e15)
            target['obs/sprofile/SRelaxTauB'].setValue(1.e15)
            target['obs/sprofile/SRelaxTauS'].setValue(1.e15)

        # ===============================================
        #  obs: temperature
        # ===============================================

        # If an analytical temperature profile is used, extract the analytical method from the main temperature setting.
        tprofile = source['obs/tprofile'].getValue()
        if tprofile>10:
            target['obs/tprofile/t_prof_method'  ].setValue(1)
            target['obs/tprofile/t_analyt_method'].setValue(tprofile-10)

        # Choose between constant and surface temperature based on chosen analytical method.
        t_analyt_method = target['obs/tprofile/t_analyt_method'].getValue()
        if t_analyt_method==1:
            target['obs/tprofile/t_1'].setValue(source['obs/tprofile/t_const'].getValue())
        elif t_analyt_method==3:
            target['obs/tprofile/t_1'].setValue(source['obs/tprofile/t_surf' ].getValue())

        # Disable temperature relaxation where needed.
        if not source['obs/tprofile/TRelax'].getValue():
            target['obs/tprofile/TRelaxTauM'].setValue(1.e15)
            target['obs/tprofile/TRelaxTauB'].setValue(1.e15)
            target['obs/tprofile/TRelaxTauS'].setValue(1.e15)

        # ===============================================
        #  obs: external pressure
        # ===============================================

        if source['obs/ext_pressure'].getValue()==1:
            target['obs/ext_pressure/PressConstU'].setValue(source['obs/ext_pressure/PressUOffset'].getValue())
            target['obs/ext_pressure/PressConstV'].setValue(source['obs/ext_pressure/PressVOffset'].getValue())

        # ===============================================
        #  obs: internal pressure
        # ===============================================

        if source['obs/int_pressure'].getValue()==1:
            if source['obs/sprofile'].getValue()==0:
                target['obs/int_pressure/const_dsdx'].setValue(0.0)
                target['obs/int_pressure/const_dsdy'].setValue(0.0)
            if source['obs/tprofile'].getValue()==0:
                target['obs/int_pressure/const_dtdx'].setValue(0.0)
                target['obs/int_pressure/const_dtdy'].setValue(0.0)
        
        # ===============================================
        #  obs: sea surface elevation
        # ===============================================

        if source['obs/zetaspec'].getValue()==1:
            target['obs/zetaspec/zeta_0' ].setValue(source['obs/zetaspec/zeta_offset'].getValue())
        else:
            target['obs/zetaspec/zeta_0' ].setValue(source['obs/zetaspec/zeta_const' ].getValue())

Scenario.addConvertor(Convertor_gotmgui_0_5_0_to_gotm_4_1_0)

