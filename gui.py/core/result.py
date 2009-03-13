import os.path, xml.dom.minidom, shutil, StringIO

import xmlplot.common, xmlstore.xmlstore, xmlplot.plot, xmlplot.data
import common, scenario

class ResultProperties(xmlstore.xmlstore.TypedStore):
    """Class for result properties, based on xmlstore.TypedStore.
    
    Currently this does nothing specific except automatically selecting the
    correct XML schema, and allowing access to schemas based on their short names.
    In the future this class can host convertors that convert between different
    versions of the XML schema for results.
    """

    def __init__(self,valueroot=None,adddefault = True):
        schemadom = os.path.join(common.getDataRoot(),'schemas/result/gotmgui.xml')
        xmlstore.xmlstore.TypedStore.__init__(self,schemadom,valueroot,adddefault=adddefault)

    schemadict = None
    @staticmethod
    def getDefaultSchemas():
        if FigureProperties.schemadict==None:
            FigureProperties.schemadict = xmlstore.xmlstore.ShortcutDictionary.fromDirectory(os.path.join(common.getDataRoot(),'schemas/result'))
        return FigureProperties.schemadict
        
    @classmethod
    def getCustomDataTypes(ownclass):
        dt = xmlplot.plot.FigureProperties.getCustomDataTypes()
        return dt

# Class that represents a GOTM result.
#   Inherits from xmlplot.data.NetCDFStore_GOTM, as it encapsulates a GOTM-created NetCDF file.
#   Contains a link to the scenario from which the result was created (if available)
class Result(xmlplot.data.NetCDFStore_GOTM):

    def __init__(self):
        xmlplot.data.NetCDFStore_GOTM.__init__(self)
        
        self.scenario = None
        self.tempdir = None
        self.changed = False
        
        self.stdout = None
        self.stderr = None
        self.returncode = 0
        self.errormessage = None
        
        self.store = ResultProperties()
        self.wantedscenarioversion = scenario.guiscenarioversion
        
        self.path = None

    def hasChanged(self):
        return self.changed or self.store.changed

    def getTempDir(self,empty=False):
        if self.tempdir!=None:
            if empty:
                common.TempDirManager.empty(self.tempdir)
        else:
            self.tempdir = common.TempDirManager.create(prefix='gotm-')
        return self.tempdir
        
    def saveNetCDF(self,path):
        xmlplot.data.NetCDFStore_GOTM.save(self,path)

    def save(self,path,addfiguresettings=True,callback=None):
        assert self.datafile!=None, 'The result object was not yet attached to a result file (NetCDF).'

        progslicer = xmlstore.util.ProgressSlicer(callback,2)

        # Create a ZIP container to hold the result.
        container = xmlstore.datatypes.DataContainerZip(path,'w')

        if not addfiguresettings:
            # First clear all figure settings.
            self.store['FigureSettings'].clearValue(recursive=True)

        # Add the XML file describing result properties.            
        df = xmlstore.datatypes.DataFileXmlNode(self.store.root.valuenode)
        df_added = container.addItem(df,'result.xml')
        df_added.release()
        df.release()
        self.store.resetChanged()

        # If we have a link to the scenario, add it to the result file.
        progslicer.nextStep('saving scenario')
        if self.scenario!=None:
            fscen = StringIO.StringIO()
            self.scenario.saveAll(fscen,claim=False,callback=progslicer.getStepCallback())
            df = xmlstore.datatypes.DataFileMemory(fscen.getvalue(),'scenario.gotmscenario')
            fscen.close()
            added = container.addItem(df)
            added.release()
            df.release()
        
        # Add the result data (NetCDF)
        progslicer.nextStep('saving result data')
        container.addFile(self.datafile,'result.nc')
        
        # Make changes to container persistent (this closes the ZIP file), and release it.
        container.persistChanges()
        container.release()

        # Saved all changes; reset "changed" state
        self.changed = False
        
        # Store the path we saved to.
        self.path = path

    @classmethod
    def canBeOpened(cls, container):
        assert isinstance(container,xmlstore.datatypes.DataContainer), 'Argument must be data container object.'
        filelist = container.listFiles()
        return ('result.nc' in filelist and 'scenario.gotmscenario' in filelist)

    def load(self,path):
        if isinstance(path,basestring):
            container = xmlstore.datatypes.DataContainer.fromPath(path)
        elif isinstance(path,xmlstore.datatypes.DataContainer):
            container = path.addref()

        files = container.listFiles()
        if 'scenario.gotmscenario' not in files:
            raise Exception('The archive "%s" does not contain "scenario.gotmscenario"; it cannot be a GOTM result.' % path)
        if 'result.nc' not in files:
            raise Exception('The archive "%s" does not contain "result.nc"; it cannot be a GOTM result.' % path)

        # Create a temporary directory to which we can unpack the archive.
        tempdir = self.getTempDir()

        df = container.getItem('scenario.gotmscenario')
        self.scenario = scenario.Scenario.fromSchemaName(self.wantedscenarioversion)
        self.scenario.loadAll(df)
        df.release()

        df = container.getItem('result.nc')
        resultpath = os.path.join(tempdir,'result.nc')
        df.saveToFile(resultpath)
        df.release()

        df = container.getItem('result.xml')
        if df!=None:
            f = df.getAsReadOnlyFile()
            valuedom = xml.dom.minidom.parse(f)
            self.store.setStore(valuedom)
            f.close()
            df.release()

        # Store path from where the result was loaded
        self.path = container.path

        # Close the archive
        container.release()

        # Attach the result, try to open the CDF file
        xmlplot.data.NetCDFStore_GOTM.load(self,resultpath)

        # Reset "changed" status.
        self.changed = False

    def setFigure(self,name,source):
        setroot = self.store['FigureSettings']
        fignodename = source.root.templatenode.getAttribute('name')
        fig = setroot.getChildById(fignodename,name,create=True)
        fig.copyFrom(source.root,replace=True)

    def getFigure(self,name,target):
        setroot = self.store['FigureSettings']
        fignodename = target.root.templatenode.getAttribute('name')
        fig = setroot.getChildById(fignodename,name,create=False)
        if fig!=None:
            target.root.copyFrom(fig,replace=True)
            return True
        return False

    def unlink(self):
        # First unlink NetCDf store, because that releases the .nc file,
        # allowing us to delete the temporary directory.
        xmlplot.data.NetCDFStore_GOTM.unlink(self)

        if self.tempdir!=None:
            # Delete temporary directory.
            common.TempDirManager.delete(self.tempdir)
            self.tempdir = None
        if self.scenario!=None:
            self.scenario.release()
        self.store.release()
            
    def attach(self,srcpath,scenario=None,copy=True):
        if scenario!=None:
            self.scenario = scenario.convert(self.wantedscenarioversion)
        else:
            self.scenario = None
            
        if copy:
            # Create a copy of the result file.
            tempdir = self.getTempDir(empty=True)
            datafile = os.path.join(tempdir,'result.nc')
            shutil.copyfile(srcpath,datafile)
        else:
            datafile = srcpath

        xmlplot.data.NetCDFStore_GOTM.load(self,datafile)

        # Attached to an existing result: we consider it unchanged.
        self.changed = False
        
        if self.scenario!=None and self.scenario.path!=None and self.scenario.path.endswith('.gotmscenario'):
            self.path = self.scenario.path[:-12]+'gotmresult'
        else:
            self.path = None

    def getPlottableVariableNames_raw(self):
        names = self.getVariableNames_raw()
        for i in range(len(names)-1,-1,-1):
            dimnames = self.nc.variables[names[i]].dimensions
            dimcount = len(dimnames)
            good = False
            if   dimcount==3:
                if dimnames==('time','lat','lon'):
                    good = True
            elif dimcount==4:
                if (dimnames==('time','z','lat','lon')) | (dimnames==('time','z1','lat','lon')):
                    good = True
            if not good: del names[i]
        return names

    def getVariableTree(self,path,plottableonly=True):
        otherstores = {}
        if self.scenario!=None: otherstores['scenario'] = self.scenario
        return xmlplot.common.VariableStore.getVariableTree(self,path,otherstores=otherstores,plottableonly=plottableonly)

    def getDefaultCoordinateDelta(self,dimname,coord):
        if self.scenario!=None and self.isTimeDimension(dimname):
            delta = self.scenario['output/dtsave'].getValue(usedefault=True)
            if delta!=None: return delta.getAsSeconds()/86400.
        return xmlplot.data.NetCDFStore_GOTM.getDefaultCoordinateDelta(self,dimname,coord)
