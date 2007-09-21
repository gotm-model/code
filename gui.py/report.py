import os, xml.dom.minidom, shutil

import common, xmlstore, plot, matplotlib, data

def createtable(xmldocument,tds,columncount):
    table = xmldocument.createElement('table')
    icurvar = 0
    tr = None
    for td in tds:
        if icurvar % columncount == 0:
            tr = xmldocument.createElement('tr')
            table.appendChild(tr)
        tr.appendChild(td)
        icurvar = icurvar+1
    if tr!=None and len(table.childNodes)>1:
        for i in range(columncount - len(tr.childNodes)):
            tr.appendChild(xmldocument.createElement('td'))
    return table

class Report(common.referencedobject):
    reportdirname = 'reporttemplates'
    reportname2path = None

    @staticmethod
    def getTemplates():
        if Report.reportname2path==None:
            Report.reportname2path = {}
            sourcedir = Report.reportdirname
            if os.path.isdir(sourcedir):
                for filename in os.listdir(sourcedir):
                    if filename=='CVS': continue
                    fullpath = os.path.join(sourcedir,filename)
                    if os.path.isdir(fullpath):
                        if os.path.isfile(os.path.join(fullpath,'index.xml')):
                            Report.reportname2path[filename] = fullpath
                        else:
                            print 'WARNING: template directory "%s" does not contain "index.xml"; it will be ignored.' % fullpath
                    else:
                        print 'WARNING: template directory "%s" contains "%s" which is not a directory; the latter will be ignored.' % (sourcedir,filename)
            else:
                print 'WARNING: no report templates will be available, because subdirectory "%s" is not present!' % Report.reportdirname
        return Report.reportname2path

    def __init__(self,defaultfont=None):
        common.referencedobject.__init__(self)
        
        self.store = xmlstore.TypedStore('schemas/report/gotmgui.xml')

        self.defaultstore = xmlstore.TypedStore('schemas/report/gotmgui.xml')

        # Set some default properties.
        self.defaultstore.setProperty('Figures/Width',10)
        self.defaultstore.setProperty('Figures/Height',8)
        self.defaultstore.setProperty('Figures/Resolution',96)
        self.defaultstore.setProperty('Figures/FontScaling',100)
        self.defaultstore.setProperty('Figures/FontName',defaultfont)

        self.store.setDefaultStore(self.defaultstore)
        
    def unlink(self):
        self.defaultstore.release()
        self.defaultstore = None
        self.store.release()
        self.store = None
        
    def generate(self,result,outputpath,templatepath,columncount=2,callback=None):
        xmldocument = xml.dom.minidom.parse(os.path.join(templatepath,'index.xml'))
        scenario = result.scenario
        
        # Get report settings
        figuresize = (self.store.getProperty('Figures/Width',usedefault=True),self.store.getProperty('Figures/Height',usedefault=True))
        dpi = self.store.getProperty('Figures/Resolution',usedefault=True)
        fontscaling = self.store.getProperty('Figures/FontScaling',usedefault=True)
        fontname = self.store.getProperty('Figures/FontName',usedefault=True)

        # Get list of variables to plot
        selroot = self.store.root['Figures/Selection']
        plotvariables = [node.getValue() for node in selroot.children]

        steps = float(2+len(plotvariables))
        istep = 0

        # Get a list of all input datasets.
        inputdata = []
        for node in scenario.root.getNodesByType('file'):
            if node.isHidden(): continue
            value = node.getValueOrDefault()
            if value!=None and value.isValid():
                store = data.LinkedFileVariableStore.fromNode(node)
                inputdata.append((node,store,value))
                steps += 1+len(store.getVariableNames())

        # Create output directory if it does not exist yet.
        if not os.path.isdir(outputpath): os.mkdir(outputpath)

        # Copy auxilliary files such as CSS, JS (everything but index.xml)
        for f in os.listdir(templatepath):
            fullpath = os.path.join(templatepath,f)
            if f.lower()!='index.xml' and os.path.isfile(fullpath):
                shutil.copy(fullpath,os.path.join(outputpath,f))

        # --------------------------------------------------------------
        # Replace "gotm:scenarioproperty" tags in index.xml by the
        # current value of the corresponding scenario property.
        # --------------------------------------------------------------

        for node in xmldocument.getElementsByTagName('gotm:scenarioproperty'):
            variablepath = node.getAttribute('variable')
            assert variablepath!='', 'gotm:scenarioproperty node in report template lacks "variable" attribute, whcih should point to a location in the scenario.'
            variablenode = scenario.root[variablepath]
            assert variablenode!=None, 'Unable to locate "%s" in the scenario.' % variablepath
            val = variablenode.getValueAsString()
            node.parentNode.replaceChild(xmldocument.createTextNode(unicode(val)),node)
            node.unlink()

        # --------------------------------------------------------------
        # Build table with scenario settings.
        # --------------------------------------------------------------

        scenarionodes = xmldocument.getElementsByTagName('gotm:scenario')
        assert len(scenarionodes)<=1, 'Found more than one "gotm:scenario" node in the report template.'
        if len(scenarionodes)>0:
            if callback!=None: callback(istep/steps,'Creating scenario description...')
            scenarionode = scenarionodes[0]

            sceninterface = scenario.getInterface(showhidden=False,omitgroupers=True)

            scentable = xmldocument.createElement('table')
            scentable.setAttribute('id','tableScenario')

            totaldepth = sceninterface.getDepth(scenario.root)

            # Create columns.
            for i in range(totaldepth-2):
                col = xmldocument.createElement('col')
                col.setAttribute('width','25')
                scentable.appendChild(col)
            col = xmldocument.createElement('col')
            scentable.appendChild(col)
            col = xmldocument.createElement('col')
            scentable.appendChild(col)

            # Create rows
            for tr in sceninterface.toHtml(scenario.root,xmldocument,totaldepth-1,level=-1,hidedefaults=True):
                scentable.appendChild(tr)
            
            # Break link from scenario to interface.
            scenario.disconnectInterface(sceninterface)

            scenarionode.parentNode.replaceChild(scentable,scenarionode)

        istep += 1
        
        # Create figure to be used for plotting observations and results.
        if len(inputdata)>0 or len(plotvariables)>0:
            #mplfigure = matplotlib.figure.Figure(figsize=(figuresize[0]/2.54,figuresize[1]/2.54))
            #canvas = matplotlib.backends.backend_agg.FigureCanvasAgg(mplfigure)
            fig = plot.Figure(defaultfont=fontname)
        
        # --------------------------------------------------------------
        # Create figures for input data
        # --------------------------------------------------------------

        if len(inputdata)>0:
            nodeParent = scentable.parentNode
            nodePreceding = scentable.nextSibling
            for node,store,datafile in inputdata:
                if callback!=None:
                    store.loadDataFile(datafile,lambda msg,progress: callback((istep+progress)/steps,'Parsing %s...' % (node.getText(1),)))
                else:
                    store.loadDataFile(datafile)
                istep += 1
                tds = []
                fig.addDataSource('input',store)
                vardict = store.getVariableLongNames()
                for varid in store.getVariableNames():
                    longname = vardict[varid]
                    if callback!=None: callback(istep/steps,'Creating figure for %s...' % longname)

                    fig.setUpdating(False)
                    fig.clearProperties()
                    fig.addVariable(varid)
                    fig.properties.setProperty('FontScaling',fontscaling)
                    fig.setUpdating(True)
                    filename = 'in_'+varid+'.png'
                    outputfile = os.path.join(outputpath,filename)
                    fig.exportToFile(outputfile,dpi=dpi)

                    img = xmldocument.createElement('img')
                    img.setAttribute('src',filename)
                    img.setAttribute('alt',longname)
                    img.setAttribute('style','width:%.2fcm' % figuresize[0])
                    td = xmldocument.createElement('td')
                    td.appendChild(img)
                    tds.append(td)

                    istep += 1
                header = xmldocument.createElement('h3')
                header.appendChild(xmldocument.createTextNode(node.getText(1)))
                figurestable = createtable(xmldocument,tds,columncount)
                nodeParent.insertBefore(header,nodePreceding)
                nodeParent.insertBefore(figurestable,nodePreceding)

        # --------------------------------------------------------------
        # Create figures for result variables
        # --------------------------------------------------------------

        figuresnodes = xmldocument.getElementsByTagName('gotm:figures')
        assert len(figuresnodes)<=1, 'Found more than one "gotm:figures" node in the report template.'
        if len(figuresnodes)>0:
            figuresnode = figuresnodes[0]
        else:
            figuresnode = None
        if len(plotvariables)>0 and figuresnode!=None:
            fig.clearSources()
            fig.addDataSource('result',result)
            tds = []
            for varpath in plotvariables:
                varid = varpath.split('/')[-1]
                
                longname = result.getVariable(varid).getLongName()
                if callback!=None: callback(istep/steps,'Creating figure for %s...' % longname)
                
                fig.setUpdating(False)
                if not result.getFigure('result/'+varpath,fig.properties):
                    fig.clearProperties()
                    fig.addVariable(varid)
                fig.properties.setProperty('FontScaling',fontscaling)
                fig.setUpdating(True)
                filename = 'out_'+varid+'.png'
                outputfile = os.path.join(outputpath,filename)
                fig.exportToFile(outputfile,dpi=dpi)

                img = xmldocument.createElement('img')
                img.setAttribute('src',filename)
                img.setAttribute('alt',longname)
                img.setAttribute('style','width:%.2fcm' % figuresize[0])
                td = xmldocument.createElement('td')
                td.appendChild(img)
                tds.append(td)

                istep += 1
            figurestable = createtable(xmldocument,tds,columncount)
            figuresnode.parentNode.replaceChild(figurestable,figuresnode)
        elif figuresnode!=None:
            figuresnode.parentNode.removeChild(figuresnode)
        
        if callback!=None: callback(istep/steps,'Writing HTML...')

        if outputpath!='':
            import codecs
            f = codecs.open(os.path.join(outputpath,'index.html'),'w','utf-8')
            xmldocument.writexml(f,encoding='utf-8')
            f.close()
        else:
            print xmldocument.toxml('utf-8')
        istep += 1

        if callback!=None: callback(istep/steps,'Done.')

