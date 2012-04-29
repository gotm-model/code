import re, xml.dom.minidom, os

import util,datatypes

class Convertor(object):
    """Base class for conversion between TypedStore objects that differ
    in version; derive custom convertors from this class.
    
    For the most simple conversions, you can just derive a class from
    this generic Convertor, specify a list of links between source and
    target nodes for those nodes that changed name and/or location between
    versions. The lists of explicit links should be built in the overridden
    method "registerLinks".
    
    For more advanced conversions, you can in addition override the
    "convertCustom" method for custom functionality.
    
    Note: conversions can also be fully configured by means of XML files.
    This is usually preferable over designing custom classes that override Converter.
    For more information see the XmlConverter class.
    """
    fixedsourceid = None
    fixedtargetid = None
    
    defaults = None

    def __init__(self,sourceid=None,targetid=None):
        if sourceid is None:
            if self.fixedsourceid is None:
                raise Exception('Convertor class was created without explicit version identifiers, but also lacks default identifiers.')
            sourceid = self.fixedsourceid
            targetid = self.fixedtargetid
        else:
            if self.fixedsourceid is not None:
                raise Exception('Convertor class was created with explicit version identifiers, but also has default identifiers.')
        
        self.sourceid = sourceid
        self.targetid = targetid

        self.links = []
        self.registerLinks()

    def registerLinks(self):
        """This method can be overridden by inheriting classes to specify
        a list of links between source- and target nodes, and a list of target
        nodes that must be set to their default value during conversion. Use
        lists self.links and self.defaults for this, respectively.
        """
        pass

    def convert(self,source,target,callback=None,usedefaults=True,convertlinkedata=True,matchednodes=None):
        """Converts source TypedStore object to target TypedStore object.
        This method performs a simple deep copy of all values, and then
        handles explicitly specified links between source and target nodes
        (which can be set by inheriting classes), and sets a list of target
        nodes to their defaults (this list is also specified by inheriting
        classes)."""
        if matchednodes is None: matchednodes = {}

        import xmlstore
        if isinstance(source,xmlstore.TypedStore): source = source.root
        if isinstance(target,xmlstore.TypedStore): target = target.root
        
        # Try simple deep copy: nodes with the same name and location in both
        # source and target store will have their value copied.
        target.copyFrom(source,matchednodes=matchednodes)

        # Handle explicit one-to-one links between source nodes and target nodes.
        for (sourcepath,targetpath) in self.links:
            sourcenode = source[sourcepath]
            if sourcenode is None:
                raise Exception('Cannot locate node "%s" in source.' % sourcepath)
            targetnode = target[targetpath]
            if targetnode is None:
                raise Exception('Cannot locate node "%s" in target.' % targetpath)
            targetnode.copyFrom(sourcenode,matchednodes=matchednodes)
            
        if matchednodes is not None:
            defaults = []
            for node in target.getDescendants():
                if node in matchednodes:
                    # Check if the source and target nodes were linked-in from another source.
                    # If so, and their versions differ, try to locate a convertor automatically,
                    # and perform the conversion.
                    srcnode = matchednodes[node]
                    if convertlinkedata and node.templatenode.hasAttribute('sourcepath') and srcnode.templatenode.hasAttribute('sourcepath'):
                        srcdir = os.path.dirname(node.templatenode.getAttribute('sourcepath'))
                        subsrcversion,subtgtversion = srcnode.templatenode.getAttribute('version'),node.templatenode.getAttribute('version')
                        if subsrcversion!=subtgtversion and srcdir==os.path.dirname(srcnode.templatenode.getAttribute('sourcepath')):
                            # Both the source and target node are linked-in from the same directory and differ in version.
                            # Try to locate a converter in the same directory to perform the conversion automatically.
                            conv = xmlstore.schemainfocache[srcdir].getConverter(subsrcversion,subtgtversion)
                            if conv is not None: conv.convert(srcnode,node,usedefaults=usedefaults,convertlinkedata=False,matchednodes=matchednodes)
                elif node.canHaveValue():
                    # Node was not matched in the source store, but it should have a value.
                    # Insert the default value.
                    #print '%s in version %s did not get a value from version %s' % (node,self.targetid,self.sourceid)
                    defaults.append('/'.join(node.location[len(target.location):]))
            
            # If the list with nodes that need a default value has not been built yet, use the current one.
            if usedefaults and self.__class__.defaults is None:
                self.__class__.defaults = defaults
            
        # Set target nodes to defaults where required.
        if self.defaults and usedefaults:
            for path in self.defaults:
                targetnode = target[path]
                if targetnode.hasValue(): continue
                sourcevalue = targetnode.getDefaultValue()
                #print 'Using default value for %s/%s: %s' % (self.targetid,path,str(sourcevalue))
                if sourcevalue is not None:
                    targetnode.setValue(sourcevalue)
                    if isinstance(sourcevalue,util.referencedobject): sourcevalue.release()

        # Do custom conversions
        self.convertCustom(source,target,callback)

    def convertCustom(self,source,target,callback=None):
        pass

class XmlConvertor(Convertor):
    """This class represents converters stored in XML format.
    """
    
    @staticmethod
    def createClasses(path):
        """Parses the specified XML file and creates from it classes for forward and backward conversion.
        If backward conversion is unavailable, None is returned for the backward conversion class.
        
        For performance reasons, the XML is only parsed up to the root node in order to read the source
        and target versions. The file is fully parsed only when converters of the created classes are
        actually instantiated.
        """
        rootname,rootattr = util.getRootNodeInfo(path)
        assert rootname=='converter','Root node is named "%s", but must be named "converter" for XML-based converters.' % rootname
        sourceid,targetid = rootattr.get('source'),rootattr.get('target')

        def createconvertor(sourceid,targetid):
            defaultname = re.sub('\W','_','Xml_Convertor_%s_%s' % (sourceid,targetid))
            attr = {'fixedsourceid':sourceid,
                    'fixedtargetid':targetid,
                    'path':path}
            #print 'Creating convertor class %s.' % (defaultname,)
            return type(str(defaultname),(XmlConvertor,),attr)
        fw = createconvertor(sourceid,targetid)
        bw = createconvertor(targetid,sourceid)
        return fw,bw
        
    @classmethod
    def initialize(cls):
        """Completely parses the XML file with conversion information, creating
        a list of links between source and target version, and compiling any
        custom conversion code on the fly.
        """
        #print 'Initializing converter %s.' % (cls.__name__,)
        xmlconvertor = xml.dom.minidom.parse(cls.path)
        root = xmlconvertor.documentElement
        assert root.localName=='converter','Root element of "%s" is called "%s", but root of converter xml must be called "converter".' % (cls.path,root.localName)
        sourceid,targetid = root.getAttribute('source'),root.getAttribute('target')
        reverse = targetid==cls.fixedsourceid
        customcodename = 'forward'
        if reverse: customcodename = 'backward'
        cls.defaultlinks,cls.customconversion = [],None
        for node in root.childNodes:
            if node.nodeType==node.ELEMENT_NODE and node.localName=='links':
                for linknode in node.childNodes:
                    if linknode.nodeType==linknode.ELEMENT_NODE and linknode.localName=='link':
                        cls.defaultlinks.append((linknode.getAttribute('source'),linknode.getAttribute('target')))
            elif node.nodeType==node.ELEMENT_NODE and node.localName=='custom':
                for customnode in node.childNodes:
                    if customnode.nodeType==customnode.ELEMENT_NODE and customnode.localName==customcodename:
                        for data in customnode.childNodes:
                            if data.nodeType==data.CDATA_SECTION_NODE: break
                        cls.customconversion = compile(data.nodeValue,cls.path,'exec')
        if reverse: cls.defaultlinks = [(targetpath,sourcepath) for (sourcepath,targetpath) in cls.defaultlinks]
    
    def __init__(self):
        if not hasattr(self,'defaultlinks'): self.initialize()
        Convertor.__init__(self)
        
    def registerLinks(self):
        self.links = self.defaultlinks
        
    def convertCustom(self,source,target,callback=None):
        if self.customconversion is not None: exec self.customconversion

class ConvertorChain(Convertor):
    """Generic class for multiple-step conversions.
    Conversion steps are specified at initialization as a list of convertors."""
    def __init__(self,chain):
        Convertor.__init__(self,chain[0].sourceid,chain[-1].targetid)
        self.chain = chain

    def convert(self,source,target,callback=None,matchednodes=None,**kwargs):
        temptargets = []
        nsteps = len(self.chain)
        if callback is not None:
            stepcallback = lambda progress,message: callback((istep+progress)/nsteps,message)
        else:
            stepcallback = None
            
        oldmatches = None
        for istep in range(nsteps-1):
            convertor = self.chain[istep]
            temptargetid = convertor.targetid
            if callback is not None: callback(float(istep)/nsteps,'converting to version "%s".' % temptargetid)
            if util.verbose: print 'Converting to temporary target "%s".' % temptargetid
            temptarget = source.fromSchemaName(temptargetid)
            temptargets.append(temptarget)
            curmatches = {}
            convertor.convert(source,temptarget,callback=stepcallback,matchednodes=curmatches,**kwargs)
            if oldmatches is not None:
                oldmatches = dict([(targetnode,oldmatches[sourcenode]) for targetnode,sourcenode in curmatches.iteritems() if sourcenode in oldmatches])
            else:
                oldmatches = curmatches
            source = temptarget

        convertor = self.chain[-1]
        istep = nsteps-1
        if callback is not None: callback(float(istep)/nsteps,'converting to version "%s".' % convertor.targetid)
        if util.verbose: print 'Converting to final target "%s".' % convertor.targetid
        curmatches = {}
        convertor.convert(source,target,callback=stepcallback,matchednodes=curmatches,**kwargs)
        oldmatches = dict([(targetnode,oldmatches[sourcenode]) for targetnode,sourcenode in curmatches.iteritems() if sourcenode in oldmatches])
        for temptarget in temptargets: temptarget.release()

        if matchednodes is not None: matchednodes.update(oldmatches)
