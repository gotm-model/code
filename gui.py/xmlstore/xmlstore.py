# This module contains a class for storing data in an XML tree:
#
#   TypedStore: this class combines a XML schema file (XSD-like) with an XML tree in
#   which values are stored.
#   Value types are obtained from the schema definition. Additionally, the TypedStore
#   supports (conditional) hiding of nodes, notifications before/after changing of node
#   values and node visiblity, a set of default values, arbitrary data streams that are
#   stored aside the XML value tree, encapsulating containers such as ZIP, and many other features.

# Import modules from standard Python library
import re, xml.dom.minidom, os, StringIO, codecs

# Import own custom modules
import util, datatypes

verbose = False

class Schema:
    """Class for managing XML-based schemas, used to define TypedStore objects.
    Supports caching of schemas (based on file path), parsing of schemas
    (i.e., inserting linked templates, resolving dependencies), and provides
    access to the main properties (version and root of the XML tree).
    """
    cache = {}
    knownpaths = {}
    
    @staticmethod
    def create(source,cache=True):
        """Creates a schema from file or DOM tree object. If a file path is
        provided, the created schema is cached, and returned on subsequent
        request for schemas with the same path.
        """
        if cache and isinstance(source,basestring):
            path = os.path.abspath(source)
            if path in Schema.cache:
                #print 'Found schema "%s" in cache.' % path
                schema = Schema.cache[path]
            else:
                schema = Schema(source)
                Schema.cache[path] = schema
        else:
            schema = Schema(source)
        return schema
    
    def __init__(self,source,sourceisxml=False):
        """Initializes a new Schema from the specified source.
        A source can be a path to an XML file, a string containing XML or a xml.dom.minidom DOM object
        If it is a a string containing XML, argument "sourceisxml" must be set to True;
        otherwise the source is interpreted as a path to an XML file.
        """
        
        # The template can be specified as a DOM object, or as string (i.e. path to XML file)
        path = ''
        if isinstance(source,basestring):
            # The provided schema source is a string. It can be a path to a file or plain XML.
            if not sourceisxml:
                # The provided source is a path.
                path = os.path.abspath(source)
                if not os.path.isfile(path):
                    raise Exception('XML schema file "%s" does not exist.' % path)
                self.dom = xml.dom.minidom.parse(path)
            else:
                # The provided source is a string containing XML.
                self.dom = xml.dom.minidom.parseString(source)
        elif isinstance(source,xml.dom.minidom.Document):
            # The provided source is a DOM object
            self.dom = source
        else:
            assert False, 'First argument (the schema source) must either be a string or an XML DOM tree. Received argument: %s.' % str(source)
            
        # In addition to "element" nodes, a Schema can contains "link" nodes that either reference an
        # "template" node within the same schema, or a the root node of another XML file.
        # Below all link nodes are replaced by the their target.
        Schema.resolveLinks(self.dom,path)

        # For every variable: build a list of variables/folders that depend on its value.
        self.buildDependencies()

    @staticmethod
    def resolveLinks(dom,sourcepath):
        # Resolve links to external documents
        links = dom.getElementsByTagName('link')
        templates = dict([(node.getAttribute('id'),node) for node in dom.getElementsByTagName('template')])
        for link in links:
            assert link.hasAttribute('path') or link.hasAttribute('template'), 'Link node does not have "path" or "template" attribute.'
            
            if link.hasAttribute('path'):
                # We need to copy from an external XML document.
                linkedpath = link.getAttribute('path')
                while True:
                    match = re.match('\[(\w+)]',linkedpath)
                    if match is None: break
                    exp = match.group(1)
                    assert exp in Schema.knownpaths, 'Do not know the location of "%s" in linked path "%s".' % (exp,linkedpath)
                    linkedpath = os.path.join(linkedpath[:match.start(0)],Schema.knownpaths[exp],linkedpath[match.end(0)+1:])
                linkedpath = os.path.abspath(os.path.join(os.path.dirname(sourcepath),linkedpath))
                if not os.path.isfile(linkedpath):
                    raise Exception('Linked XML schema file "%s" does not exist.' % linkedpath)
                childdom = xml.dom.minidom.parse(linkedpath)
                Schema.resolveLinks(childdom,linkedpath)
                templateroot = childdom.documentElement
            else:
                # We need to copy from an internal template.
                templateid = link.getAttribute('template')
                assert templateid in templates, 'Cannot find template "%s".' % templateid
                templateroot = templates[templateid]
                
            # Copy nodes
            linkparent = link.parentNode
            if link.hasAttribute('skiproot'):
                for ch in templateroot.childNodes:
                    newnode = util.copyNode(ch,linkparent,targetdoc=dom)
            else:
                newnode = util.copyNode(templateroot,linkparent,targetdoc=dom,name='element',before=link)
                
                # Copy attributes and children of link node to new node.
                for key in link.attributes.keys():
                    if key not in ('path','template','skiproot'):
                        newnode.setAttribute(key,link.getAttribute(key))
                for ch in link.childNodes:
                    util.copyNode(ch,newnode,targetdoc=dom)
                    
            # Remove link node
            linkparent.removeChild(link)
        return len(links)>0
        
    def getRoot(self):
        """Returns the root of the schema DOM tree."""
        return self.dom.documentElement
        
    def getVersion(self):
        """Returns the schema version string."""
        return self.dom.documentElement.getAttribute('version')

    # buildDependencies: for every variable node, this creates lists of dependent nodes
    # (i.e. folders and variables that have one or more conditions that depend on the
    # variable under investigation). Essentially we convert lists of dependencies ('servant'-centric)
    # into lists of dependent nodes ('controller'-centric). We need the latter in order to selectively
    # re-check conditions (and hide/show corresponding nodes) after the value of
    # a dependency ('controller') changes.
    def buildDependencies(self,root=None,curpath='',curowner=None):
        if root is None: root=self.dom.documentElement
        for ch in root.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE:
                if ch.localName=='element':
                    childcurpath = curpath+'/'+ch.getAttribute('name')
                    self.buildDependencies(root=ch,curpath=childcurpath,curowner=ch)
                    if ch.hasAttribute('unit'):
                        unit = ch.getAttribute('unit')
                        if unit[0]=='[' and unit[-1]==']':
                            unitnode,relcurpath = self.getReversePath(ch,unit[1:-1],absourcepath=childcurpath)
                            self.registerDependency(unitnode,relcurpath,'unit')
                elif ch.localName=='condition':
                    assert not curowner.hasAttribute('maxoccurs'), 'Currently conditions on optional nodes are not supported.'
                    if ch.hasAttribute('source'): continue
                    if ch.hasAttribute('variable'):
                        # Get the referenced node, and the relative path from there to here.
                        depnode,relcurpath = self.getReversePath(curowner,ch.getAttribute('variable'),absourcepath=curpath)

                        # Register the current node with the referenced node,
                        # so that a change in the referenced node can trigger
                        # an update in the visibility of the current node.
                        self.registerDependency(depnode,relcurpath,'visibility')
                        
                    self.buildDependencies(root=ch,curpath=curpath,curowner=curowner)
                elif ch.localName=='options':
                    curowner.setAttribute('hasoptions',True)
                    
    # getTemplateNode: obtains template node at given path
    # (path specification consists of array of node ids)
    def getNodeFromPath(self,path,root=None):
        """Obtains DOM node in schema at specified path. If a reference node
        is provided, the path is assumed to be relative to the reference node.
        If no reference node is provided, the path is assumed absolute, that is,
        relative to the schema root element."""
        if root is None: root=self.dom.documentElement
        for childname in path:
            if childname=='..':
                assert not root.isSameNode(self.dom.documentElement)
                root = root.parentNode
            elif childname!='' and childname!='.':
                for root in root.childNodes:
                    if root.nodeType==root.ELEMENT_NODE and root.localName=='element' and root.getAttribute('name')==childname:
                        break
                else:
                    return None
        return root

    # getPathFromNode: obtains path specification for given template node
    # (path specification consists of node ids with slash separators)
    def getPathFromNode(self,node):
        """Gets the absolute path of the specified node, as an array of path
        components. The absolute path is defined as the path relative to the
        schema root element.
        """
        path = []
        while node.parentNode.parentNode is not None:
            path.insert(0,node.getAttribute('name'))
            node = node.parentNode
        return path

    def getReversePath(self,sourcenode,targetpath,absourcepath=None):
        """Takes a schema reference node, and the path of another node which
        may be relative to the reference node, and returns the referenced target
        node plus the (potentially relative) path from the target node to the
        source node.
        
        The absolute path to the source node may be provided; this saves
        computational effort only.
        """
        if absourcepath is None: '/'.join(self.getPathFromNode(sourcenode))
        
        refnode = None
        if targetpath[0]!='/': refnode = sourcenode.parentNode
        splittargetpath = targetpath.split('/')
        targetnode = self.getNodeFromPath(splittargetpath,root=refnode)
        assert targetnode is not None, 'Cannot locate target node "%s" for node "%s".' % (targetpath,absourcepath)
        
        abstargetpath = self.getPathFromNode(targetnode)
        assert len(abstargetpath)!=0, 'Target node "%s" for node "%s" corresponds to the root of the DOM tree. This is not allowed.' % (targetpath,absourcepath)
        if '.' in splittargetpath or '..' in splittargetpath:
            # Find a relative path from the referenced node to the current node.
            abstargetpath.pop()    # The reference node will be the parent of the specified node
            abscurpath = [n for n in absourcepath.split('/') if n!='']
            istart = 0
            while istart<len(abstargetpath) and istart<len(abscurpath) and abstargetpath[istart]==abscurpath[istart]: istart+=1
            return targetnode,(len(abstargetpath)-istart)*'../'+'/'.join(abscurpath[istart:])
        else:
            # Use the absolute path of the current node.
            return targetnode,absourcepath
            
    def registerDependency(self,node,dependantnodepath,type):
        """For the given template node, registers that another node at the
        specified (potentially relative) path depends on it.
        """
        deplist = util.findDescendantNode(node,['dependentvariables'],create=True)
        depnode = self.dom.createElementNS(deplist.namespaceURI,'dependentvariable')
        depnode.setAttribute('path',dependantnodepath)
        depnode.setAttribute('type',type)
        deplist.appendChild(depnode)
        
    def createDocumentation(self,fout=None,showhidden=False):
        # Get maximum depth of the tree
        def getmaxdepth(node):
            maxdepth = 1
            for ch in node.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='element':
                    maxdepth = max(maxdepth,1+getmaxdepth(ch))
            return maxdepth
        maxdepth = getmaxdepth(self.dom.documentElement)

        # Function for printing a node and its children
        def printnode(fout,node,maxdepth,nextid,depth=0,showhidden=False):
            # Print info on the node itself
            if showhidden or not node.hasAttribute('hidden'):
                fout.write('\t<tr valign="top">')
                for i in range(depth): fout.write('<td>&nbsp;</td>')
                fout.write('<td colspan="%i">%s</td>' % (maxdepth-depth,node.getAttribute('name')))
                if node.hasAttribute('type'):
                    fout.write('<td>%s</td>' % node.getAttribute('type'))
                else:
                    fout.write('<td>&nbsp;</td>')

                if node.hasAttribute('description'):
                    text = node.getAttribute('description')
                elif node.hasAttribute('label'):
                    text = node.getAttribute('label')
                else:
                    text = '&nbsp;'

                opts = []
                for ch in node.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='options':
                        for optch in ch.childNodes:
                            if optch.nodeType==ch.ELEMENT_NODE and optch.localName=='option':
                                if optch.hasAttribute('description'):
                                    label = optch.getAttribute('description')
                                elif optch.hasAttribute('label'):
                                    label = optch.getAttribute('label')
                                else:
                                    label = optch.getAttribute('value')
                                opts.append((optch.getAttribute('value'),label))
                if opts:
                    text += ', <a href="javascript:showhide(\'table%i\')">supported values</a>\n' % nextid
                    text += '<table id="table%i" cellspacing="0" style="display:none">\n' % nextid
                    text += '<tr><th>value</th><th>description</th></tr>\n'
                    text += ''.join(['<tr><td>%s</td><td>%s</td></tr>\n' % o for o in opts])
                    text += '</table>\n'

                fout.write('<td>%s</td></tr>\n' % text)
                nextid += 1
                
            # Print info on the children.
            for ch in node.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='element':
                    nextid = printnode(fout,ch,maxdepth,nextid,depth=depth+1,showhidden=showhidden)
                    
            return nextid
                        
        # Print all information.
        if fout is None: fout = sys.stdout
        fout.write('<table cellspacing="0">\n')
        for i in range(maxdepth-1): fout.write('<col width="20">')
        fout.write('\t<tr><th colspan="%i">node name</th><th>data type</th><th>description</th></tr>\n' % maxdepth)
        printnode(fout,self.dom.documentElement,maxdepth,0,showhidden)
        fout.write('</table>\n')

import UserDict
class ShortcutDictionary(UserDict.DictMixin):
    @staticmethod
    def fromDirectory(path,**kwargs):
        cache = ShortcutDictionary()
        cache.addDirectory(path,**kwargs)
        return cache
    
    def __init__(self):
        self.links = {}
        
    def __getitem__(self,item):
        return self.links[item]
        
    def __setitem__(self,item,value):
        self.links[item] = value
        
    def __delitem__(self,item):
        del self.links[item]
        
    def keys(self):
        return self.links.keys()
        
    def addDirectory(self,path,extension='.xml'):
        for templatename in os.listdir(path):
            fullpath = os.path.join(path,templatename)
            if os.path.isfile(fullpath):
                (basename,ext) = os.path.splitext(templatename)
                if ext==extension:
                    self.links[basename] = fullpath

class TypedStoreInterface:
    """This class provides an interface to a TypedStore object. The interface
    can be configured at initialization to (1) hide nodes with the "hidden"
    property set and (2) to omit nodes with the "grouponly" attribute set, replacing
    them instead with the node's children.
    """
    def __init__(self,store,showhidden=True,omitgroupers=False,processDefaultChange=0):
        self.showhidden = showhidden
        self.omitgroupers = omitgroupers
        self.blockNotifyOfHiddenNodes = (not showhidden)
        
        # How to process changes in the default node value
        # -1: never report
        #  0: report only if no explicit value is set (i.e., the default is used)
        #  1: always report
        self.processDefaultChange = processDefaultChange

        self.eventhandlers = {}

        store.connectInterface(self)
        
    def unlink(self):
        assert self.eventhandlers is not None, 'unlink called on TypedStoreInterface for the second time.'
        self.eventhandlers = None

    def getChildCount(self,node):
        """Returns the number of children of the specified node."""
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        childcount = 0
        for child in node.children:
            if child.visible or self.showhidden:
                if self.omitgroupers and child.grouponly:
                    childcount += self.getChildCount(child)
                else:
                    childcount += 1
        return childcount

    def getChildren(self,node):
        """Returns a list of children of the specified node."""
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        res = []
        for child in node.children:
            if child.visible or self.showhidden:
                if self.omitgroupers and child.grouponly:
                    res += self.getChildren(child)
                else:
                    res.append(child)
        return res

    def getParent(self,node):
        """Returns the parent of the specified node."""
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        if not self.omitgroupers: return node.parent
        par = node.parent
        while par.grouponly: par = par.parent
        return par

    def getChildByIndex(self,node,index,returnindex=False):
        """Gets the child of the specified node, at the specified index."""
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        for child in node.children:
            if child.visible or self.showhidden:
                if self.omitgroupers and child.grouponly:
                    index = self.getChildByIndex(child,index,returnindex=True)
                    if not isinstance(index,int): return index
                else:
                    if index==0: return child
                    index -= 1
        if returnindex:
            return index
        else:
            return None

    def getOwnIndex(self,node):
        """Returns the index of the specified node in its list of siblings."""
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        ind = 0
        par = node.parent
        if self.omitgroupers and par.grouponly: ind = self.getOwnIndex(par)
        for (isib,sib) in enumerate(par.children):
            if sib is node or isib==node.futureindex: break
            if sib.visible or self.showhidden:
                if self.omitgroupers and sib.grouponly:
                    ind += self.getChildCount(sib)
                else:
                    ind += 1
        else:
            assert node.futureindex is not None, 'Could not find node "%s" in children of supposed parent, but future index was also not set. Data: %s' % (node,node.valuenode.toxml('utf-8'))
            assert node.futureindex==len(par.children), 'Could not find node "%s" in children of supposed parent, but future index (%i) was also not set to tailing position (%i).' % (node,node.futureindex,len(par.children))
        return ind

    def getDepth(self,node):
        """Gets the maximum depth of the tree of descendants of the specified node."""
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        childmax = 0
        for child in self.getChildren(node):
            curchilddepth = self.getDepth(child)
            if curchilddepth>childmax: childmax = curchilddepth
        return childmax+1

    def toHtml(self,node,xmldocument,totaldepth,level=0,hidedefaults=False):
        """Returns a list of HTML "tr" nodes that describe the specified node
        and its children."""
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        res = []

        tr = None
        if level>=0:
            tr = xmldocument.createElement('tr')

            for i in range(level):
                td = xmldocument.createElement('td')
                tr.appendChild(td)

            td1 = xmldocument.createElement('td')
            templatenode = node.templatenode
            td1.appendChild(xmldocument.createTextNode(node.getText(detail=1)))
            if level+1<totaldepth:
                td1.setAttribute('colspan',unicode(totaldepth-level))
            tr.appendChild(td1)

            td2 = xmldocument.createElement('td')
            if node.canHaveValue():
                val = node.getValueAsString(usedefault=True)
            else:
                val = ' '
            td2.appendChild(xmldocument.createTextNode(val))
            tr.appendChild(td2)

            res.append(tr)

        childtrs = []
        for child in self.getChildren(node):
            childnodes = self.toHtml(child,xmldocument,totaldepth,level+1,hidedefaults=hidedefaults)
            childtrs += childnodes
        res += childtrs

        if tr is not None and hidedefaults:
            isdefault = True
            if node.canHaveValue() and not node.hasDefaultValue():
                isdefault = False
            else:
                for childtr in childtrs:
                    if not childtr.hasAttribute('default'):
                        isdefault = False
                        break
            if isdefault:
                tr.setAttribute('style','display:none')
                tr.setAttribute('default','')

        return res

    # ---------------------------------------------------------------------------
    # Functions for connecting to events
    # ---------------------------------------------------------------------------

    def connect(self,eventname,handler):
        assert eventname in ('beforeVisibilityChange','afterVisibilityChange','afterStoreChange','beforeChange','afterChange'), 'attempt to register for unknown event "%s".' % eventname
        assert eventname not in self.eventhandlers, 'handler for event "%s" exists.' % eventname
        self.eventhandlers[eventname] = handler

    def addChangeHandler(self,callback):
        assert not self.changehandlers, 'change handler exists'
        self.changehandlers.append(callback)

    # ---------------------------------------------------------------------------
    # Functions called by store when events occur
    # ---------------------------------------------------------------------------

    def beforeVisibilityChange(self,node,shownew,showhide):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        #print 'beforeVisibilityChange'
        self.upcomingvizchange = node
        if 'beforeVisibilityChange' not in self.eventhandlers: return
        if self.blockNotifyOfHiddenNodes and self.getParent(node).isHidden(): return
        if self.blockNotifyOfHiddenNodes and (not showhide) and node.isHidden(): return
        if self.omitgroupers and node.grouponly:
            children = self.getChildren(node)
            if len(children)==0: return
            self.eventhandlers['beforeVisibilityChange'](children,shownew,showhide)
        else:
            self.eventhandlers['beforeVisibilityChange']((node,),shownew,showhide)

    def afterVisibilityChange(self,node,shownew,showhide):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        assert node==self.upcomingvizchange, 'The node supplied to afterVisibilityChange (%s) was not the last one supplied to beforeVisibilityChange (%s).' % (node,self.upcomingvizchange)
        #print 'afterVisibilityChange'
        self.upcomingvizchange = None
        if 'afterVisibilityChange' not in self.eventhandlers: return
        if self.blockNotifyOfHiddenNodes and self.getParent(node).isHidden(): return
        if self.blockNotifyOfHiddenNodes and (not showhide) and node.isHidden(): return
        if self.omitgroupers and node.grouponly:
            children = self.getChildren(node)
            if len(children)==0: return
            self.eventhandlers['afterVisibilityChange'](children,shownew,showhide)
        else:
            self.eventhandlers['afterVisibilityChange']((node,),shownew,showhide)

    def afterStoreChange(self):
        #print 'afterStoreChange'
        if 'afterStoreChange' not in self.eventhandlers: return
        self.eventhandlers['afterStoreChange']()

    def onBeforeChange(self,node,newvalue):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        #print 'onBeforeChange'
        if 'beforeChange' not in self.eventhandlers: return True
        if node.isHidden() and self.blockNotifyOfHiddenNodes: return True
        return self.eventhandlers['beforeChange'](node,newvalue)

    def onChange(self,node,feature):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        #print 'onChange'
        if 'afterChange' not in self.eventhandlers: return
        if node.isHidden() and self.blockNotifyOfHiddenNodes: return
        self.eventhandlers['afterChange'](node,feature)

    def onDefaultChange(self,node,feature):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        #print 'onDefaultChange'
        if self.processDefaultChange==1 or (self.processDefaultChange==0 and not node.hasValue()):
            self.onChange(node,feature)

class Node:
    def __init__(self,controller,templatenode,valuenode,location,parent):
        assert templatenode.hasAttribute('name'),'Schema node %s lacks "name" attribute.' % location

        self.controller = controller
        self.templatenode = templatenode
        self.valueroot = valuenode
        self.valuenode = valuenode
        self.location = tuple(location)
        self.parent = parent
        self.children = []
        self.futureindex = None
        self.visible = (not self.templatenode.hasAttribute('hidden'))
        self.grouponly = self.templatenode.hasAttribute('grouponly')

        # Build a dictionary with all child value nodes
        valuechildren = {}
        if self.valueroot is not None:
            for ch in self.valueroot.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE:
                    valuechildren.setdefault(ch.localName,[]).append(ch)

        canhavechildren = False
        for templatechild in self.templatenode.childNodes:
            if templatechild.nodeType==templatechild.ELEMENT_NODE and templatechild.localName=='element':
                childid = templatechild.getAttribute('name')
                canhavechildren = True
                
                # Get all value nodes that correspond to the current template child.
                curvaluechildren = valuechildren.pop(childid,())
                if not templatechild.hasAttribute('maxoccurs'):
                    # The node always occurs exactly one time.
                    assert len(curvaluechildren)<=1, 'Node "%s" can at most occur 1 time, but it occurs %i times.' % (childid,len(curvaluechildren))
                    if len(curvaluechildren)==0: curvaluechildren = (None,)
                else:
                    # The node may occur zero or more times.
                    maxoccurs = int(templatechild.getAttribute('maxoccurs'))
                    assert len(curvaluechildren)<=maxoccurs, 'Node "%s": number of children (%i) is greater than the imposed maximum (%i).' % (childid,len(curvaluechildren),maxoccurs)

                # Create nodes for all value nodes found.                     
                childloc = list(self.location) + [childid]
                for valuechild in curvaluechildren:
                    self.children.append(Node(self.controller,templatechild,valuechild,childloc,parent=self))

        # For nodes that can have children as well as a value, the value is stored in a
        # child node. This child node carries the same name as the parent.
        if canhavechildren and self.canHaveValue():
            curvaluechildren = valuechildren.pop(self.location[-1],(None,))
            assert len(curvaluechildren)<=1, 'Value node (%s) can at most occur 1 time below %s, but it occurs %i times.' % (self.location[-1],self.location,len(curvaluechildren))
            self.valuenode = curvaluechildren[0]

        # Check for existing value nodes that are not in the template.
        for childid,childnodes in valuechildren.iteritems():
            print 'WARNING! Value "%s" below "%s" was unexpected and will be ignored.' % (childid,self.location)
            for ch in childnodes: self.valueroot.removeChild(ch)

    def __str__(self):
        """Returns a string representation of the node.
        """
        return str(self.location)

    def destroy(self):
        """Deallocates all variables of the node, breaking circular
        references.
        """
        for ch in self.children:
            if ch is not None: ch.destroy()
        self.location = ()
        self.children = []
        self.parent = None
        self.templatenode = None
        self.valueroot = None
        self.valuenode = None
        self.controller = None
        
    def isValid(self):
        """Determines whether the node is valid. Returns False only if
        "destroy" has been called.
        """
        return self.controller != None

    def getValue(self,usedefault=False):
        """Returns the typed value of the node. This function returns
        None if the node does not have a value yet, and throws an error
        if the node cannot have a value (i.e., it is a container only).
        """
        value = None
        if self.valuenode is not None:
            valuetype = self.getValueType(returnclass=True)
            value = valuetype.load(self.valuenode,self.controller.context,self.templatenode)
        if value is None and usedefault: value = self.getDefaultValue()
        return value
        
    def hasValue(self):
        if self.valuenode is None: return False
        value = self.getValue()
        if value is None: return False
        if isinstance(value,util.referencedobject): value.release()
        return True

    def getDefaultValue(self):
        """Returns the default value of the node. This function returns
        None if no default value if available, which applies also if
        a default store has not been specified.
        """
        defaultstore = self.controller.defaultstore
        if defaultstore is None: return None
        defaultnode = defaultstore.mapForeignNode(self)
        if defaultnode is None: return None
        return defaultnode.getValue(usedefault=True)
        
    def hasDefaultValue(self):
        value = self.getValue()
        if value is None: return True
        defvalue = self.getDefaultValue()
        hasdef = value==defvalue
        if isinstance(value,   util.referencedobject): value.release()
        if isinstance(defvalue,util.referencedobject): defvalue.release()
        return hasdef

    def setValue(self,value):
        """Sets the typed value of the node. Returns True if the value
        of the node was changed, False if it was not changed. Changes
        may be prevented by an attached interface disallowing the change.
        """
        if value is None:
            self.clearValue()
            return

        curval = self.getValue()
        changed = False
        if curval!=value:
            if self.controller.onBeforeChange(self,value):
                valuetype = self.getValueType(returnclass=True)
                if not isinstance(value,valuetype): value = valuetype(value)
                if self.valuenode is None: self.createValueNode()
                changed = value.save(self.valuenode,self.controller.context)
                self.controller.onChange(self,'value')
        if isinstance(curval,util.referencedobject): curval.release()
        return changed

    def clearValue(self,recursive=False,skipreadonly=False,deleteclones=True):
        """Clears the value of the node.
        
        If recursive=True, also clears the value of the descendant nodes; if
        additionally deleteclones=True, all optional descendant nodes are
        deleted completely.
        
        If skipreadonly is True, the read-only status of nodes if
        respected, implying that their value is not cleared if they have
        the read-only attribute.
        """
        # First clear children.
        cleared = True
        if recursive:
            if deleteclones: self.removeAllChildren(optionalonly=True)
            for ch in self.children:
                if not ch.clearValue(recursive=True,skipreadonly=skipreadonly,deleteclones=deleteclones):
                    cleared = False
            
        # Do not clear if (1) it is already cleared (result: success), (2) it is
        # read-only and the user wants to respect that (result: failure),
        # (3) it is the root node (result: failure), or (4) clearing the child nodes failed.
        if self.valuenode is None: return True
        if (skipreadonly and self.isReadOnly()) or self.parent is None or not cleared: return False
        
        # Clear if (1) this node can have no value - it must occur, and (2) the attached interfaces approve.
        if (not self.canHaveClones()) and self.controller.onBeforeChange(self,None):
            self.valuenode.parentNode.removeChild(self.valuenode)
            if (self.valueroot==self.valuenode): self.valueroot = None
            self.valuenode = None
            self.controller.onChange(self,'value')
            return True
        else:
            return False

    def getValueAsString(self,addunit = True,usedefault = False):
        """Returns a user-readable string representation of the value of the node.
        """
        # Get the value -  return an empty string if no value is set.
        value = self.getValue(usedefault=usedefault)
        if value is None: return ''

        # Get the XML template node describing the data type, and the Python class representing the type.
        templatenode = self.templatenode
        valuetype = self.getValueType(returnclass=True)
        
        # Initially we do not have a string representation
        strvalue = None

        # First look if the value was chosen from a list of predefined options.
        if templatenode.hasAttribute('hasoptions'):
            # Get label of currently selected option
            optionsroot = util.findDescendantNode(templatenode,['options'])
            for ch in optionsroot.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                    chvalue = valuetype.fromXmlString(ch.getAttribute('value'),{},templatenode)
                    if value==chvalue:
                        # We found the currently selected option; its label will serve as displayed value.
                        if ch.hasAttribute('label'):
                            strvalue = ch.getAttribute('label')
                        else:
                            strvalue = ch.getAttribute('value')
                        break

        # If we do not have a string representation yet, then cast the data to the internally
        # registered type, and let that figure out the best pretty string.
        if strvalue is None:
            if not isinstance(value,valuetype): value = valuetype(value)
            strvalue = value.toPrettyString()
            
        # Release the reference to the value if needed.
        if isinstance(value,util.referencedobject): value.release()

        # Append unit specifier (if available)
        if addunit:
            unit = self.getUnit()
            if unit is not None: strvalue += ' ' + unit

        return strvalue

    def addChild(self,childname,position=None,id=None):
        """Adds a new child node; this child node must be optional as
        defined in the template with minoccurs/maxoccurs attributes.
        
        The new child node is by default appended to the list of existing
        nodes with the same name, or inserted at the specified "position".
        """
        index = -1
        templatenode = None

        # First see of already one instance of this child is in the tree; that makes finding the position easy.
        existingcount = 0
        for curindex,child in enumerate(self.children):
            if child.location[-1]==childname:
                assert id is None or child.getSecondaryId()!=id, 'Child node with the id "%s" already exists below %s.' % (id,str(self.location))
                index = curindex
                templatenode = child.templatenode
                existingcount += 1
            elif index!=-1:
                # We are at the end of the list of nodes with the specified name. Stop.
                break
                
        # If no insert position was specified, append at the end
        if position is None: position = existingcount
        
        if index!=-1:
            # Found an existing node with this name
            assert position>=0, 'Position must be positive, but is %i. Use position=None to append to the end.' % position
            assert position<=existingcount, 'Cannot insert child "%s" at position %i, because only %i nodes exist so far.' % (childname,position,existingcount)
            index = index+1-existingcount+position
        else:
            # Node with this name not yet in tree.
            assert position==0, 'Cannot insert child "%s" at position %i, because no node wih this name exists so far.' % (childname,position)

            # Enumerate over all template children of the parent we want to insert below.
            # Store a list of names of children that precede the node to be inserted.
            predecessors = []
            for templatenode in self.templatenode.childNodes:
                if templatenode.nodeType==templatenode.ELEMENT_NODE and templatenode.localName=='element':
                    childid = templatenode.getAttribute('name')
                    if childid==childname: break
                    predecessors.append(childid)
            else:
                # Could not find the specified child in the template.
                return None

            # Enumerate over all actual children until we reach the point where the child should be inserted.
            index = 0
            for child in self.children:
                curname = child.location[-1]
                while len(predecessors)>0 and curname!=predecessors[0]:
                    predecessors.pop(0)
                if len(predecessors)==0: break
                index += 1
                
        # Ensure the parent to insert below has a value node
        # (we need to insert the value node below it to give the child life)
        self.createValueNode(rootonly=True)
        
        # Find the XML document
        doc = self.valueroot
        while doc.parentNode is not None: doc=doc.parentNode
        assert doc.nodeType==doc.DOCUMENT_NODE, 'Could not find DOM document node. Node "%s" does not have a parent.' % doc.tagName

        # Create the value node for the current child
        node = doc.createElementNS(self.valueroot.namespaceURI,childname)
        if id is not None: node.setAttribute('id',id)
        
        # Insert the value node
        if position>=existingcount:
            valueroot = self.valueroot.appendChild(node)
        else:
            valueroot = self.valueroot.insertBefore(node,self.children[index].valueroot)
            
        # Create the child (template + value)
        child = Node(self.controller,templatenode,valueroot,list(self.location)+[childname],parent=self)
        assert child.canHaveClones(), 'Cannot add another child "%s" because there can exist only one child with this name.' % childname

        # Set the node visibility before notifying anyone of its presence.
        # For this to work under all circumstances, we need to [temporarily] add the new child.
        self.children.insert(index,child)
        child.updateVisibility(recursive=True,notify=False)
        self.children.pop(index)
        
        # Insert the child, and notify attached interfaces.
        child.futureindex = index
        self.controller.beforeVisibilityChange(child,True,False)
        self.children.insert(index,child)
        self.controller.afterVisibilityChange(child,True,False)
        child.futureindex = None
        
        # Return the newly inserted child.
        return child
        
    def createValueNode(self,rootonly=False):
        """Creates the (empty) value node, and creates value nodes for
        all ancestors that lacks a value node as well.
        """
        if self.valuenode is not None or (rootonly and self.valueroot is not None): return
        assert rootonly or self.canHaveValue(),'Asked to create value node for %s, but this node cannot have a value.' % (str(self.location),)

        # Build a list of all ancestors that do not have a value root yet.
        parents = []
        root = self
        while root.valueroot is None:
            parents.insert(0,root)
            root = root.parent
        valueroot = root.valueroot
        
        # Find the XML document for values.
        doc = valueroot
        while doc.parentNode is not None: doc=doc.parentNode
        assert doc.nodeType==doc.DOCUMENT_NODE, 'Could not find DOM document node needed to create %s. Node "%s" does not have a parent.' % (location,doc.tagName)

        # Create value roots for all ancestors that lack one.
        for par in parents:
            par.valueroot = doc.createElementNS(valueroot.namespaceURI,par.getId())
            valueroot.appendChild(par.valueroot)
            valueroot = par.valueroot

        if self.canHaveValue() and self.canHaveChildren():
            # This node can have a value as well as children, and therefore needs a
            # separate value node.
            if rootonly: return
            self.valuenode = doc.createElementNS(self.valueroot.namespaceURI,self.getId())
            self.valueroot.appendChild(self.valuenode)
        else:
            # This node uses the value root for storing its value.
            self.valuenode = self.valueroot
            
        import xml.dom
        assert isinstance(self.valueroot,xml.dom.Node),'Value root is not of type xml.dom.Node. Value = %s' % (str(self.valueroot),)
        assert isinstance(self.valuenode,xml.dom.Node),'Value node is not of type xml.dom.Node. Value = %s' % (str(self.valuenode),)
        
    def getChildById(self,childname,id,create=False):
        """Gets an optional node (typically a node that can occur more than once)
        by its identifier. If the it does not exist yet, and create is True,
        the requested node is created (and intermediate nodes as well).
        """
        for child in self.children:
            if child.location[-1]==childname and child.getSecondaryId()==id:
                break
        else:
            if not create: return None
            child = self.addChild(childname,id=id)
        return child

    def getChildByNumber(self,childname,index,create=False):
        """Gets an optional node (typically a node that can occur more than once)
        by its number. If the it does not exist yet, and create is True,
        the requested node is created (and intermediate nodes as well).
        """
        curindex = 0
        for child in self.children:
            if child.location[-1]==childname:
                if curindex==index: break
                curindex += 1
        else:
            if not create: return None
            for ichild in range(index-curindex+1):
                child = self.addChild(childname)
        return child
        
    def removeChild(self,childname,id):
        """Removes an optional child node with the specified name and
        id. An id can either be the number (int) of the child node in the list
        with children of that name, or the id (string) set in its "id"
        child node.
        """
        assert isinstance(id,int) or isinstance(id,basestring), 'Specified id must be an integer or a string.'
        if isinstance(id,int):
            return self.removeChildren(childname,id,id)
        else:
            for child in reversed(self.children):
                if child.location[-1]==childname and child.getSecondaryId()==id:
                    self.removeChildNode(child)
                    break
            else:
                assert False, 'Cannot find child "%s" with id "%s".' % (childname,id)
                
    def removeChildren(self,childname,first=0,last=None):
        """Removes a (range of) optional child nodes with the specified name.
        If the last number to remove is not specified, nodes will be removed
        till the end.
        """
        ipos = 0
        ichildpos = -1
        while ipos<len(self.children):
            child = self.children[ipos]
            if child.location[-1]==childname:
                assert child.canHaveClones(),'Cannot remove child "%s" because it must occur exactly one time.' % childname
                ichildpos += 1
                if last is not None and ichildpos>last: return
                if ichildpos>=first:
                    self.removeChildNode(child,ipos)
                    ipos -= 1
            ipos += 1
            
    def removeAllChildren(self,optionalonly=True):
        """Removes all optional child nodes. The "optionalonly" argument
        is used internally only to remove every single descendant
        of an optional node.
        """
        for ipos in range(len(self.children)-1,-1,-1):
            child = self.children[ipos]
            if (not optionalonly) or child.canHaveClones():
                self.removeChildNode(child,ipos)

    def removeChildNode(self,node,pos=None):
        if pos is None: pos = self.children.index(node)
        node.removeAllChildren(optionalonly=False)
        self.controller.beforeVisibilityChange(node,False,False)
        self.children.pop(pos)
        if node.valueroot is not None:
            assert self.valueroot is not None,'Child has a value root but the parent does not.'
            self.valueroot.removeChild(node.valueroot)
            node.valueroot = None
            node.valuenode = None
        self.controller.afterVisibilityChange(node,False,False)
        node.destroy()

    def getId(self):
        """Returns the id of the node.
        """
        return self.location[-1]

    def getSecondaryId(self):
        """Returns the secondary id of the node. This is only present for
        nodes that can occur multiple times, and must then be set on creation
        of the node. Returns an empty string if the secondary id has not been set.
        """
        assert self.valueroot is not None, 'The value node has not been set; this node cannot be optional.'
        return self.valueroot.getAttribute('id')

    def getValueType(self,returnclass=False):
        """Returns the value type of the node; an empty string is returned
        if the node cannot have a value.
        """
        valuetype = self.templatenode.getAttribute('type')
        if returnclass:
            if valuetype=='': return None
            valuetype = self.controller.getDataType(valuetype)
        return valuetype
        
    def getUnit(self):
        """Returns the unit of the node; None is returned if the node
        does not have a unit specified.
        """
        if not self.templatenode.hasAttribute('unit'): return None
        unit = self.templatenode.getAttribute('unit')
        if unit[0]=='[' and unit[-1]==']':
            node = self.parent[unit[1:-1]]
            if node is None: return None
            unit = node.getValueAsString(addunit=False,usedefault=True)
        return unit

    def getText(self,detail,minimumdetail = 0,capitalize=False):
        """Returns a (template) text describing the node. Depending
        on the "detail" argument, this returns the node id (detail=0),
        the node label (detail=1), or the node description (detail=2).
        
        If the text within the specified detail is unavailable, text
        with lower detail is looked for down to level "minimumdetail".
        
        If no text is found that meets the criteria, None is returned.
        If "capitalize" is True, the first letter of the returned text
        is capitalized.
        """
        templatenode = self.templatenode
        ret = None
        if self.canHaveClones():
            ret = self.getSecondaryId()
            if ret=='': ret = None
        if ret is None:
            if detail==2 and templatenode.hasAttribute('description'):
                ret = templatenode.getAttribute('description')
            elif detail>=1 and minimumdetail<=1 and templatenode.hasAttribute('label'):
                ret = templatenode.getAttribute('label')
            elif minimumdetail==0:
                ret = self.getId()
        if ret is not None and capitalize: ret = ret[0].upper() + ret[1:]
        return ret
        
    def __getitem__(self,path):
        assert isinstance(path,basestring), 'Supplied node path is not a string: %s.' % path
        return self.getLocation(path.split('/'))

    def getLocation(self,location):
        """Returns the child node at the specified location (a list of
        path components - strings).
        """
        node = self
        for childname in location:
            if childname=='..':
                assert self.parent is not None,'Cannot go up one level because we are at the root.'
                node = node.parent
            elif childname!='' and childname!='.':
                for node in node.children:
                    if node.location[-1]==childname: break
                else:
                    return None
        return node

    def getLocationMultiple(self,location):
        """Returns all child nodes at the specified location (a list of
        path components - strings).
        """
        # Get the first non-empty path term.
        path = location[:]
        target = ''
        while target=='' and len(path)>0: target = path.pop(0)
        if target=='': return [self]

        res = []
        for child in self.children:
            if child.location[-1]==target:
                if len(path)==0:
                    res.append(child)
                else:
                    res += child.getLocationMultiple(path)
        return res

    def isHidden(self):
        """Returns True is the node is currently hidden. Nodes can be hidden
        because the template conditions on their visibility are not met,
        or because they simply have the "hidden" attribute set in the template.
        """
        node = self
        while node is not None:
            if not node.visible: return True
            node = node.parent
        return False

    def isReadOnly(self):
        """Returns True if the template specifies the read-only attribute
        for the node.
        
        Note that settings the read-only attribute does not prevent any
        modification of the node value through the API; it is merely a
        sign the UI editors not to allow editing of the node.
        """
        return self.templatenode.hasAttribute('readonly')

    def hasChildren(self):
        """Returns True if the node has children.
        """
        return len(self.children)>0

    def canHaveValue(self):
        """Returns True if the node can have a value, False if not
        (e.g. when the node is a container only).
        """
        return self.templatenode.hasAttribute('type')

    def canHaveChildren(self):
        if len(self.children)>0: return True
        for templatechild in self.templatenode.childNodes:
            if templatechild.nodeType==templatechild.ELEMENT_NODE and templatechild.localName=='element': return True
        return False

    def canHaveClones(self):
        """Returns True if the node can occurs more than once.
        """
        return self.templatenode.hasAttribute('maxoccurs')

    def getDescendants(self):
        """Returns all descendant nodes.
        """
        res = [self]
        for ch in self.children:
            res += ch.getDescendants()
        return res

    def getNodesByType(self,valuetype,allowderived=False):
        """Returns all descendant nodes with the specified data type.
        """
        res = []
        owntype = self.getValueType(returnclass=True)
        if isinstance(valuetype,basestring): valuetype = self.controller.getDataType(valuetype)
        if (allowderived and owntype is not None and issubclass(owntype,valuetype)) or owntype==valuetype:
            res.append(self)
        for ch in self.children:
            res += ch.getNodesByType(valuetype,allowderived)
        return res

    def getEmptyNodes(self,usedefault=False):
        """Returns all descendant nodes that do not have a value assigned
        to them, but are capable of having a value.
        """
        res = []
        if self.canHaveValue():
            value = self.getValue(usedefault=usedefault)
            if value is None: res.append(self)
            if isinstance(value,util.referencedobject): value.release()
        for ch in self.children:
            res += ch.getEmptyNodes()
        return res

    def updateVisibility(self,recursive=False,notify=True):
        """Updates the dynamic visibility of the node by re-evaluating
        the conditions imposed by the template on the node's visibility.
        """
        templatenode = self.templatenode
        cond = util.findDescendantNode(templatenode,['condition'])
        if cond is not None:
            shownew = self.controller.checkCondition(cond,self)
            if shownew!=self.visible:
                if notify: self.controller.beforeVisibilityChange(self,shownew)
                self.visible = shownew
                if notify: self.controller.afterVisibilityChange(self,shownew)
        if recursive:
            for child in self.children: child.updateVisibility(recursive=True,notify=notify)

    def copyFrom(self,sourcenode,replace=True):
        """Recursively copies the value of the current node from the
        specified source node.
        
        If "replace" is not set, values are copied only if the current
        value has not been set, and existing optional nodes are not
        removed first.
        """
        # Copy node value (if both source and target can have a value)
        if self.canHaveValue() and sourcenode.canHaveValue():
            if replace or not self.hasValue():
                curval = sourcenode.getValue()
                self.setValue(curval)
                if isinstance(curval,util.referencedobject): curval.release()

        # If replacing previous contents, remove optional nodes (with minoccurs=0)
        prevchildname = None
        index = 0
        oldchildren = list(self.children)
        for sourcechild in sourcenode.children:
            childname = sourcechild.location[-1]
            if childname!=prevchildname:
                index = 0
                prevchildname = childname
            if sourcechild.canHaveClones():
                secid = sourcechild.getSecondaryId()
                if secid!='':
                    child = self.getChildById(childname,secid,create=True)
                else:
                    child = self.getChildByNumber(childname,index,create=True)
            else:
                child = self[childname]
            if child is None: continue
            child.copyFrom(sourcechild,replace=replace)
            if child in oldchildren:
                oldchildren.remove(child)
            index += 1
        if replace:
            for ch in oldchildren:
                if ch.canHaveClones(): self.removeChildNode(ch)

# ------------------------------------------------------------------------------------------
# TypedStore
# ------------------------------------------------------------------------------------------
            
# TypedStore: encapsulates the above store.
#   Adds the use of a second XML document (template) that describes the data types
#   of the nodes of the first DOM, and that describes dependencies between nodes.
#   Any node in the original document for which conditions are not met is hidden.
#   Nodes that are not described by the template are not allowed in the property store.
#   Node are obtained by traversing the tree (start: TypedStore.root).
class TypedStore(util.referencedobject):

    defaultname2scenarios = None
    
    @staticmethod
    def getDefaultSchemas():
        """Returns a dictionary that links short schema names to the full schema
        paths (XML schema file).
        
        This method may be overridden by deriving classes if they need the
        ability to refer to schemas by shortcuts rather than paths.
        """
        return ShortcutDictionary()

    @staticmethod
    def getDefaultValues():
        """Returns a dictionary that links short names of default value set
        to the paths of the value files (XML file).
        
        This method may be overridden by deriving classes if they need the
        ability to refer to default value sets by shortcuts rather than paths.
        """
        return ShortcutDictionary()

    @classmethod
    def getDefault(cls,name,version):
        """Returns a TypedStore with the set of default value identified by
        the specified name, converted to the specified version if needed.
        
        To use this, the deriving class MUST implement getDefaultValues!
        """
        import atexit
        if cls==TypedStore: return None
        if name is None: name = 'default'
        if cls.defaultname2scenarios is None: cls.defaultname2scenarios = {}
        version2store = cls.defaultname2scenarios.setdefault(name,{})
        if version in version2store:
            # We have the requested default with the requested version in our cache; return it.
            return version2store[version]
        elif 'source' not in version2store:
            # We do not have any version of the requested default; first obtain the source version.
            path = cls.getDefaultValues().get(name,None)
            if path is None: return None
            sourcestore = cls.fromXmlFile(path,adddefault=False)
            atexit.register(TypedStore.release,sourcestore)
            version2store['source'] = sourcestore
            version2store[sourcestore.version] = sourcestore
            if sourcestore.version==version: return sourcestore
        # We now have the source version of the requested default, but we need another version. Convert.
        sourcestore = version2store['source']
        defstore = cls.fromSchemaName(version,adddefault=False)
        sourcestore.convert(defstore)
        atexit.register(TypedStore.release,defstore)
        version2store[version] = defstore
        return defstore

    @classmethod
    def fromSchemaName(cls,schemaname,*args,**kwargs):
        """Returns a TypedStore based on the schema identified by the specified
        name.
        
        To use this, the deriving class MUST implement getDefaultSchemas!
        """
        assert cls!=TypedStore, 'fromSchemaName cannot be called on base class "TypedStore", only on derived classes. You need to create a derived class with versioning support.'
        schemapath = cls.getDefaultSchemas().get(schemaname,None)
        if schemapath is None: 
            raise Exception('Unable to locate XML schema file for "%s".' % schemaname)
        kwargs['schema'] = schemapath
        store = cls(*args,**kwargs)
        return store

    @classmethod
    def fromXmlFile(cls,path,**kwargs):
        """Returns a TypedStore for the values at the specified path (XML file).
        
        The values file is openend, its schema identified retrieved. Then the
        program attempt to created the required schema. For this to work,
        the deriving class must implement getDefaultSchemas.
        """
        assert cls!=TypedStore, 'fromXmlFile cannot be called on base class "TypedStore", only on derived classes. Use setStore if you do not require versioning.'
        if not os.path.isfile(path):
            raise Exception('Specified path "%s" does not exist, or is not a file.' % path)
        valuedom = xml.dom.minidom.parse(path)
        version = valuedom.documentElement.getAttribute('version')
        if version=='':
            raise Exception('XML file "%s" does not have the version attribute. Therefore we cannot automatically select the XML schema to use.' % path)
        store = cls.fromSchemaName(version,**kwargs)
        store.setStore(valuedom)
        return store

    def __init__(self,schema,valueroot=None,otherstores={},adddefault=True):
        
        util.referencedobject.__init__(self)

        if not isinstance(schema,Schema): schema = Schema.create(schema)
        self.schema = schema

        # Get schema version
        self.version = self.schema.getVersion()
        self.originalversion = None

        # Allow subclasses to provide custom data types
        self.customdatatypes = self.getCustomDataTypes()

        # Events
        self.interfaces = []
        self.blockedinterfaces = set()

        self.otherstores = otherstores
        for v in self.otherstores.itervalues(): v.addref()

        # Link to original source (if any)
        self.path = None

        # Clear store variables
        self.context = {}
        self.defaultstore = None
        self.defaultinterface = None
        self.root = None
        
        # Add store with default values if requested and available.
        if adddefault:
            defscenario = self.getDefault(None,self.version)
            if defscenario is not None: self.setDefaultStore(defscenario,updatevisibility=False)

        # Now set current values in the store
        # NB: this must be done after default values are set, so that the default
        # values can be taken into account when checking conditions (via setStore)
        self.setStore(valueroot)

        # Validation history: list of valid nodes
        self.validnodes = set()
        
    def unlink(self):
        """Destroys the store and breaks circular references. The TypedStore object
        should not be used after this method has been called!
        """
        if self.root is not None: self.root.destroy()
        self.root = None

        # Release container
        self.setContainer(None)
        
        # Release default store
        if self.defaultstore is not None:
            self.defaultstore.disconnectInterface(self.defaultinterface)
            self.defaultinterface = None
            self.defaultstore.release()
            self.defaultstore = None

        # Release any linked objects
        if 'linkedobjects' in self.context:
            for v in self.context['linkedobjects'].itervalues(): v.release()
            del self.context['linkedobjects']
            
        # Release any linked stores
        for v in self.otherstores.itervalues(): v.release()
        
        # Release all interfaces
        for i in self.interfaces: i.unlink()
        self.interfaces = []

    def getDataType(self,name):
        if name in self.customdatatypes: return self.customdatatypes[name]
        assert name in datatypes.types,'Unknown data type "%s" requested.' % name
        return datatypes.types[name]
       
    @classmethod 
    def getCustomDataTypes(cls):
        return {}

    def getInterface(self,**kwargs):
        """Returns an interface to the store. Interfaces offer several facilities
        to e.g. consistently show or hide nodes with the "hidden" property, and to
        omit schema nodes that are meant for grouping only (with the "grouponly"
        attribute). Also, interfaces provide the *only* means of being notified by the
        store about changes of node value, visibility, etc.
        """
        return TypedStoreInterface(self,**kwargs)

    def setContainer(self,container):
        """Sets the container to be used by nodes that point to external data.
        This function also clears the cache with external data objects.
        """
        if 'cache' in self.context:
            for v in self.context['cache'].itervalues(): v.release()
            del self.context['cache']
        if self.context.get('container',None) is not None:
            self.context['container'].release()
        if container is not None: container.addref()
        self.context['container'] = container

    def setStore(self,valueroot):
        """Provides an XML DOM tree with values for the TypedStore. This
        replaces any existing values. The values can be specified as a
        path to an XML file (i.e., a string), an XML document, or an XML
        node. None may be specified instead to clear the store of all values.
        """
        if self.root is not None: self.root.destroy()

        if 'linkedobjects' in self.context:
            for n,v in self.context['linkedobjects'].iteritems():
                assert isinstance(v,util.referencedobject), 'Linked file %s is not of type util.referencedobject.' % n
                v.release()
            del self.context['linkedobjects']

        templateroot = self.schema.getRoot()

        assert valueroot is None or isinstance(valueroot,basestring) or isinstance(valueroot,xml.dom.Node), 'Supplied value root must None, a path to an XML file, or an XML node, but is %s.' % valueroot

        valuedom = None
        if valueroot is None:
            impl = xml.dom.minidom.getDOMImplementation()
            assert templateroot.hasAttribute('name'), 'Root of the schema does not have attribute "name".'
            valuedom = impl.createDocument(None, templateroot.getAttribute('name'), None)
            valueroot = valuedom.documentElement
            valueroot.setAttribute('version',self.version)
        elif isinstance(valueroot,basestring):
            valuedom = xml.dom.minidom.parse(valueroot)
            valueroot = valuedom.documentElement
        elif valueroot.nodeType==valueroot.DOCUMENT_NODE:
            valuedom = valueroot
            valueroot = valuedom.documentElement
        else:
            valuedom = valueroot
            while valuedom.parentNode is not None: valuedom = valuedom.parentNode
            assert valuedom.nodeType==valuedom.DOCUMENT_NODE, 'Could not find DOM document node.'

        valuesversion = valueroot.getAttribute('version')
        assert valuesversion==self.version or valuesversion=='', 'Versions of the xml schema ("%s") and and the xml values ("%s") do not match.' % (self.version,valuesversion)

        if not valueroot.hasAttribute('syntax'):
            syntax = (1,0)
        else:
            syntax = tuple(map(int,valueroot.getAttribute('syntax').split('.')))

        self.xmldocument = valuedom
        self.xmlroot = valueroot

        self.context = {}

        self.root = Node(self,templateroot,self.xmlroot,[],None)
        self.changed = False
        self.setContainer(None)
        
        # Update the visibility of all nodes - based on conditions
        # Disable individual notifications because the single "storechanged" event emitted
        # below replaces them)
        self.root.updateVisibility(recursive=True,notify=False)
        
        # Notify attached interface about the store change.
        self.afterStoreChange()

    def setDefaultStore(self,store,updatevisibility=True):
        """Attached a TypedStore object with default values. The attached
        store MUST use the same schema as the store that is attached to.
        """
        assert self.version==store.version,'Version of supplied default store must match version of current store.'
        if self.defaultstore is not None:
            self.defaultstore.disconnectInterface(self.defaultinterface)
            self.defaultinterface = None
            self.defaultstore.release()
            
        self.defaultstore = store.addref()
        self.defaultinterface = self.defaultstore.getInterface()
        self.defaultinterface.connect('afterChange',self.onDefaultChange)
        
        # Default nodes are used in condition checking, so changing the default store
        # requires updating the visibility of all nodes. Do so, unless explicitly said not to.
        if updatevisibility: self.root.updateVisibility(recursive=True)

    def hasChanged(self):
        """Returns whether any value in the store has changed since the values
        were loaded (through "setStore"), or since "resetChanged" was called.
        """
        if self.changed: return True
        for v in self.context.get('linkedobjects',{}).itervalues():
            if isinstance(v,TypedStore) and v.hasChanged(): return True
        return False

    def resetChanged(self):
        """Resets the "changed" status of the store to "unchanged".
        See also "hasChanged".
        """
        self.changed = False
        for v in self.context.get('linkedobjects',{}).itervalues():
            if isinstance(v,TypedStore): v.resetChanged()

    def __getitem__(self,path):
        """Returns node at the specified path below the root of the tree.
        """
        return self.root[path]

    def mapForeignNode(self,foreignnode):
        """Takes a node from another TypedStore that uses the same XML schema,
        and returns the equivalent node in the current store. Used for finding
        corresponding nodes in the store with defaults, among others.
        """
        indices = []
        currentnode = foreignnode
        
        # First we walk up the tree from the supplied foreign node, in order to find the indices
        # of all involved ancestors.
        for name in reversed(foreignnode.location):
            if not currentnode.canHaveClones():
                # This node must appear once; its index can only be zero.
                indices.insert(0,0)
            else:
                # This node can appear zero or more times. It can be identified
                # by its unique id, or if not available, by its number.
                index = currentnode.getSecondaryId()
                if index=='':
                    siblings = currentnode.parent.getLocationMultiple([name])
                    for (index,sib) in enumerate(siblings):
                        if sib is currentnode: break
                    else:
                        assert False, 'Cannot find foreign node "%s" in list of its own siblings.' % name
                indices.insert(0,index)
            currentnode = currentnode.parent
        assert currentnode.parent is None, 'Location does not describe complete path to root. Currently at %s.' % currentnode
        
        # Now find the same location in our own store.
        currentnode = self.root
        for (name,index) in zip(foreignnode.location,indices):
            if isinstance(index,int):
                currentnode = currentnode.getChildByNumber(name,index)
            else:
                currentnode = currentnode.getChildById(name,index)
            if currentnode is None: return None
            
        return currentnode
        
    def persist(self,callback=None):
        """Directs all custom nodes to store their custom contents in a container."""
        nodes = [node for node in self.root.getNodesByType(datatypes.DataType,True) if node.valuenode is not None]
        progslicer = util.ProgressSlicer(callback,len(nodes))
        for node in nodes:
            progslicer.nextStep(node.getText(1))
            value = node.getValue()
            if isinstance(value,datatypes.DataType):
                value.persist(node.valuenode,self.context)
            if isinstance(value,util.referencedobject): value.release()

    def preparePersist(self):
        """Prepares custom nodes for being stored on disk.
        
        This functionality is used by DataFile objects to read all
        data from the source archive before it is overwritten by
        an in-place save.
        """
        nodes = self.root.getNodesByType(datatypes.DataType,True)
        for node in nodes:
            value = node.getValue()
            if isinstance(value,datatypes.DataType):
                value.preparePersist(node.valuenode,self.context)
            if isinstance(value,util.referencedobject): value.release()

    def checkCondition(self,nodeCondition,ownernode,ownstorename=None):
        """Checks whether the condition specified by the specified XML "conditon" node
        from the schema is met. The specified ownernode is used to resolve references to
        relative paths; it is the first ancestor of the condition that is of type
        element.
        """
        assert nodeCondition.hasAttribute('type'), 'condition lacks "type" attribute in XML schema file.'
        src = nodeCondition.getAttribute('source')
        if src!='' and src!=ownstorename:
            if src not in self.otherstores: return True
            return self.otherstores[src].checkCondition(nodeCondition,ownernode,ownstorename=src)
        condtype = nodeCondition.getAttribute('type')
        if condtype=='eq' or condtype=='ne':
            # Check for required XML attributes
            assert nodeCondition.hasAttribute('variable'), 'condition lacks "variable" attribute in XML schema file.'
            assert nodeCondition.hasAttribute('value'), 'condition lacks "value" attribute in XML schema file.'

            valuepath = nodeCondition.getAttribute('variable')
            refnode = self.root
            if valuepath[0]!='/': refnode = ownernode.parent
            node = refnode[valuepath]
            assert node is not None, 'Cannot locate dependency "%s" for node "%s".' % (nodeCondition.getAttribute('variable'),ownernode)

            # Get the current value of the variable we depend on
            curvalue = node.getValue(usedefault=True)

            # If the node in question currently does not have a value, we cannot check the condition;
            # just return 'valid'.
            if curvalue is None: return True

            # Get the reference value we will compare against
            valuetype = node.getValueType(returnclass=True)
            assert issubclass(valuetype,datatypes.DataTypeSimple), 'Data type of target node of condition must be DataTypeSimple, but is %s.' % (str(valuetype),)
            refvalue = valuetype.fromXmlString(nodeCondition.getAttribute('value'),{},node)

            # Compare
            if condtype=='eq':
                result = (curvalue==refvalue)
            else:
                result = (curvalue!=refvalue)
                
            if isinstance(curvalue,util.referencedobject): curvalue.release()
            
            return result
            
        elif condtype=='and' or condtype=='or':
            # Check every child condition.
            for ch in nodeCondition.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='condition':
                    if self.checkCondition(ch,ownernode):
                        # OR query: one True guarantees success 
                        if condtype=='or': return True
                    else:
                        # AND query: one False guarantees failure 
                        if condtype=='and': return False
                        
            # We evaluated all children. If we are doing an OR, that means all
            # children returned False: we failed, if we are doing an AND, all
            # children returned True: we succeeded.
            if condtype=='and': return True
            return False
        else:
            raise Exception('unknown condition type "%s" in XML schema file.' % condtype)
            
    def fillMissingValues(self,skiphidden=False):
        """For every node that does not have a value, set its value to the default.
        Set "skiphidden" to True to leave the value of nodes that are currently hidden
        untouched.
        """
        assert self.defaultstore is not None, 'Cannot fill missing values with defaults because no default store has been specified.'
        if skiphidden:
            for n in self.root.getEmptyNodes():
                if not n.isHidden():
                    defvalue = n.getDefaultValue()
                    n.setValue(defvalue)
                    if isinstance(defvalue,util.referencedobject): defvalue.release()
        else:
            self.root.copyFrom(self.defaultstore.root,replace=False)

    def clearValidationHistory(self,nodes=None):
        if nodes is None:
            self.validnodes.clear()
        else:
            self.validnodes -= set(nodes)
        
    def updateValidationHistory(self,validity):
        for node,valid in validity.iteritems():
            if valid:
                self.validnodes.add(node)
            else:
                self.validnodes.discard(node)
            
    def validate(self,nodes=None,usedefault=True,repair=0,callback=None,usehistory=True):

        # If no nodes were specified explicitly, we must validate all.
        if nodes is None: nodes = self.root.getDescendants()
            
        # Call base implementation
        errors, validity = self._validate(nodes,usedefault=usedefault,repair=repair,callback=callback,usehistory=usehistory)

        # Update validation history (if required)
        if usehistory: self.updateValidationHistory(validity)
        
        # Returns list of validation errors (strings)
        return errors
        
    def _validate(self,nodes,usedefault=True,repair=0,callback=None,usehistory=True):

        errors = []
        validity = dict([(node,True) for node in nodes])
            
        # Build relevant subsets of node list.
        customnodes,selectnodes,emptynodes,intnodes,floatnodes = [],[],[],[],[]
        for node in nodes:
            if not node.canHaveValue(): continue
            type = node.getValueType()
            value = node.getValue(usedefault=usedefault)
            if value is None:
                emptynodes.append(node)
            elif isinstance(value,datatypes.DataType):
                customnodes.append(node)
            elif node.templatenode.hasAttribute('hasoptions'):
                selectnodes.append(node)
            elif type=='int':
                intnodes.append(node)
            elif type=='float':
                floatnodes.append(node)
            if isinstance(value,util.referencedobject): value.release()
        
        # Find used nodes that have not been set, and lack a default value.
        for node in emptynodes:
            if node.isHidden(): continue
            validity[node] = False
            errors.append('variable "%s" has not been set.' % node.getText(1))

        # Find used file nodes that have not been supplied with data.
        visiblecustomnodes = [node for node in customnodes if not node.isHidden()]
        progslicer = util.ProgressSlicer(callback,len(visiblecustomnodes))
        for node in visiblecustomnodes:
            progslicer.nextStep('validating '+node.getText(detail=1))
            value = node.getValue(usedefault=usedefault)
            if not value.validate(callback=progslicer.getStepCallback()):
                validity[node] = False
                errors.append('variable "%s" is set to an invalid value.' % node.getText(1))
            if isinstance(value,util.referencedobject): value.release()

        # Find nodes of type "select" that have been set to an invalid (non-existing) option.
        for node in selectnodes:
            value = node.getValue(usedefault=usedefault)
            optionsroot = util.findDescendantNode(node.templatenode,['options'])
            assert optionsroot is not None, 'Schema node %s is of type "select", but lacks the "options" child node.' % node
            opt = 0
            for ch in optionsroot.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                    chvalue = node.getValueType(returnclass=True).fromXmlString(ch.getAttribute('value'),{},node.templatenode)
                    if value==chvalue:
                        opt = 1
                        if not ch.hasAttribute('disabled'): opt = 2
                        break
            if opt!=2:
                if repair==2 or (repair==1 and node.isHidden()):
                    node.setValue(node.getDefaultValue())
                elif opt==1:
                    validity[node] = False
                    errors.append('variable "%s" is set to option "%s", which is currently disabled (perhaps not yet implemented).' % (node.getText(1),ch.getAttribute('value')))
                else:
                    validity[node] = False
                    errors.append('variable "%s" is set to non-existent option %s.' % (node.getText(1),str(value)))

        # Find nodes with numeric data types, and check if they respect specified ranges (if any).
        for node in intnodes:
            value = node.getValue(usedefault=usedefault)
            minval,maxval = node.templatenode.getAttribute('minInclusive'),node.templatenode.getAttribute('maxInclusive')
            if minval!='':
                if value<int(minval):
                    if repair==2 or (repair==1 and node.isHidden()):
                        node.setValue(minval)
                    else:
                        validity[node] = False
                        errors.append('variable "%s" is set to %i, which lies below the minimum of %i.' % (node.getText(1),value,int(minval)))
            if maxval!='':
                if value>int(maxval):
                    if repair==2 or (repair==1 and node.isHidden()):
                        node.setValue(maxval)
                    else:
                        validity[node] = False
                        errors.append('variable "%s" is set to %i, which lies above the maximum of %i.' % (node.getText(1),value,int(maxval)))
                        
        for node in floatnodes:
            value = node.getValue(usedefault=usedefault)
            minval,maxval = node.templatenode.getAttribute('minInclusive'),node.templatenode.getAttribute('maxInclusive')
            if minval!='':
                if value<float(minval):
                    if repair==2 or (repair==1 and node.isHidden()):
                        node.setValue(minval)
                    else:
                        validity[node] = False
                        errors.append('variable "%s" is set to %.6g, which lies below the minimum of %.6g.' % (node.getText(1),value,float(minval)))
            if maxval!='':
                if value>float(maxval):
                    if repair==2 or (repair==1 and node.isHidden()):
                        node.setValue(maxval)
                    else:
                        validity[node] = False
                        errors.append('variable "%s" is set to %.6g, which lies above the maximum of %.6g.' % (node.getText(1),value,float(maxval)))
        
        return errors,validity
        
    def convert(self,target,callback=None):
        """Converts the TypedStore object to the specified target. The target may be
        a version string (a new TypedStore object with the desired version will be created)
        or an existing TypedStore object with the different version.
        """
        if isinstance(target,basestring):
            if target==self.version:
                return self.addref()
            target = self.fromSchemaName(target)
        elif target.version==self.version:
            return target

        convertor = self.getConvertor(self.version,target.version)
        if convertor is None:
            raise Exception('No convertor available to convert version "%s" to "%s".' % (self.version,target.version))
        convertor.convert(self,target,callback=callback)

        return target

    convertorsfrom = {}

    @classmethod
    def getConvertor(cls,sourceid,targetid,directonly=False):
        """Returns a convertor object, capable of converting between the specified versions.
        Conversion routes may be direct (using one convertor object), or indirect (using a
        chain of convertor objects). Specify "directonly" to retrieve only direct conversion
        routes. Return None if no convertor is available that meets the specified criteria.
        """
        # Try direct route first.
        if (sourceid in cls.convertorsfrom) and (targetid in cls.convertorsfrom[sourceid]):
            convertorclass = cls.convertorsfrom[sourceid][targetid]
            if convertorclass==Convertor:
                conv = convertorclass(sourceid,targetid)
                revconv = cls.getConvertor(targetid,sourceid,directonly=True)
                if revconv is not None:
                    conv.links = revconv.reverseLinks()
                return conv
            else:
                return convertorclass()

        # Direct route not available, try indirect routes
        if not directonly:
            indirectroutes = cls.findIndirectConversion(sourceid,targetid,depth='  ')
            if len(indirectroutes)>0:
                indirectroutes.sort(key=len)
                route = indirectroutes[0]
                chain = []
                for istep in range(len(route)-1):
                    convertor = cls.getConvertor(route[istep],route[istep+1],directonly=True)
                    chain.append(convertor)
                return ConvertorChain(chain)

        # No route available.
        return None

    @classmethod
    def findIndirectConversion(cls,sourceid,targetid,disallowed=[],depth=''):
        """Returns all conversion routes between the specified source version and target
        version. Use of intermediate versions specified in "disallowed" will be avoided
        (this is used specifically for prevetion of circular conversion routes). The
        depth argument is used for debugging output only."""
        next = cls.convertorsfrom.get(sourceid,{}).keys()
        routes = []
        curdisallowed = disallowed[:]+[sourceid]
        for curnext in next:
            if curnext in curdisallowed: continue
            if curnext==targetid:
                routes.append([sourceid,curnext])
            else:
                childroutes = cls.findIndirectConversion(curnext,targetid,curdisallowed,depth=depth+'  ')
                for cr in childroutes:
                    routes.append([sourceid]+cr)
        return routes

    @classmethod
    def clearConvertors(cls):
        cls.convertorsfrom = {}

    @classmethod
    def addConvertor(cls,convertorclass,addsimplereverse=False):
        """Registers the specified convertor class. The source and target version that
        the convertor supports are part of the convertor class supplied, and are therefore
        not specified explicitly. The 'addsimplereverse" option will additionally register
        a simple class for back conversion, via addDefaultConvertor."""
        sourceid = convertorclass.fixedsourceid
        targetid = convertorclass.fixedtargetid
        assert sourceid is not None, 'Error! Specified convertor class lacks a source identifier.'
        assert targetid is not None, 'Error! Specified convertor class lacks a target identifier.'
        if sourceid not in cls.convertorsfrom: cls.convertorsfrom[sourceid] = {}
        assert targetid not in cls.convertorsfrom[sourceid], 'Error! A class for converting from "%s" to "%s" was already specified previously.' % (sourceid,targetid)
        cls.convertorsfrom[sourceid][targetid] = convertorclass
        if addsimplereverse: cls.addDefaultConvertor(targetid,sourceid)
        
    @classmethod
    def addDefaultConvertor(cls,sourceid,targetid):
        """Registers the "default convertor" for conversion from the specified source, to the
        specified target. The default convertor will attempt a deep copy, copying all values
        at locations that match between source and target."""
        if sourceid not in cls.convertorsfrom: cls.convertorsfrom[sourceid] = {}
        assert targetid not in cls.convertorsfrom[sourceid], 'Error! A class for converting from "%s" to "%s" was already specified previously.' % (sourceid,targetid)
        cls.convertorsfrom[sourceid][targetid] = Convertor

    @classmethod
    def hasConvertor(cls,sourceid,targetid):
        """Checks if a conversion route between the specified versions is available.
        Both direct and indirect (via another version) routes are ok.
        """
        # Try direct conversion
        if cls.getConvertor(sourceid,targetid) is not None:
            return True

        print 'Searching for indirect conversion routes from '+sourceid+' to '+targetid+'.'
        indirectroutes = cls.findIndirectConversion(sourceid,targetid,depth='  ')
        for indirect in indirectroutes:
            print indirect
        print 'Found '+str(len(indirectroutes))+' indirect routes.'

        return len(indirectroutes)>0

    @classmethod
    def rankSources(cls,targetid,sourceids,requireplatform=None):
        """Rank a set of supplied versions/identifiers according to platform (i.e. gotmgui, gotm)
        and version. Rank criterion is 'closeness' (in version and platform) to the reference
        targetid.
        """
        (targetplatform,targetversion) = targetid.split('-')
        targetversion = versionStringToInt(targetversion)

        # Decompose source ids into name and (integer) version, but only take
        # source we can actually convert to the target version.
        sourceinfo = []
        for sid in sourceids:
            if sid==targetid or cls.hasConvertor(sid,targetid):
                (platform,version) = sid.split('-')
                if requireplatform is None or requireplatform==platform:
                    version = versionStringToInt(version)
                    sourceinfo.append((platform,version,sid))

        # Sort by platform (because we want the target platform first)
        sourceinfoclasses = {}
        for sinf in sourceinfo:
            if sinf[0] not in sourceinfoclasses: sourceinfoclasses[sinf[0]] = []
            sourceinfoclasses[sinf[0]].append(sinf)

        # Now sort per platform according to version (higher versions first)
        result = []
        for sourceplatform in sourceinfoclasses.keys():
            infos = sourceinfoclasses[sourceplatform]
            infos.sort(cmp=lambda x,y: cmp(y[1],x[1]))
            if sourceplatform==targetplatform:
                result = infos+result
            else:
                result += infos

        resultids = []
        for res in result: resultids.append(res[2])

        return resultids

    @classmethod
    def canBeOpened(cls, container):
        """Returns whether the specified path can be opened as a TypedStore object."""
        assert isinstance(container,datatypes.DataContainer), 'Argument must be data container object.'
        return cls.storefilename in container.listFiles()

    def save(self,path):
        """Saves the values as XML, to the specified path. A file saved in this manner
        might be loaded again through the "load" method."""
        util.stripWhitespace(self.xmlroot)
        f = codecs.open(path,'w','utf-8')
        self.xmldocument.writexml(f,encoding='utf-8',addindent='\t',newl='\n')            
        f.close()

    def load(self,path):
        """Loads values from an existing XML file. This file may have been saved with the
        "save" method, or it may be taken from a container saved with the "saveAll" method.
        
        If the version of the XML file does not match the version of the store, conversion
        is attempted."""
        if isinstance(path,datatypes.DataFile):
            f = path.getAsReadOnlyFile()
            try:
                valuedom = xml.dom.minidom.parse(f)
            except Exception,e:
                raise Exception('Unable to parse as XML: '+unicode(e))
            f.close()
        else:
            if not os.path.isfile(path):
                raise Exception('Specified path "%s" does not exist, or is not a file.' % path)
            try:
                valuedom = xml.dom.minidom.parse(path)
            except Exception,e:
                raise Exception('"%s" does not contain valid XML: %s' % (path,unicode(e)))
            
        schemarootname = self.schema.getRoot().getAttribute('name')
        if valuedom.documentElement.localName!=schemarootname:
            raise Exception('Name of XML root node (%s) does not match root identifier in schema specification (%s).' % (valuedom.documentElement.localName,schemarootname))

        version = valuedom.documentElement.getAttribute('version')
        if self.version!=version and version!='':
            # The version of the saved store does not match the version of this store object; convert it.
            if verbose: print 'Value file "%s" has version "%s"; starting conversion to "%s".' % (path,version,self.version)
            tempstore = self.fromSchemaName(version)
            tempstore.setStore(valuedom)
            tempstore.convert(self)
            tempstore.release()
            self.originalversion = version
        else:
            self.setStore(valuedom)

    def saveAll(self,path,targetversion=None,targetisdir = False,claim=True,fillmissing=False,callback=None):
        """Saves the values plus any associated data in a ZIP archive or directory.
        A file or direcoty created in this manner may be loaded again through the
        "loadAll" method.
        
        The "claim" argument decides whether the TypedStore object will, after the save,
        refer to the newly saved container for external data objects. If this is not
        set, the TypedStore will after the save still use its original container for
        external data objects."""
        if targetversion is not None and self.version!=targetversion:
            progslicer = util.ProgressSlicer(callback,3)
        
            # First convert to the target version
            progslicer.nextStep('converting to version %s' % targetversion)
            tempstore = self.convert(targetversion,callback=progslicer.getStepCallback())

            # Now save the result of the conversion.
            progslicer.nextStep('saving')
            tempstore.saveAll(path, targetversion = targetversion,targetisdir = targetisdir,callback=progslicer.getStepCallback())

            if claim:
                # Convert back: by doing this the original store will be able to reference nodes
                # (of type "file") in the newly saved file.
                progslicer.nextStep('loading saved version')
                tempstore.convert(self,callback=progslicer.getStepCallback())

            # Release the conversion result.
            tempstore.release()
        else:
            progslicer = util.ProgressSlicer(callback,2)

            # First: fill nodes that are not set with the default value.
            if fillmissing: self.fillMissingValues()

            # Before opening the target container, allow nodes to prepare for saving to the specified path.
            # Specifically, nodes will read all files that might be overwritten into memory.
            if isinstance(path,basestring):
                self.context['targetcontainerpath'] = path
                self.preparePersist()
                del self.context['targetcontainerpath']

            # Open target container
            if isinstance(path,basestring):
                if targetisdir:
                    container = datatypes.DataContainerDirectory(path,create=True)
                else:
                    container = datatypes.DataContainerZip(path,mode='w')
            elif isinstance(path,StringIO.StringIO):
                container = datatypes.DataContainerZip(path,mode='w')
                claim = False
            else:
                assert False,'Supplied target must be a path to file or directory, or a StringIO object.'

            # Allow all nodes to add custom data to the target container. This can change the values
            # in the XML store, and must therefore be done before the store is added to the container.
            self.context['targetcontainer'] = container
            self.context['donotclaimtarget'] = (not claim)
            progslicer.nextStep('adding data streams')
            self.persist(progslicer.getStepCallback())
            del self.context['donotclaimtarget']
            
            # Add any other objects that were linked to the store by a node
            # of custom type (e.g. DataFileEx)
            for name,linkedfile in self.context.get('linkedobjects',{}).iteritems():
                assert isinstance(linkedfile,TypedStore), 'Do not know how to add linked file %s of type %s to container.' % (name,str(type(linkedfile)))
                df = datatypes.DataFileXmlNode(linkedfile.xmldocument)
                df_added = container.addItem(df,name)
                df_added.release()
                df.release()

            # Add XML store to the container
            progslicer.nextStep('saving configuration')
            df = datatypes.DataFileXmlNode(self.xmldocument)
            df_added = container.addItem(df,self.storefilename)
            df_added.release()
            df.release()

            # Make the container save all changes and then release it.
            # Note if claim=True: even though we release it, many nodes (of type "file") may now hold
            # references to data in the saved container; the container will likely not be completely
            # released. On the other hand, the original sources that were locked before saving now
            # probably will be released (the nodes do not lock them any longer).
            container.persistChanges()
            container.release()

        if isinstance(path,basestring):
            self.path = path
        else:
            self.path = None
        
        self.resetChanged()

    def loadAll(self,path,callback=None):
        """Loads values plus associated data from the specified path. The path should point
        to a valid data container, i.e., a ZIP file, TAR/GZ file, or a directory. The source
        container typically has been saved through the "saveAll" method.
        """
        if isinstance(path,basestring):
            container = datatypes.DataContainer.fromPath(path)
        elif isinstance(path,datatypes.DataContainer):
            container = path.addref()
        elif isinstance(path,datatypes.DataFile):
            container = datatypes.DataContainerZip(path)
        else:
            assert False,'Supplied source must be a path, a data container object or a data file object.'

        # Get list of files in source container.
        files = container.listFiles()

        # Check for existence of main file.
        # Note: the name of the main file (self.storefilename) and its description
        # (self.storetitle) must be set by inheriting classes.
        if self.storefilename not in files:
            raise Exception('The specified source does not contain "%s" and can therefore not be a %s.' % (self.storefilename,self.storetitle))

        # Report that we are beginning to load.            
        if callback is not None: callback(0.,'parsing XML')

        # Read and parse the store XML file.
        datafile = container.getItem(self.storefilename)
        f = datafile.getAsReadOnlyFile()
        storedom = xml.dom.minidom.parse(f)
        f.close()
        datafile.release()
        
        # Get the schema version that the store values file matches.
        version = storedom.documentElement.getAttribute('version')
        if self.version!=version and version!='':
            # The version of the saved scenario does not match the version of this scenario object; convert it.
            if verbose: print '%s "%s" has version "%s"; starting conversion to "%s".' % (self.storetitle,path,version,self.version)
            if callback is not None: callback(0.5,'converting scenario')
            tempstore = self.fromSchemaName(version)
            tempstore.loadAll(container)
            if callback is None:
                tempstore.convert(self)
            else:
                tempstore.convert(self,callback=lambda p,s: callback(.5+.5*p,'converting scenario: '+s))
            tempstore.release()
            self.originalversion = version
        else:
            # Attach the parsed scenario (XML DOM).
            self.setStore(storedom)
            self.setContainer(container)
            
        # Store source path.
        self.path = container.path

        # Release reference to container.
        container.release()

        # Report that we have finished loading.            
        if callback is not None: callback(1.,'done')

    def toXml(self,enc='utf-8'):
        """Returns the values as an XML string, with specified encoding."""
        return self.xmldocument.toxml(enc)

    def toXmlDom(self,target=None):
        """Obtains a copy of the values as XML DOM tree. Values are appended to a newly
        created XML document, or to the specified target node, if present."""
        return util.copyNode(self.xmlroot,target)

    # ----------------------------------------------------------------------------------------
    # Event handling
    # ----------------------------------------------------------------------------------------

    def connectInterface(self,interface):
        """Connects an interface to the store. Interfaces provide events and
        can hide nodes with the hidden attribute from view, amongst others."""
        self.interfaces.append(interface)
        
    def disconnectInterface(self,interface):
        """Disconnects an interface from the store. This is required to allow
        the interface to go completely out of scope, and be cleaned-up."""
        for i in range(len(self.interfaces)-1,-1,-1):
            if self.interfaces[i] is interface:
                self.interfaces.pop(i).unlink()

    def onDefaultChange(self,defaultnode,feature):
        """Called internally after a property of a node in the store with default
        values has changed. Note that its argument will be a node in the DEFAULT store,
        not in the current store! The string "feature" specifies which property has
        changed.
        """
        # Map node in default store to node in our own store.
        ownnode = self.mapForeignNode(defaultnode)
        if ownnode is None: return

        # Emit change event
        for i in self.interfaces:
            if i not in self.blockedinterfaces: i.onDefaultChange(ownnode,feature)

        # If the default is being used: update (visibility of) nodes that depend on the changed node.
        if not ownnode.hasValue(): self.updateDependantNodes(ownnode)

    def onChange(self,node,feature):
        """Called internally after a property (e.g., value, unit) of a node has changed.
        The string "feature" specifies which property has changed, e.g., "value", "unit".
        """
        # Register that we changed.
        self.changed = True

        # Emit change event
        for i in self.interfaces:
            if i not in self.blockedinterfaces: i.onChange(node,feature)

        # Update (visibility of) nodes that depend on the changed node.
        self.updateDependantNodes(node)

    def updateDependantNodes(self,node):
        """Called internally after the value of the specified node has changed.
        This method then looks up all nodes that depend on the value of the specified
        node, and emits events if their visibility/unit/... changes in turn.
        """
        # Get nodes that depend on the changed node; if there are none, exit.
        deps = util.findDescendantNode(node.templatenode,['dependentvariables'])
        if deps is None: return

        # Now build a list of the dependant nodes; currently hidden nodes first, currently visible
        # nodes last, so that when we iterate over the list and switch visibilities first extra nodes
        # will appear, and only later some are removed (this prevents unnecessary automatic scrolling in GUI)
        depnodes = []
        for d in util.findDescendantNodes(deps,['dependentvariable']):
            varpath = d.getAttribute('path')
            
            if varpath[0]!='/':
                refnode = node.parent
            else:
                refnode = self.root
            varnode = refnode[varpath]
            assert varnode is not None, 'Unable to locate node "%s" at %s.' % (varpath,refnode)
            
            deptype = d.getAttribute('type')
            if deptype=='visibility':
                if varnode.visible:
                    depnodes.append(varnode)
                else:
                    depnodes.insert(0,varnode)
            else:
                self.onChange(varnode,deptype)
        for varnode in depnodes: varnode.updateVisibility()

    def onBeforeChange(self,node,newvalue):
        """Called internally just before the value of a node changes. The return value
        decides if the change is allowed (return True) or denied (return False)."""
        for i in self.interfaces:
            if i in self.blockedinterfaces: continue
            if not i.onBeforeChange(node,newvalue): return False
        return True

    def afterStoreChange(self):
        """Called internally after the store changes, i.e., all values have changed."""
        self.blockedinterfaces = set(self.interfaces)
        for i in self.interfaces:
            i.afterStoreChange()
            self.blockedinterfaces.remove(i)

    def beforeVisibilityChange(self,node,visible,showhide=True):
        """Called internally before a node is hidden (showhide=True) or deleted (showhide=False)."""
        for i in self.interfaces:
            if i not in self.blockedinterfaces: i.beforeVisibilityChange(node,visible,showhide)

    def afterVisibilityChange(self,node,visible,showhide=True):
        """Called internally after a node is hidden (showhide=True) or deleted (showhide=False)."""
        for i in self.interfaces:
            if i not in self.blockedinterfaces: i.afterVisibilityChange(node,visible,showhide)

def versionStringToInt(versionstring):
    """Converts a version string to a representative integer.
    Versions may consist of components separated by a dot, e.g., "major.minor.build"
    In that case, individual components cannot exceed 255.
    """
    comps = versionstring.split('.')
    version,base = 0,1
    for c in comps[::-1]:
        version += int(c)*base
        base *= 256
    return version
    
class Convertor:
    """Base class for conversion between TypedStore objects that differ
    in version; derive custom convertors from this class.
    
    For the most simple conversions, you can just derive a class from
    this generic Convertor, specify a list of links between source and
    target nodes for those nodes that changed name and/or location between
    versions, and a list of (newly introduced) target nodes that must be
    set to their default. The lists of explicit links and defaults should
    be set in the overridden method registerLinks, in lists self.links and
    self.defaults.
    
    For more advanced conversions, you can in addition override the convert
    method (which should then still call the base method) for custom
    functionality.
    """
    fixedsourceid = None
    fixedtargetid = None

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

        self.sourceversion = sourceid.split('-')[-1]
        self.targetversion = targetid.split('-')[-1]

        self.sourceversionint = versionStringToInt(self.sourceversion)
        self.targetversionint = versionStringToInt(self.targetversion)

        self.links = []
        self.defaults = []
        self.registerLinks()

    def registerLinks(self):
        """This method can be overridden by inheriting classes to specify
        a list of links between source- and target nodes, and a list of target
        nodes that must be set to their default value during conversion. Use
        lists self.links and self.defaults for this, respectively.
        """
        pass

    def convert(self,source,target,callback=None):
        """Converts source TypedStore object to target TypedStore object.
        This method performs a simple deep copy of all values, and then
        handles explicitly specified links between source and target nodes
        (which can be set by inheriting classes), and sets a list of target
        nodes to their defaults (this list is also specified by inheriting
        classes)."""
        # Try simple deep copy: nodes with the same name and location in both
        # source and target store will have their value copied.
        target.root.copyFrom(source.root)

        # Handle explicit one-to-one links between source nodes and target nodes.
        for (sourcepath,targetpath) in self.links:
            sourcenode = source[sourcepath]
            if sourcenode is None:
                raise Exception('Cannot locate node "%s" in source.' % sourcepath)
            targetnode = target[targetpath]
            if targetnode is None:
                raise Exception('Cannot locate node "%s" in target.' % targetpath)
            targetnode.copyFrom(sourcenode)

        # Reset target nodes to defaults where that was explicitly specified.
        if len(self.defaults)>0:
            defscen = target.getDefault(None,target.version)
            for path in self.defaults:
                sourcenode = defscen[path]
                if sourcenode is None:
                    raise Exception('Cannot locate node "%s" in default.' % path)
                targetnode = target[path]
                targetnode.copyFrom(sourcenode)

    def reverseLinks(self):
        """Convert mapping from source to target nodes to mapping from target 
        to source nodes. Used as basis for easy back-conversions."""
        return [(targetpath,sourcepath) for (sourcepath,targetpath) in self.links]

class ConvertorChain(Convertor):
    """Generic class for multiple-step conversions.
    Conversion steps are specified at initialization as a list of convertors."""
    def __init__(self,chain):
        Convertor.__init__(self,chain[0].sourceid,chain[-1].targetid)
        self.chain = chain

    def convert(self,source,target,callback=None):
        temptargets = []
        nsteps = len(self.chain)
        if callback is not None:
            stepcallback = lambda p,s: callback((istep+p)/nsteps,s)
        else:
            stepcallback = None
        for istep in range(nsteps-1):
            convertor = self.chain[istep]
            temptargetid = convertor.targetid
            if callback is not None: callback(float(istep)/nsteps,'converting to version "%s".' % temptargetid)
            if verbose: print 'Converting to temporary target "%s".' % temptargetid
            temptarget = source.fromSchemaName(temptargetid)
            temptargets.append(temptarget)
            convertor.convert(source,temptarget,callback=stepcallback)
            source = temptarget
        convertor = self.chain[-1]
        istep = nsteps-1
        if callback is not None: callback(float(istep)/nsteps,'converting to version "%s".' % target.version)
        if verbose: print 'Converting to final target "%s".' % target.version
        convertor.convert(source,target,callback=stepcallback)
        for temptarget in temptargets: temptarget.release()

