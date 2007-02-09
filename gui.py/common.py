#!/usr/bin/python

#$Id: common.py,v 1.16 2007-02-09 11:39:01 jorn Exp $

import datetime,time
import xml.dom.minidom, os, re, sys
import zipfile, tarfile, tempfile, shutil

# Import NetCDF file format support
#import pycdf
from pynetcdf import NetCDFFile

# Import all MatPlotLib libraries
import matplotlib
matplotlib.use('Qt4Agg')
#matplotlib.rcParams['numerix'] = 'numeric'
matplotlib.rcParams['numerix'] = 'numpy'
import matplotlib.numerix,matplotlib.numerix.ma
import matplotlib.dates
import matplotlib.pylab
import matplotlib.backends.backend_agg

import scenarioformats

# Current GOTM/namelist version used by (1) the GUI, (2) saved GUI scenario files.
# Currently (2) differs from (1) because (1) is still in development, while saved files must use a frozen
# scenario version in order to be usable later too.
guiscenarioversion = 'gotmgui-0.5.0'
savedscenarioversion = 'gotm-4.0.0'

# ------------------------------------------------------------------------------------------
# Date-time parsing variables and functions
# ------------------------------------------------------------------------------------------

# datetime_displayformat: date format used to display datetime objects in the GUI.
datetime_displayformat = '%Y-%m-%d %H:%M:%S'
datetime_displayformat = '%x %X'

# parsedatetime: Convert string to Python datetime object, using specified format.
#   Counterpart of datetime.strftime.
def parsedatetime(str,fmt):
    t1tmp = time.strptime(str,fmt) 
    return datetime.datetime(*t1tmp[0:6])

# ------------------------------------------------------------------------------------------
# Command line argument utility functions
# ------------------------------------------------------------------------------------------

# getNamedArgument: Get the value of a named command line argument, and removes both name
#   and value from the global list of command line arguments. Returns None if the command
#   line argument was not specified. If the script was called with 'script.py -d hello',
#   getNamedArgument('-d') will return 'hello'.
def getNamedArgument(name):
    try:
        iarg = sys.argv.index(name)
    except ValueError:
        return None
    val = sys.argv[iarg+1]
    del sys.argv[iarg+1]
    del sys.argv[iarg]
    return val

# ------------------------------------------------------------------------------------------
# XML DOM utility functions
# ------------------------------------------------------------------------------------------

# findDescendantNode: Return the first child XML DOM node with the specified location
#   (location = array of path components) below the specified XML DOM node (root).
#   If create = True, the node will be created if it does not exist yet.
def findDescendantNode(root,location,create=False):
    assert root!=None,'findDescendantNode called on non-existent parent node (parent = None).'
    node = root
    for childname in location:
        if childname=='': continue
        foundchild = None
        for ch in node.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE and ch.localName==childname:
                foundchild = ch
                break
        if foundchild==None:
            if create:
                doc = root
                while doc.parentNode!=None: doc=doc.parentNode
                foundchild = doc.createElementNS(node.namespaceURI,childname)
                node.appendChild(foundchild)
            else:
                return None
        node = foundchild
    return node

# findDescendantNodes: Return a list of all child XML DOM nodes with the specified location
#   (location = array of path components) below the specified XML DOM node (root).
def findDescendantNodes(root,location):
    parentloc = location[:]
    name = parentloc.pop()
    parent = findDescendantNode(root,parentloc,create=False)
    children = []
    if parent!=None:
        for ch in parent.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE and ch.localName==name:
                children.append(ch)
    return children

##def removeDescendantNodes(root,location):
##    parentloc = location[:]
##    name = parentloc.pop()
##    parent = findDescendantNode(root,parentloc,create=False)
##    if parent==None: return
##    children = []
##    for ch in node.childNodes:
##        if ch.nodeType==ch.ELEMENT_NODE and ch.localName==name:
##            children.append(ch)
##    for ch in children:
##        parent.removeChild(ch)
##        ch.unlink()

def addDescendantNode(root,location):
    parentloc = location[:]
    name = parentloc.pop()
    parent = findDescendantNode(root,parentloc,create=True)
    assert parent!=None,'Unable to locate or create parent node for "%s".' % str(location)
    doc = root
    while doc.parentNode!=None: doc=doc.parentNode
    node = doc.createElementNS(parent.namespaceURI,name)
    parent.appendChild(node)
    return node

# ------------------------------------------------------------------------------------------
# Namelist parsing utilities
# ------------------------------------------------------------------------------------------

class NamelistParseException(Exception):
    def __init__(self,error,filename=None,namelistname=None,variablename=None):
        Exception.__init__(self,error)
        self.filename     = filename
        self.namelistname = namelistname
        self.variablename = variablename

    def __str__(self):
        return Exception.__str__(self)+'.\nFile: '+str(self.filename)+', namelist: '+str(self.namelistname)+', variable: '+str(self.variablename)

class NamelistSubstitutions:
    # Commonly used regular expression (for parsing substitions in the .values file)
    subs_re = re.compile('\s*s/(\w+)/(.+)/')

    def __init__(self,path):
        self.subs = []
        try:
            valuesfile = open(path,'rU')
        except Exception,e:
            raise NamelistParseException('Cannot open .values file "%s". Error: %s' % (path,str(e)))
        line = valuesfile.readline()
        while line!='':
            m = self.subs_re.match(line)
            if m!=None:
                #pat = re.compile(m.group(1),re.IGNORECASE)
                #self.subs.append((pat,m.group(2)))
                self.subs.append((m.group(1),m.group(2)))
            line = valuesfile.readline()
        valuesfile.close()

    def substitute(self,text):
        for (old,new) in self.subs:
            #text = old.sub(new,text)
            text = text.replace(old,new)
        return text

class Namelist:

    varassign_re = re.compile('\s*(\w+)\s*=\s*')
    varstopchar_re = re.compile('[/,\n"\']')

    def __init__(self,name,data,filepath=None):
        self.name = name
        self.data = data
        self.filepath = filepath

    def getNextVariable(self):
        match = self.varassign_re.match(self.data);
        if match==None:
            raise NamelistParseException('Cannot find a variable assignment (variable_name = ...). Current namelist data: "%s"' % (self.data,),self.filepath,self.name,None)
        foundvarname = match.group(1)
        
        self.data = self.data[match.end():]
        ipos = 0
        while True:
            match = self.varstopchar_re.search(self.data,pos=ipos)
            if match==None:
                raise NamelistParseException('End of variable value not found. Current namelist data: "%s"' % (self.data,),self.filepath,self.name,foundvarname)
            ch = match.group(0)
            if ch=='\'' or ch=='"':
                ipos = match.end(0)
                inextquote = self.data.find(ch,ipos)
                if inextquote==-1:
                    raise NamelistParseException('Opening quote %s was not matched by end quote.' % (ch,),self.filepath,self.name,foundvarname)
                ipos = inextquote+1
            else:
                # Found end of variable value.
                vardata = self.data[0:match.start()].rstrip()
                self.data = self.data[match.end():].lstrip()
                break

        return (foundvarname,vardata)

    def isEmpty(self):
        return len(self.data)==0

class NamelistFile:
    
    # Commonly used regular expressions, for:
    #   - locating the start of comments, or opening quotes.
    #   - locating the start of namelist (ampersand followed by list name).
    #   - locating the end of a namelist, i.e. a slash, or opening quotes.
    commentchar_re = re.compile('[!#"\']')
    namelistname_re = re.compile('\s*&\s*(\w+)\s*')
    stopchar_re = re.compile('[/"\']')

    def __init__(self,path,subs=[]):
        # Attempt to open namelist file and read all data
        try:
            nmlfile = open(path,'rU')
        except Exception,e:
            raise NamelistParseException('Cannot open namelist file. Error: %s' % (str(e),),path)

        try:
            
            self.data = ''
            line = nmlfile.readline()
            iline = 1
            while line!='':

                # Strip comments, i.e. on every line, remove everything after (and including) the first exclamation
                # mark; ignore text between single and double quotes.
                ipos = 0
                match = self.commentchar_re.search(line,pos=ipos)
                while match!=None:
                    ch = match.group(0)
                    if ch=='\'' or ch=='"':
                        ipos = match.end(0)
                        inextquote = line.find(ch,ipos)
                        if inextquote==-1:
                            raise NamelistParseException('Line %i: opening quote %s was not matched by end quote.' % (iline,ch),path)
                        ipos = inextquote+1
                    else:
                        # Found start of comment; only keep everything preceding the start position.
                        line = line[:match.start(0)]
                        break
                    match = self.commentchar_re.search(line,pos=ipos)

                self.data += line
                line = nmlfile.readline()
                iline += 1
        finally:
            nmlfile.close()

        # Make substitutions (if any).
        for sub in subs:
            self.data = sub.substitute(self.data)

        self.path = path

    def parseNextNamelist(self,expectedlist=None):
        match = self.namelistname_re.match(self.data)
        if match==None:
            raise NamelistParseException('No namelist found; expected ampersand followed by namelist name.',self.path)
        name = match.group(1)
        if expectedlist!=None and name!=expectedlist:
            raise NamelistParseException('Expected namelist "%s", but found "%s".' % (expectedlist,name),self.path,expectedlist)
        istart = match.end(0)
        ipos = istart
        while True:
            match = self.stopchar_re.search(self.data,pos=ipos)
            if match==None:
                raise NamelistParseException('End of namelist (slash) not found.',self.path,name)
            ch = match.group(0)
            ipos = match.end(0)
            if ch=='/':
                break
            else:
                inextquote = self.data.find(ch,ipos)
                if inextquote==-1:
                    raise NamelistParseException('Opening quote %s was not matched by end quote.' % (ch,),self.path,name)
                ipos = inextquote+1
        namelistdata = self.data[istart:ipos-1]
        self.data = self.data[ipos:]
        return Namelist(name,namelistdata,filepath=self.path)

# ------------------------------------------------------------------------------------------
# XMLPropertyStore
# ------------------------------------------------------------------------------------------

# dateformat: date format used for storing datetime objects in XML.
#   Used in conversion of (XML) string to datetime, and vice versa.
dateformat = '%Y-%m-%d %H:%M:%S'

# XMLPropertyStore: class for storing 'properties' (i.e name,value pairs) in
#   hierarchical structure, using in-memory XML DOM. All values are stored as
#   strings, since XML is text-based; strings are converted to and from other
#   types (date, int, float, bool) whenever necessary.
class XMLPropertyStore:
    
    # =========================================================================================
    # PROTECTED
    # =========================================================================================
    # __init__: constructor
    def __init__(self,xmldocument=None,xmlroot=None):
        if isinstance(xmldocument,str):
            assert xmlroot==None,'Path to XML file specified, but also a (already parsed!) root node was supplied. This combination is invalid'
            xmldocument = xml.dom.minidom.parse(xmldocument)

        self.xmldocument = xmldocument
        if xmlroot==None: xmlroot = xmldocument.documentElement
        self.xmlroot = xmlroot
        self.xmlnamespace = self.xmldocument.namespaceURI

        # Links data type names (strings) to their respective Python classes.
        #   This is particularly relevant for the later TypedXMLPropertyStore, which
        #   uses these data type names in its template XML files. Note that this is extensible:
        #   one can simple add other data types to the self.filetypes dictionary; just make sure
        #   their classes have an associated __unicode__ function.
        self.filetypes = {'string'  :unicode,
                          'int'     :int,
                          'float'   :float,
                          'bool'    :bool,
                          'datetime':datetime.datetime}

    # =========================================================================================
    # PROTECTED
    # =========================================================================================
    # getText: gets all text directly below an XML element; may consist of multiple text nodes.
    def getText(self,node):
        rc = ''
        for ch in node.childNodes:
            if ch.nodeType == ch.TEXT_NODE: rc = rc + ch.data
        return rc

    # =========================================================================================
    # PROTECTED
    # =========================================================================================
    # getText: sets text directly below an XML element, using one text node
    #   replaces any existing child text nodes.
    def setText(self,node,text):
        for ch in node.childNodes:
            if ch.nodeType == ch.TEXT_NODE:
                node.removeChild(ch)
                ch.unlink()
        val = self.xmldocument.createTextNode(text)
        node.insertBefore(val,node.firstChild)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # setProperty: sets specified location (list of ancestor names) to specified value.
    #   autoconverts specified value to string format.
    def setProperty(self,location,value):
        node = findDescendantNode(self.xmlroot,location,create=True)
        assert node!=None, 'Unable to create new child node at "%s".' % str(location)
        return self.setNodeProperty(node,value)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # addProperty: adds a node at specified location (list of ancestor names) with specified
    #   value. Autoconverts specified value to string format.
    def addProperty(self,location,value):
        parentloc = location[:]
        name = parentloc.pop()
        parent = findDescendantNode(self.xmlroot,parentloc,create=True)
        assert parent!=None, 'Unable to locate or create parent node for "%s".' % str(location)
        node = self.xmldocument.createElementNS(parent.namespaceURI,name)
        parent.appendChild(node)
        self.setNodeProperty(node,value)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # setNodeProperty: sets specified node to specified value.
    #   autoconverts specified value to string format.
    def setNodeProperty(self,node,value):
        value = self.packvalue(value)
        if self.getText(node)!=value:
            self.setText(node,value)
            return True
        else:
            return False

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # getProperty: gets value at specified location (list of ancestor names).
    #   autoconverts value to the type requested (otherwise value = string).
    def getProperty(self,location,valuetype=str):
        node = findDescendantNode(self.xmlroot,location)
        if node==None: return None
        return self.getNodeProperty(node,valuetype=valuetype)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # getNodeProperty: gets value at node.
    #   autoconverts value to the type requested (otherwise value = string).
    def getNodeProperty(self,node,valuetype=str):
        return self.unpackvalue(self.getText(node),valuetype=valuetype)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # clearProperty: removes all nodes with specified location (list of ancestor names).
    def clearProperty(self,location):
        parentloc = location[:]
        name = parentloc.pop()
        parent = findDescendantNode(self.xmlroot,parentloc,create=False)
        if parent==None: return
        children = []
        for ch in parent.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE and ch.localName==name:
                children.append(ch)
        for ch in children:
            parent.removeChild(ch)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # clearNodeProperty: removes specified node.
    def clearNodeProperty(self,node):
        node.parentNode.removeChild(node)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # save: saves the current property tree to an XML document.
    def save(self,path):
        self.xmldocument.writexml(file(path,'w'),encoding='utf-8')

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # packvalue: converts a value to a string representation suitable for storing in XML.
    def packvalue(self,value):
        if isinstance(value,datetime.datetime):
            return value.strftime(dateformat)
        elif isinstance(value,bool):
            if value: return 'True'
            else:     return 'False'
        else:
            return unicode(value)

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # unpackvalue: converts string representation of a value to the desired type.
    def unpackvalue(self,value,valuetype=str):
        if isinstance(valuetype,str) or isinstance(valuetype,unicode):
            if valuetype not in self.filetypes:
                raise Exception('unpackvalue: unknown type "%s" requested.' % valuetype)
            valuetype = self.filetypes[valuetype]
        if valuetype==datetime.datetime:
            return parsedatetime(value,dateformat)
        elif valuetype==bool:
            return (value=='True')
        else:
            return valuetype(value)

class DataFile:
    def __init__(self,path=None):
        if path!=None:
            self.path = unicode(path)
        else:
            self.path = None

    def __str__(self):
        if self.path==None: return ''
        return str(self.path)

    def __unicode__(self):
        if self.path==None: return ''
        return unicode(self.path)

    def getName(self):
        if self.path==None: return ''
        (path,name) = os.path.split(self.path)
        return name

    def isValid(self):
        return (self.path!=None and os.path.isfile(self.path))

    def getAsReadOnlyFile(self):
        if not self.isValid():
            Exception('Cannot get the contents of file because the source file "%s" does not exist.' % self.path)
        f = open(self.path,'rU')
        return f

    def save(self,targetpath,claim=True):
        if self.path==targetpath: return
        (sourcepath,sourcename) = os.path.split(self.path)
        print 'Copying "%s".' % sourcename
        shutil.copyfile(self.path,targetpath)
        if claim: self.path=targetpath

    def addToZip(self,zfile,filename):
        if not self.isValid():
            raise Exception('Cannot add "%s" to zip archive because the source file "%s" does not exist.' % (filename,self.path))
        zfile.write(self.path,filename)

# ------------------------------------------------------------------------------------------
# TypedXMLPropertyStore
# ------------------------------------------------------------------------------------------
            
# TypedXMLPropertyStore: encapsulates the above XMLPropertyStore.
#   Adds the use of a second XML document (template) that describes the data types
#   of the nodes of the first DOM, and that describes dependencies between nodes.
#   Any node in the original document for which conditions are not met is hidden.
#   Nodes that are not described by the template are not allowed in the property store.
#   Node are obtained by traversing the tree (start: TypedXMLPropertyStore.root).
class TypedXMLPropertyStore:

    class Node:
        def __init__(self,controller,templatenode,valuenode,location,parent):
            assert templatenode.hasAttribute('id'),'Schema node %s lacks "id" attribute.' % location

            self.controller = controller
            self.store = controller.store
            self.templatenode = templatenode
            self.valuenode = valuenode
            self.location = location
            self.parent = parent
            self.children = []
            self.futureindex = None
            self.visible = (not self.templatenode.hasAttribute('hidden'))

            for templatechild in self.templatenode.childNodes:
                if templatechild.nodeType==templatechild.ELEMENT_NODE and templatechild.localName=='element':
                    childloc = self.location[:] + [templatechild.getAttribute('id')]
                    if templatechild.hasAttribute('maxoccurs'):
                        maxoccurs = int(templatechild.getAttribute('maxoccurs'))
                        valuechildren = findDescendantNodes(self.store.xmlroot,childloc)
                        assert len(valuechildren)<=maxoccurs, 'Number of children is greater than the imposed maximum (%i).' % maxoccurs
                        for valuechild in valuechildren:
                            self.children.append(TypedXMLPropertyStore.Node(self.controller,templatechild,valuechild,childloc,parent=self))
                    else:
                        valuechild = findDescendantNode(self.store.xmlroot,childloc)                            
                        self.children.append(TypedXMLPropertyStore.Node(self.controller,templatechild,valuechild,childloc,parent=self))

        def __str__(self):
            return str(self.location)

        def destroy(self):
            for ch in self.children:
                if ch!=None: ch.destroy()
            self.location = []
            self.children = []
            self.parent = None
            self.templatenode = None
            self.valuenode = None
            self.store = None

        def setValue(self,value):
            if value==None:
                self.clearValue()
                return

            # If the changed node is of type "file", create a local copy of it.
            valuetype = self.templatenode.getAttribute('type')
            if (not self.controller.suppressautofilecopy) and valuetype=='file':
                datafile = value
                filename = self.getId()+'.dat'
                if not self.isHidden() and datafile.isValid():
                    # Only actually copy the file if the node is currently visible.
                    tmpdir = self.controller.getTempDir()
                    targetpath = os.path.join(tmpdir,filename)
                    datafile.save(targetpath)
                value = filename
            
            curval = self.getValue()
            if curval!=value:
                if self.controller.onBeforeChange(self,value):
                    if self.valuenode==None:
                        self.valuenode = findDescendantNode(self.store.xmlroot,self.location,create=True)
                        assert self.valuenode!=None, 'unable to create value node at %s.' % str(self.location)
                    changed = self.store.setNodeProperty(self.valuenode,value)
                    self.controller.onChange(self)
                    return changed
            return False

        def getValue(self):
            if self.valuenode==None: return None
            valuetype = self.templatenode.getAttribute('type')
            assert valuetype!='', 'getValue was used on node without type (%s); canHaveValue should have showed that this node does not have a type.' % self
            value = self.store.getNodeProperty(self.valuenode,valuetype=valuetype)
            if valuetype=='file':
                if self.controller.tempdir!=None:
                    datafile = DataFile(os.path.join(self.controller.tempdir,value))
                else:
                    datafile = DataFile()
                return datafile
            return value

        def getDefaultValue(self):
            defaultstore = self.controller.defaultstore
            if defaultstore==None: return None
            return defaultstore.getProperty(self.location)

        def getValueAsString(self,addunit = True):
            templatenode = self.templatenode
            fieldtype = templatenode.getAttribute('type')
            value = self.getValue()
            if value==None: return ''
            if fieldtype=='datetime':
                value = value.strftime(datetime_displayformat)
            if fieldtype=='bool':
                if value:
                    value = 'Yes'
                else:
                    value = 'No'
            elif fieldtype=='file':
                # Return filename only (not the path)
                value = value.getName()
            elif fieldtype=='select':
                # Get label of currently selected option
                optionsroot = findDescendantNode(templatenode,['options'])
                if optionsroot==None: raise Exception('Variable with "select" type lacks "options" element below.')
                for ch in optionsroot.childNodes:
                    if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                        if value==int(ch.getAttribute('value')):
                            # We found the currently selected option; its label will serve as displayed value.
                            value = ch.getAttribute('label')
                            break
            else:
                value = unicode(value)

            # Append unit specifier (if available)
            if addunit and templatenode.hasAttribute('unit'):
                value = value + ' ' + templatenode.getAttribute('unit')

            return value

        def setToDefault(self,recursive=False):
            if recursive:
                for ch in self.children: ch.setToDefault(True)
            if self.controller.defaultstore!=None:
                return self.setValue(self.getDefaultValue())
            else:
                return self.setValue(None)

        def addChild(self,childname):
            index = -1
            templatenode = None

            # First see of already one instance of this child is in the tree; that makes finding the position easy.
            curindex = 0
            for child in self.children:
                curindex += 1
                if child.location[-1]==childname:
                    index = curindex
                    templatenode = child.templatenode
                elif index!=-1:
                    break

            # The child is not yet in the tree; find the position where to insert the child.
            if index==-1:
                predecessors = []
                for templatechild in self.templatenode.childNodes:
                    if templatechild.nodeType==templatechild.ELEMENT_NODE and templatechild.localName=='element':
                        childid = templatechild.getAttribute('id')
                        if childid==childname:
                            templatenode = templatechild
                            break
                        predecessors.append(childid)
                index = 0
                for child in self.children:
                    curname = child.location[-1]
                    while len(predecessors)>0 and curname!=predecessors[0]:
                        predecessors.pop(0)
                    if len(predecessors)==0:
                        break
                    else:
                        index += 1

            if templatenode==None: return None

            # Create child node
            location = self.location + [childname]
            valuenode = addDescendantNode(self.store.xmlroot,location)
            child = TypedXMLPropertyStore.Node(self.controller,templatenode,valuenode,location,parent=self)
            assert child.canHaveClones(), 'Cannot add another child "%s" because there can exist only one child with this name.' % childname
            child.futureindex = index
            self.controller.beforeVisibilityChange(child,True,False)
            self.children.insert(index,child)
            self.controller.afterVisibilityChange(child,True,False)
            child.futureindex = None
            return child

        def getNumberedChild(self,childname,index):
            children = self.getLocationMultiple([childname])
            if index<len(children): return children[index]
            for ichild in range(index-len(children)+1):
                child = self.addChild(childname)
            return child

        def removeChildren(self,childname,first=0,last=None):
            ipos = 0
            ichildpos = -1
            while ipos<len(self.children):
                child = self.children[ipos]
                if child.location[-1]==childname:
                    assert child.canHaveClones(),'Cannot remove child "%s" because it must occur exactly one time.' % childname
                    ichildpos += 1
                    if last!=None and ichildpos>last: return
                    if ichildpos>=first:
                        self.controller.beforeVisibilityChange(child,False,False)
                        child = self.children.pop(ipos)
                        self.store.clearNodeProperty(child.valuenode)
                        self.controller.afterVisibilityChange(child,False,False)
                        ipos -= 1
                ipos += 1

        def removeAllChildren(self):
            ipos = 0
            while ipos<len(self.children):
                child = self.children[ipos]
                if child.canHaveClones():
                    self.controller.beforeVisibilityChange(child,False,False)
                    child = self.children.pop(ipos)
                    self.store.clearNodeProperty(child.valuenode)
                    self.controller.afterVisibilityChange(child,False,False)
                else:
                    ipos += 1

        def clearValue(self):
            if self.valuenode==None: return
            if self.controller.onBeforeChange(self,None):
                self.store.clearNodeProperty(self.valuenode)
                self.valuenode = None
                self.controller.onChange(self)

        def getId(self):
            return self.location[-1]

        def getValueType(self):
            return self.templatenode.getAttribute('type')

        def getDescription(self,idallowed = False):
            templatenode = self.templatenode
            if templatenode.hasAttribute('description'):
                return templatenode.getAttribute('description')
            elif templatenode.hasAttribute('label'):
                return templatenode.getAttribute('label').capitalize()
            elif idallowed:
                return self.getId().capitalize()
            return None

        def getChildCount(self,showhidden=False):
            if showhidden: return len(self.children)
            childcount = 0
            for child in self.children:
                if not child.isHidden(): childcount += 1
            return childcount

        def getChildren(self,showhidden=False):
            if showhidden: return self.children[:]
            res = []
            for child in self.children:
                if not child.isHidden(): res.append(child)
            return res

        def getDepth(self,showhidden=False):
            childmax = 0
            for child in self.children:
                if showhidden or not child.isHidden():
                    curchilddepth = child.getDepth(showhidden=showhidden)
                    if curchilddepth>childmax: childmax = curchilddepth
            return childmax+1

        def getChildByIndex(self,index,showhidden=False):
            if showhidden: return self.children[index]
            curindex = 0
            for child in self.children:
                if not child.isHidden():
                    if curindex==index: return child
                    curindex += 1
            assert False, 'Node %s: could not find child number %i' % (self,index)

        def getOwnIndex(self,showhidden=False):
            offspring = self.parent.children
            irow = 0
            if self.futureindex!=None:
                if showhidden: return self.futureindex
                else:
                    for isibling in range(self.futureindex):
                        if not offspring[isibling].isHidden(): irow += 1
                    return irow
            else:
                for child in offspring:
                    if child is self: return irow
                    if showhidden or (not child.isHidden()): irow += 1
            assert False, 'Cannot find ourselves in child list of parent.'

        def getLocation(self,location):
            node = self
            for childname in location:
                if childname=='' or childname=='.':
                    continue
                elif childname=='..':
                    assert self.parent!=None,'Cannot go up one level because we are at the root.'
                    node = node.parent
                    continue
                foundchild = None
                for child in node.children:
                    if child.location[-1]==childname:
                        foundchild = child
                        break
                if foundchild == None: return None
                node = foundchild
            return node

        def getLocationMultiple(self,location):
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
            node = self
            while node!=None:
                if not node.visible: return True
                node = node.parent
            return False

        def isReadOnly(self):
            return self.templatenode.hasAttribute('readonly')

        def hasChildren(self):
            return len(self.children)>0
    
        def canHaveValue(self):
            return self.templatenode.hasAttribute('type')

        def canHaveClones(self):
            return self.templatenode.hasAttribute('maxoccurs')

        def getNodesByType(self,valuetype):
            res = []
            if self.getValueType()==valuetype:
                res.append(self)
            for ch in self.children:
                res += ch.getNodesByType(valuetype)
            return res

        def getEmptyNodes(self):
            res = []
            if self.canHaveValue() and self.getValue()==None:
                res.append(self)
            for ch in self.children:
                res += ch.getEmptyNodes()
            return res

        def updateVisibility(self,recursive=False):
            templatenode = self.templatenode
            cond = findDescendantNode(templatenode,['condition'])
            if cond!=None:
                showold = self.visible
                shownew = self.controller.checkCondition(cond,templatenode)
                if showold!=shownew:
                    # Visibility of dependent node has changed. Set the new status,
                    # and emit before and after visibility-changed events
                    self.controller.beforeVisibilityChange(self,shownew)
                    self.visible = shownew
                    self.controller.afterVisibilityChange(self,shownew)
            if self.controller.blockNotifyOfHiddenNodes and (not self.visible):
                return
            if recursive:
                children = self.getChildren(showhidden=True)
                for child in children:
                    child.updateVisibility(recursive=True)

        def copyFrom(self,sourcenode,replace=True):
            # Copy node value (if both source and target can have a value)
            if self.canHaveValue() and sourcenode.canHaveValue():
                if replace or self.getValue()==None:
                    self.setValue(sourcenode.getValue())

            # If replacing previous contents, remove optional nodes (with minoccurs=0)
            if replace: self.removeAllChildren()
            prevchildname = None
            index = 0
            for sourcechild in sourcenode.children:
                childname = sourcechild.location[-1]
                if childname!=prevchildname:
                    index = 0
                    prevchildname = childname
                if sourcechild.canHaveClones():
                    child = self.getNumberedChild(childname,index)
                else:
                    child = self.getLocation([childname])
                if child==None: continue
                child.copyFrom(sourcechild,replace=replace)
                index += 1

        def toHtml(self,xmldocument,totaldepth,level=0,hidedefaults=False):
            res = []

            tr = None
            if level>=0:
                tr = xmldocument.createElement('tr')

                for i in range(level):
                    td = xmldocument.createElement('td')
                    tr.appendChild(td)

                td1 = xmldocument.createElement('td')
                templatenode = self.templatenode
                if templatenode.hasAttribute('label'):
                    lab = templatenode.getAttribute('label')
                else:
                    lab = self.getId()
                td1.appendChild(xmldocument.createTextNode(lab))
                if level+1<totaldepth:
                    td1.setAttribute('colspan',str(totaldepth-level))
                tr.appendChild(td1)

                td2 = xmldocument.createElement('td')
                if self.canHaveValue():
                    val = self.getValueAsString()
                else:
                    val = ' '
                td2.appendChild(xmldocument.createTextNode(val))
                tr.appendChild(td2)

                res.append(tr)

            childtrs = []
            for child in self.children:
                if not child.isHidden():
                    childnodes = child.toHtml(xmldocument,totaldepth,level+1,hidedefaults=hidedefaults)
                    childtrs += childnodes
            res += childtrs

            if tr!=None and hidedefaults:
                isdefault = True
                if self.canHaveValue() and self.getValue()!=self.getDefaultValue():
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
            
    def __init__(self,xmltemplate,xmldocument=None,xmlroot=None):

        # The template can be specified as a DOM object, or as string (i.e. path to XML file)
        if isinstance(xmltemplate,str):
            xmltemplate = xml.dom.minidom.parse(xmltemplate)
        self.templatedom = xmltemplate
        self.version = self.templatedom.documentElement.getAttribute('version')

        # Set event handlers
        self.visibilityhandlers = []
        self.changehandlers = []
        self.beforechangehandlers = []
        self.storechangedhandlers = []
        self.enableevents = True
        self.suppressConditionChecking = False
        self.blockNotifyOfHiddenNodes = True

        # For every variable: build a list of variables/folders that depend on its value.
        self.buildDependencies()

        # Link to original source (if any)
        self.path = None

        # Set property store
        self.store = None
        self.defaultstore = None
        self.root = None
        self.setStore(xmldocument,xmlroot)

        # Variables that deal with storage of data files the store links to.
        self.tempdir = None
        self.tempdirowner = True
        self.suppressautofilecopy = False
        self.datafiles = {}

    def unlink(self):
        if self.tempdir!=None:
            if self.tempdirowner:
                print 'Deleting temporary directory "%s".' % self.tempdir
                shutil.rmtree(self.tempdir)
            self.tempdir = None

        if self.root!=None: self.root.destroy()
        self.root = None
        self.store = None
        self.visibilityhandlers = []
        self.changehandlers = []
        self.beforechangehandlers = []
        self.storechangedhandlers = []

    def getTempDir(self,empty=False):
        if self.tempdir!=None:
            if empty and self.tempdirowner:
                for f in os.listdir(self.tempdir): 
                    os.remove(os.path.join(self.tempdir,f))
        else:
            self.tempdir = tempfile.mkdtemp('','gotm-')
            self.tempdirowner = True
            print 'Created temporary property store directory "%s".' % self.tempdir
        return self.tempdir

    def setStore(self,xmldocument,xmlroot=None):
        if self.root!=None: self.root.destroy()

        templateroot = self.templatedom.documentElement

        if xmldocument==None:
            if xmlroot!=None:
                xmldocument = xmlroot
                while xmldocument.parentNode!=None: xmldocument = xmldocument.parentNode
            else:
                impl = xml.dom.minidom.getDOMImplementation()
                assert templateroot.hasAttribute('id'), 'Root of the schema does not have attribute "id".'
                xmldocument = impl.createDocument('', templateroot.getAttribute('id'), None)
                xmldocument.documentElement.setAttribute('version',self.version)
        elif isinstance(xmldocument,str):
            assert xmlroot==None,'Path to XML file specified, but also a (already parsed!) root node was supplied. This combination is invalid'
            xmldocument = xml.dom.minidom.parse(xmldocument)

        if xmlroot==None: xmlroot = xmldocument.documentElement
        storeversion = xmlroot.getAttribute('version')
        assert storeversion==self.version, 'Versions of the xml template ("%s") and and the xml values ("%s") do not match.' % (self.version,storeversion)
                    
        self.store = XMLPropertyStore(xmldocument,xmlroot=xmlroot)
        self.store.filetypes['select'] = int
        self.store.filetypes['file'] = str
        self.root = TypedXMLPropertyStore.Node(self,templateroot,self.store.xmlroot,[],None)
        if not self.suppressConditionChecking: self.updateVisibility()
        self.changed = False

        self.afterStoreChange()

    def setDefaultStore(self,store):
        assert self.version==store.version
        self.defaultstore = store

    def hasChanged(self):
        return self.changed

    def resetChanged(self):
        self.changed = False

    def setProperty(self,location,value):
        node = self.root.getLocation(location)
        if node==None: raise Exception('Cannot locate node at '+str(location))
        return node.setValue(value)
    
    def getProperty(self,location):
        node = self.root.getLocation(location)
        if node==None: raise Exception('Cannot locate node at '+str(location))
        return node.getValue()

    # suppressVisibilityUpdates: de-activates or re-activates dynamic re-checking of node-conditions
    #   when other nodes change (for performance gains only).
    def suppressVisibilityUpdates(self,sup):
        if self.suppressConditionChecking==sup: return
        if not sup: self.updateVisibility()
        self.suppressConditionChecking = sup

    # buildDependencies: for every variable node, this creates lists of dependent nodes
    # (i.e. folders and variables that have one or more conditions that depend on the
    # variable under investigation). Essentially we convert lists of dependencies ('servant'-centric)
    # into lists of dependent nodes ('controller'-centric). We need the latter in order to selectively
    # re-check conditions (and hide/show corresponding nodes) after the value of
    # a dependency ('controller') changes.
    def buildDependencies(self,root=None,curpath='',curowner=None):
        if root==None: root=self.templatedom.documentElement
        for ch in root.childNodes:
            if ch.nodeType==ch.ELEMENT_NODE:
                if ch.localName=='element':
                    self.buildDependencies(root=ch,curpath=curpath+'/'+ch.getAttribute('id'),curowner=ch)
                elif ch.localName=='condition':
                    if ch.hasAttribute('variable'):
                        deppath = ch.getAttribute('variable')
                        refnode = None
                        if deppath[0]!='/': refnode = curowner.parentNode
                        dep = self.getTemplateNode(deppath.split('/'),root=refnode)
                        assert dep!=None, 'Cannot locate dependency "%s" for node "%s".' % (ch.getAttribute('variable'),curpath)
                        deplist = findDescendantNode(dep,['dependentvariables'],create=True)
                        node = self.templatedom.createElementNS(deplist.namespaceURI,'dependentvariable')
                        node.setAttribute('path',curpath)
                        deplist.appendChild(node)
                    self.buildDependencies(root=ch,curpath=curpath,curowner=curowner)

    # updateVisibility: this checks all conditions on variable and folder nodes, and adds
    # the "hidden" attribute to those nodes if their root condition is not met.
    # This is done only on start-up; after that, conditions are checked selectively after
    # nodes appearing in those conditions change value.
    def updateVisibility(self):
        self.root.updateVisibility(recursive=True)

    # checkCondition: checks whether then given condition (an XML node in the template) is currently met.
    #   "nodeCondition" is the "condition" XML node to check
    #   "ownernode" is the "variable" or "folder" XML node that 'owns' the condition
    #       (= the first ancestor that is not a condition itself)
    def checkCondition(self,nodeCondition,ownernode):
        assert nodeCondition.hasAttribute('type'), 'condition lacks "type" attribute in XML scenario template'
        condtype = nodeCondition.getAttribute('type')
        if condtype=='eq' or condtype=='ne':
            # Check for required XML attributes
            assert nodeCondition.hasAttribute('variable'), 'condition lacks "variable" attribute in XML scenario template'
            assert nodeCondition.hasAttribute('value'), 'condition lacks "value" attribute in XML scenario template'

            valuepath = nodeCondition.getAttribute('variable')
            refnode = self.root
            if valuepath[0]!='/': refnode = self.root.getLocation(self.getTemplateNodePath(ownernode)).parent
            node = refnode.getLocation(valuepath.split('/'))
            assert node!=None, 'Cannot locate dependency "%s" for node "%s".' % (nodeCondition.getAttribute('variable'),self.getTemplateNodePath(ownernode))

            # Get the type and current value of the variable we depend on
            valuetype = node.getValueType()
            curvalue = node.getValue()

            # If the node in question currently does not have a value, we cannot check the condition; just return 'valid'.
            if curvalue==None: return True

            # Get the reference value we will compare against
            refvalue = self.store.unpackvalue(nodeCondition.getAttribute('value'),valuetype)

            # Compare
            if condtype=='eq': return (curvalue==refvalue)
            if condtype=='ne': return (curvalue!=refvalue)
            
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
            raise Exception('unknown condition type "%s" in XML scenario template' % condtype)

    # getTemplateNode: obtains template node at given path
    # (path specification consists of array of node ids)
    def getTemplateNode(self,path,root=None):
        if root==None: root=self.templatedom.documentElement
        for childname in path:
            if childname=='' or childname=='.':
                continue
            elif childname=='..':
                assert not root.isSameNode(self.templatedom.documentElement)
                root = root.parentNode
                continue
            foundnode = None
            for ch in root.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='element' and ch.getAttribute('id')==childname:
                    foundnode = ch
                    break
            assert foundnode!=None, 'Could not find child node "%s" while locating "%s".' % (childname,path)
            root = foundnode
        return root

    # getNodePath: obtains path specification for given template node
    # (path specification consists of node ids with slash separators)
    def getTemplateNodePath(self,node):
        path = []
        while not node.isSameNode(self.templatedom.documentElement):
            path.insert(0,node.getAttribute('id'))
            node = node.parentNode
        return path

    # onBeforeChange: called internally just before the value of a node changes.
    def onBeforeChange(self,node,newvalue):
        if self.enableevents:
            for callback in self.beforechangehandlers:
                if not callback(node,newvalue): return False
        return True

    # onChange: called internally when the value of a node changes.
    #   Here it is used to dynamically re-check the conditions that depend on the changed node.
    def onChange(self,node):
        # Register that we changed.
        self.changed = True

        # Emit change event
        if self.enableevents and not (self.blockNotifyOfHiddenNodes and node.isHidden()):
            for callback in self.changehandlers:
                callback(node)

        # Check if other nodes depend on the changed node, if so, update their visibility.
        if self.suppressConditionChecking: return
        deps = findDescendantNode(node.templatenode,['dependentvariables'])
        if deps==None: return
        for d in findDescendantNodes(deps,['dependentvariable']):
            varpath = d.getAttribute('path').split('/')
            varnode = self.root.getLocation(varpath)
            if self.blockNotifyOfHiddenNodes and varnode.parent!=None and varnode.parent.isHidden():
                continue
            varnode.updateVisibility()

    def addStoreChangedHandler(self,callback):
        self.storechangedhandlers += [callback]

    def afterStoreChange(self):
        if self.enableevents:
            for callback in self.storechangedhandlers:
                callback()

    def addVisibilityChangeHandler(self,beforecallback,aftercallback):
        self.visibilityhandlers += [[beforecallback,aftercallback]]

    def beforeVisibilityChange(self,node,visible,showhide=True):
        if self.enableevents:
            for callback in self.visibilityhandlers:
                if callback[0]!=None: callback[0](node,visible,showhide)

    def afterVisibilityChange(self,node,visible,showhide=True):
        if self.enableevents:
            for callback in self.visibilityhandlers:
                if callback[1]!=None: callback[1](node,visible,showhide)

    def save(self,path):
        return self.store.save(path)

    def saveAll(self,path,xmlname,targetisdir = False):
        if targetisdir:
            # If the directory to write to does not exist, create it.
            if (not os.path.isdir(path)):
                try:
                    os.mkdir(path)
                except Exception,e:
                    raise Exception('Unable to create target directory "%s". Error: %s' % (path,str(e)))
            self.save(os.path.join(path,xmlname))
        else:
            zfile = zipfile.ZipFile(path,'w',zipfile.ZIP_DEFLATED)
            zfile.writestr(xmlname, self.toxml('utf-8'))

        filenodes = self.root.getNodesByType('file')
        for fn in filenodes:
            if not fn.isHidden():
                filename = str(fn.getId()+'.dat')
                datafile = fn.getValue()
                if targetisdir:
                    datafile.save(os.path.join(path,filename),claim=False)
                else:
                    print 'Adding "%s" to archive...' % filename
                    datafile.addToZip(zfile,filename)
        if not targetisdir: zfile.close()
        
        self.resetChanged()

    def toxml(self,enc):
        return self.store.xmldocument.toxml(enc)

    def toxmldom(self):
        return self.store.xmldocument.cloneNode(True)

    def toHtml(self,xmldocument,hidedefaults=True):
        table = xmldocument.createElement('table')
        table.setAttribute('id','tableScenario')

        totaldepth = self.root.getDepth()

        # Create columns.
        for i in range(totaldepth-2):
            col = xmldocument.createElement('col')
            col.setAttribute('width','25')
            table.appendChild(col)
        col = xmldocument.createElement('col')
        table.appendChild(col)

        for tr in self.root.toHtml(xmldocument,totaldepth-1,level=-1,hidedefaults=hidedefaults):
            table.appendChild(tr)

        return table

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # addChangeHandler: registers a callback, to be called when a property changes value.
    def addChangeHandler(self,callback):
        self.changehandlers += [callback]

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # addBeforeChangeHandler: registers a callback, to be called just before a property would
    #    change value. the change is approved if the callback return True, and rejected if it
    #    returns False.
    def addBeforeChangeHandler(self,callback):
        self.beforechangehandlers += [callback]

    # =========================================================================================
    # PUBLIC
    # =========================================================================================
    # enableEvents: enables/disables sending of change-events.
    def enableEvents(self,enabled):
        self.enableevents = enabled

class Scenario(TypedXMLPropertyStore):

    templates = None
    defaultname2path = None
    defaultname2scenarios = {}

    defaultdirname = 'defaultscenarios'
    schemadirname = 'scenarioschemas'
    
    def __init__(self,xmltemplate=None,xmldocument=None,templatename=None,adddefault = True):
        if templatename!=None:
            # If the specified scenario is the id of a template, fill in the path to the template file
            tmpls = Scenario.getTemplates()
            if templatename in tmpls:
                xmltemplate = tmpls[templatename]
            else:
                raise Exception('Unable to locate template XML file for specified scenario version "%s".' % templatename)
        elif xmltemplate==None:
            raise Exception('No scenario template specified. Either specify a file path, or a name of a template (named argument "templatename").')
        elif not os.path.isfile(xmltemplate):
            raise Exception('Scenario template "%s" does not exist.' % xmltemplate)

        TypedXMLPropertyStore.__init__(self,xmltemplate,xmldocument)

        if adddefault:
            defscenario = Scenario.getDefault(None,self.version)
            self.setDefaultStore(defscenario)

        self.namelistextension = self.root.templatenode.getAttribute('namelistextension')

    @staticmethod
    def getTemplates():
        if Scenario.templates==None:
            Scenario.templates = {}
            templatedir = os.path.join(os.path.dirname(__file__),Scenario.schemadirname)
            if os.path.isdir(templatedir):
                for templatename in os.listdir(templatedir):
                    fullpath = os.path.join(templatedir,templatename)
                    if os.path.isfile(fullpath):
                        (root,ext) = os.path.splitext(templatename)
                        if ext=='.xml':
                            Scenario.templates[root] = fullpath
                        else:
                            print 'WARNING: schema directory "" contains non-XML file "%s"; this file will be ignored.' % (Scenario.schemadirname,templatename)
            else:
                print 'WARNING: no templates will be available, because subdirectory "%s" is not present!' % Scenario.schemadirname
        return Scenario.templates

    @staticmethod
    def getDefaultPaths():
        if Scenario.defaultname2path==None:
            Scenario.defaultname2path = {}
            defaultdir = os.path.join(os.path.dirname(__file__),Scenario.defaultdirname)
            if os.path.isdir(defaultdir):
                for filename in os.listdir(defaultdir):
                    fullpath = os.path.join(defaultdir,filename)
                    if os.path.isfile(fullpath):
                        (root,ext) = os.path.splitext(filename)
                        if ext=='.xml':
                            Scenario.defaultname2path[root] = fullpath
                        else:
                            print 'WARNING: default directory "" contains non-XML file "%s"; this file will be ignored.' % (Scenario.defaultdirname,filename)
            else:
                print 'WARNING: no default scenarios will be available, because subdirectory "%s" is not present!' % Scenario.defaultdirname
        return Scenario.defaultname2path

    @staticmethod
    def getDefault(name=None,version=None):
        if name   ==None: name = 'default'
        if version==None: version=guiscenarioversion
        if name not in Scenario.defaultname2scenarios: Scenario.defaultname2scenarios[name] = {}
        version2scenario = Scenario.defaultname2scenarios[name]
        if version in version2scenario:
            # We have the requested default with the requested version in our cache; return it.
            return version2scenario[version]
        elif 'source' not in version2scenario:
            # We do not have any version of the requested default; first obtain the source version.
            path = Scenario.getDefaultPaths()[name]
            sourcescen = Scenario.fromXmlFile(path,adddefault=False)
            version2scenario['source'] = sourcescen
            version2scenario[sourcescen.version] = sourcescen
            if sourcescen.version==version: return sourcescen
        # We now have the source version of the requested default, but we need another version. Convert.
        sourcescen = version2scenario['source']
        scenario = Scenario(templatename=version,adddefault=False)
        sourcescen.convert(scenario)
        version2scenario[version] = scenario
        return scenario

    @staticmethod
    def fromNamelists(path,protodir=None,targetversion=None,strict = True):
        if targetversion==None: targetversion=guiscenarioversion
        
        templates = Scenario.getTemplates()
        sourceids = scenarioformats.rankSources(targetversion,templates.keys(),requireplatform='gotm')
        scenario = None
        failures = ''
        for sourceid in sourceids:
            print 'Trying scenario format "'+sourceid+'"...'
            scenario = Scenario(templatename=sourceid)
            try:
                scenario.loadFromNamelists(path,strict=strict,protodir=protodir)
            except NamelistParseException,e:
                failures += 'Path "'+path+'" does not match template "'+sourceid+'".\nReason: '+str(e)+'\n'
                scenario.unlink()
                scenario = None
            if scenario!=None:
                #print 'Path "'+path+'" matches template "'+template+'".'
                break
        if scenario==None:
            raise Exception('The path "'+path+'" does not contain a supported GOTM scenario. Details:\n'+failures)
        if scenario.version!=targetversion:
            newscenario = scenario.convert(targetversion)
            scenario.unlink()
            return newscenario
        else:
            return scenario

    @staticmethod
    def fromXmlFile(path,adddefault=True):
        if not os.path.isfile(path):
            raise Exception('Specified path "%s" does not exist, or is not a file.' % path)
        xmldocument = xml.dom.minidom.parse(path)
        version = xmldocument.documentElement.getAttribute('version')
        scenario = Scenario(templatename=version,adddefault=adddefault)
        scenario.load(path)
        return scenario

    def convert(self,target,targetownstemp=True):        
        if isinstance(target,str):
            target = Scenario(templatename=target)
        
        convertor = scenarioformats.getConvertor(self.version,target.version)
        convertor.targetownstemp = targetownstemp
        if convertor==None:
            raise Exception('No convertor available to convert version "'+self.version+'" into "'+target.version+'".')
        convertor.convert(self,target)

        return target

    def loadFromNamelists(self, srcpath, strict = False, protodir = None):
        print 'Importing scenario from namelist files...'

        # Start with empty scenario
        self.setStore(None,None)

        nmltempdir = None
        if not os.path.isdir(srcpath):
            # The source path does not point to a directory; it may be a compressed .tar.gz file.
            if os.path.isfile(srcpath):
                # The source path points to a file; try to open it as .tar.gz file.
                try:
                    tf = tarfile.open(srcpath,'r')
                except Exception,e:
                    raise Exception('Path "%s" is not a directory or a tar/gz archive. %s' % (srcpath,str(e)))
                nmltempdir = tempfile.mkdtemp('','gotm-')
                print 'Created temporary namelist directory "'+nmltempdir+'".'
                for tarinfo in tf:
                    tf.extract(tarinfo,nmltempdir)
                tf.close()
                srcpath = nmltempdir
                extracteditems = os.listdir(srcpath)
                if len(extracteditems)==1:
                    # Only one item in the folder with extracted files. If this is a directory, then
                    # that directory will contain the input files.
                    itempath = os.path.join(srcpath,extracteditems[0])
                    if os.path.isdir(itempath):
                        srcpath = itempath
            else:
                raise Exception('Path "%s" does not point to an existing directory or file.' % srcpath)

        globalsubs = []
        if protodir!=None:
            # Namelist are specified as .proto files plus one or more .values files.
            # Load the substitutions specified in the main .values file.
            valuespath = os.path.join(srcpath,os.path.basename(srcpath)+'.values')
            globalsubs.append(NamelistSubstitutions(valuespath))

        # Commonly used regular expressions (for parsing strings and datetimes).
        strre = re.compile('^([\'"])(.*?)\\1$')
        datetimere = re.compile('(\d\d\d\d)[/\-](\d\d)[/\-](\d\d) (\d\d):(\d\d):(\d\d)')

        try:
            for mainchild in self.root.getChildren(showhidden=True):
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
                    nmlfilepath = os.path.join(srcpath, nmlfilename+self.namelistextension)
                else:
                    # Prototype namelist in which values will be substituted.
                    nmlfilepath = os.path.join(protodir, nmlfilename+'.proto')

                    # Load the relevant value substitutions (if any).
                    cursubspath = os.path.join(srcpath,nmlfilename+'.values')
                    if os.path.isfile(cursubspath):
                        cursubs = [NamelistSubstitutions(cursubspath)]

                # Parse the namelist file.
                if not os.path.isfile(nmlfilepath):
                    if mainchild.templatenode.hasAttribute('optional') or mainchild.isHidden():
                        # This namelist file is mssing but not required. Thus no worries: continue
                        continue
                    else:
                        raise NamelistParseException('Namelist file "%s" is not present.' % (os.path.basename(nmlfilepath),),nmlfilepath,None,None)
                nmlfile = NamelistFile(nmlfilepath,cursubs)

                # Loop over all nodes below the root (each node represents a namelist file)
                for filechild in mainchild.getChildren(showhidden=True):
                    # Get name of the expected namelist.
                    listname = filechild.getId()

                    assert not filechild.canHaveValue(), 'Found non-folder node with id %s below branch %s, where only folders are expected.' % (listname,nmlfilename)

                    # Parse the next namelist.
                    namelist = nmlfile.parseNextNamelist(expectedlist=listname)

                    listchildren = filechild.getChildren(showhidden=True)

                    while not namelist.isEmpty():
                        (foundvarname,vardata) = namelist.getNextVariable()

                        if strict:
                            # Strict parsing: all variables must appear once and in predefined order.
                            listchild = listchildren.pop(0)
                            varname = listchild.getId()
                            if varname.lower()!=foundvarname.lower():
                                raise NamelistParseException('Found variable "%s" where "%s" was expected.' % (foundvarname,varname),nmlfilepath,listname,varname)
                        else:
                            # Loose parsing: variables can appear multiple times or not at all, and do not need to appear in order.
                            listchild = None
                            for lc in listchildren:
                                varname = lc.getId()
                                if varname.lower()==foundvarname.lower():
                                    listchild = lc
                                    break
                            if listchild==None:
                                raise NamelistParseException('Encountered variable "%s", which should not be present in this namelist.' % (foundvarname,),nmlfilepath,listname,varname)

                        vartype = listchild.getValueType()

                        if vartype=='string' or vartype=='datetime' or vartype=='file':
                            strmatch = strre.match(vardata)
                            if strmatch==None:
                                raise NamelistParseException('Variable is not a string. Data: %s' % vardata,nmlfilepath,listname,varname)
                            val = strmatch.group(2)
                        elif vartype=='int':
                            try:
                                val = int(vardata)
                            except:
                                raise NamelistParseException('Variable is not an integer. Data: "%s"' % vardata,nmlfilepath,listname,varname)
                        elif vartype=='float':
                            try:
                                val = float(vardata)
                            except:
                                raise NamelistParseException('Variable is not a floating point value. Data: "%s"' % vardata,nmlfilepath,listname,varname)
                        elif vartype=='bool':
                            if   vardata[0].lower()=='f' or vardata[0:2].lower()=='.f':
                                val = False
                            elif vardata[0].lower()=='t' or vardata[0:2].lower()=='.t':
                                val = True
                            else:
                                raise NamelistParseException('Variable is not a boolean. Data: "%s"' % vardata,nmlfilepath,listname,varname)
                        elif vartype=='select':
                            try:
                                val = int(vardata)
                            except:
                                raise NamelistParseException('Variable is not an integer. Data: "%s"' % vardata,nmlfilepath,listname,varname)
                        else:
                            raise Exception('Unknown variable type. I do not know how to parse a variable with type "%s" from namelists.' % vartype)
                        
                        if vartype=='datetime':
                            datetimematch = datetimere.match(val)
                            if datetimematch==None:
                                raise NamelistParseException('Variable is not a date + time. String contents: "'+val+'"',nmlfilepath,listname,varname)
                            refvals = map(lambda(i): int(i),datetimematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                            val = datetime.datetime(*refvals)
                        elif vartype=='file':
                            # Make absolute path
                            filepath = os.path.normpath(os.path.join(os.getcwd(),srcpath, val))
                            val = DataFile(filepath)

                        listchild.setValue(val)
                    if strict and len(listchildren)>0:
                        lcnames = []
                        for lc in listchildren:
                            lcnames.append('"%s"' % lc.getId())
                        raise NamelistParseException('Variables %s are missing' % ', '.join(lcnames),nmlfilepath,listname,None)
        finally:
            if nmltempdir!=None:
                print 'Removing temporary namelist directory "'+nmltempdir+'".'
                shutil.rmtree(nmltempdir)

    def writeAsNamelists(self, targetpath, copydatafiles=True, addcomments = False):
        print 'Exporting scenario to namelist files...'

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
            
            for mainchild in self.root.getChildren(showhidden=True):
                assert not mainchild.canHaveValue(), 'Found a variable below the root node, where only folders are expected.'

                if mainchild.isHidden(): continue

                # Create the namelist file.
                nmlfilename = mainchild.getId()
                nmlfilepath = os.path.join(targetpath, nmlfilename+self.namelistextension)
                nmlfile = open(nmlfilepath,'w')

                try:
                    for filechild in mainchild.getChildren(showhidden=True):
                        assert not filechild.canHaveValue(), 'Found a variable directly below branch "%s", where only folders are expected.' % nmlfilename
                        listname = filechild.getId()

                        if addcomments:
                            nmlfile.write('!'+(linelength-1)*'-'+'\n')
                            title = filechild.getDescription(idallowed=True).encode('ascii','xmlcharrefreplace')
                            nmlfile.write(textwrap.fill(title,linelength-2,initial_indent='! ',subsequent_indent='! '))
                            nmlfile.write('\n!'+(linelength-1)*'-'+'\n')

                            comments = []
                            varnamelength = 0
                            for listchild in filechild.getChildren(showhidden=True):
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
                        for listchild in filechild.getChildren(showhidden=True):
                            if listchild.hasChildren():
                                raise Exception('Found a folder ("%s") below branch %s/%s, where only variables are expected.' % (listchild.getId(),nmlfilename,listname))
                            varname = listchild.getId()
                            varval = listchild.getValue()
                            if varval==None:
                                raise Exception('Value for variable "%s" in namelist "%s" not set.' % (varname,listname))
                            vartype = listchild.getValueType()
                            if vartype=='string':
                                varval = '\''+varval+'\''
                            elif vartype=='file':
                                filename = listchild.getId()+'.dat'
                                if not listchild.isHidden() and copydatafiles:
                                    varval.save(os.path.join(targetpath,filename),claim=False)
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
        description = node.getDescription(idallowed=True)
        lines = [description]
        
        if datatype == 'select':
            # Create list of options.
            options = findDescendantNode(node.templatenode,['options'])
            assert options!=None, 'Node is of type "select" but lacks "options" childnode.'
            for ch in options.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                    lab = ch.getAttribute('description')
                    if lab=='': lab = ch.getAttribute('label')
                    lines.append(ch.getAttribute('value') + ': ' + lab)

        # Create description of data type and range.
        if datatype=='file':
            datatype = 'file path'
        elif datatype=='int' or datatype=='select':
            datatype = 'integer'
        elif datatype=='datetime':
            datatype = 'string, format = "yyyy-mm-dd hh:mm:ss"'
        if node.templatenode.hasAttribute('minimum'):
            datatype += ', minimum = ' + node.templatenode.getAttribute('minimum')
        if node.templatenode.hasAttribute('maximum'):
            datatype += ', maximum = ' + node.templatenode.getAttribute('maximum')
        if node.templatenode.hasAttribute('unit'):
            datatype += ', unit = ' + node.templatenode.getAttribute('unit')

        # Get description of conditions (if any).
        condition = findDescendantNode(node.templatenode,['condition'])
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
            conds = findDescendantNodes(node,['condition'])
            conddescs = map(Scenario.getNamelistConditionDescription,conds)
            return '('+(' '+condtype+' ').join(conddescs)+')'
        else:
            raise Exception('Unknown condition type "%s".' % condtype)

    def saveAll(self,path,targetversion=None,targetisdir = False):
        if targetversion==None:
            # Default save version
            targetversion = savedscenarioversion
            
        if self.version!=targetversion:
            tempscenario = self.convert(targetversion,targetownstemp=False)
            tempscenario.saveAll(path,targetversion=targetversion,targetisdir = targetisdir)
            tempscenario.unlink()
        else:
            TypedXMLPropertyStore.saveAll(self,path,'scenario.xml',targetisdir = targetisdir)
        
        self.resetChanged()

    def load(self,path):
        if not os.path.isfile(path):
            raise Exception('Specified path "%s" does not exist, or is not a file.' % path)
        xmldocument = xml.dom.minidom.parse(path)

        version = xmldocument.documentElement.getAttribute('version')
        if version=='':
            raise Exception('This file lacks the "version" attribute; it cannot be a GOTM scenario.')
        if self.version!=version:
            # The version of the saved scenario does not match the version of this scenario object; convert it.
            print 'Scenario "%s" has version "%s"; starting conversion to "%s".' % (path,version,self.version)
            tempscenario = Scenario(templatename=version)
            tempscenario.setStore(xmldocument)
            tempscenario.convert(self)
            tempscenario.unlink()

            # If the scenario was stored in the official 'save' version, we should not consider it changed.
            # (even though we had to convert it to the 'display' version). Therefore, reset the 'changed' status.
            if version==savedscenarioversion: self.resetChanged()
        else:
            self.setStore(xmldocument)

    def loadAll(self,path):
        # Basic check: does the specified source path exist?
        if not os.path.exists(path):
            raise Exception('Source path "%s" does not exist.' % path)

        # File name for XML.
        xmlname = 'scenario.xml'

        # The specified source file may be a ZIP archive, or a directory that contains the extracted
        # contents of such an archive; decide which.
        srcisdir = os.path.isdir(path)

        if srcisdir:
            # Get list of files in source directory.
            files = os.listdir(path)
        else:
            # Open the archive, and get list of members.
            zfile = zipfile.ZipFile(path,'r')
            files = zfile.namelist()

        # Check for existence of main scenario file.
        if xmlname not in files:
            raise Exception('The specified source does not contain "%s" and can therefore not be a GOTM scenario.' % xmlname)

        # Read and parse the scenario file.
        if not srcisdir:
            xmldata = zfile.read(xmlname)
            storedom = xml.dom.minidom.parseString(xmldata)
        else:
            storedom = xml.dom.minidom.parse(os.path.join(path,xmlname))
        
        # Get the scenario version
        version = storedom.documentElement.getAttribute('version')
        if version=='':
            # Unsupported
            raise Exception('This is an unversioned scenario created with a gotm-gui alpha. These are no longer supported; please recreate your scenario with the current gotm-gui.')
        if self.version!=version:
            # The version of the saved scenario does not match the version of this scenario object; convert it.
            print 'Scenario "%s" has version "%s"; starting conversion to "%s".' % (path,version,self.version)
            if not srcisdir: zfile.close()
            tempscenario = Scenario(templatename=version)
            tempscenario.loadAll(path)
            tempscenario.convert(self)
            tempscenario.unlink()

            # If the scenario was stored in the official 'save' version, we should not consider it changed.
            # (even though we had to convert it to the 'display' version). Therefore, reset the 'changed' status.
            if version==savedscenarioversion: self.resetChanged()
        else:
            # Attach the parsed scenario (XML DOM).
            self.setStore(storedom,None)

            # Get all data files that belong to this scenario from the archive.
            tmpdir = self.getTempDir(empty=True)
            filenodes = self.root.getNodesByType('file')
            for fn in filenodes:
                if not fn.isHidden():
                    filename = fn.getId()+'.dat'
                    if filename not in files:
                        raise Exception('The archive "%s" does not contain required data file "%s".' % (path,filename))
                    targetfilepath = os.path.join(tmpdir,filename)
                    if srcisdir:
                        print 'Copying "filename" to temporary scenario directory...' % filename
                        shutil.copyfile(os.path.join(path,filename),targetfilepath)
                    else:
                        print 'Extracting "%s" to temporary scenario directory...' % filename
                        filedata = zfile.read(filename)
                        f = open(targetfilepath,'wb')
                        f.write(filedata)
                        f.close()

            # Close the archive
            if not srcisdir: zfile.close()

        # Store source path.
        self.path = path

# Abstract class that contains one or more variables that can be plotted.
# Classes deriving from it must support the virtual methods below.
class PlotVariableStore:

    def __init__(self):
        pass

    def getVariableNames(self):
        return []

    def getVariableLongNames(self):
        varnames = self.getVariableNames()
        vardict = {}
        for name in varnames:
            vardict[name] = self.getVariable(name).getLongName()
        return vardict

    def getVariable(self,varname):
        return None

    def getVariableTree(self,path):
        xmlschema = xml.dom.minidom.parse(path)
        vardict = self.getVariableLongNames()
        self.filterNodes(xmlschema.documentElement,vardict)
        other = None
        for ch in xmlschema.getElementsByTagName('element'):
            if ch.getAttribute('id')=='other':
                other = ch
                break
        if other!=None:
            if len(vardict)==0:
                other.parentNode.removeChild(other)
            else:
                ids = vardict.keys()
                ids.sort(cmp=lambda x,y: cmp(vardict[x].lower(), vardict[y].lower()))
                for varid in ids:
                    el = xmlschema.createElement('element')
                    el.setAttribute('id',varid)
                    el.setAttribute('label',vardict[varid])
                    other.appendChild(el)
        store = TypedXMLPropertyStore(xmlschema)
        return store

    def filterNodes(self,node,vardict):
        nodeid = node.getAttribute('id')
        assert nodeid!='', 'Node lacks "id" attribute.'
        keep = (nodeid in vardict)
        if keep:
            node.setAttribute('label',vardict[nodeid])
            node.setAttribute('type','bool')
            del vardict[nodeid]
        children = findDescendantNodes(node,['element'])
        for ch in children:
            if self.filterNodes(ch,vardict) and not keep: keep = True
        if not keep and nodeid!='other':
            node.parentNode.removeChild(node)
        return keep

# Abstract class that represents a variable that can be plotted.
# Classes deriving from it must support the virtual methods below.
class PlotVariable:

    def __init__(self):
        pass

    # getName()
    #   Return type: string
    #   Returns the short name (or identifier) of the variable.
    def getName(self):
        return ''

    # getLongName()
    #   Return type: string
    #   Returns the pretty name for the variable.
    def getLongName(self):
        return ''

    # getUnit()
    #   Return type: string
    #   Returns the unit of the variable.
    def getUnit(self):
        return ''

    # getDimensions()
    #   Return type: tuple of strings
    #   Returns the names of the dimensions of the variable; currently supported dimensions: "time", "z".
    def getDimensions(self):
        return ()

    # getValues(bounds, staggered=False)
    #   Return type: tuple of (numpy/numeric) arrays
    #   Returns the arrays with coordinates (in order of dimnesions returned by getDimensions), and the value array.
    #   Coordinates may be given as 1D arrays (if the coordinates are constant across all other dimensions), or as arrays with
    #   the same numbers of dimensions as the value array (i.e., for every value a coordinate is supplied). If staggered is True,
    #   coordinates must be given at the interfaces and values at the centers; then coordinate arrays must have one extra value in
    #   every dimension compared to the value array.
    def getValues(self,bounds,staggered=False):
        return ()

def findindices(bounds,data):
    # Zero-based indices!
    start = 0
    stop = len(data)-1
    if bounds!=None:
        if bounds[0]!=None:
            while start<len(data) and data[start]<bounds[0]: start+=1
        if bounds[1]!=None:
            while stop>=0         and data[stop] >bounds[1]: stop-=1

        # Greedy: we want take the interval that fully encompasses the specified range.
        # (note that this also corrects for a start beyond the available range, or a stop before it)
        if start>0:          start-=1
        if stop<len(data)-1: stop +=1
        
    return (start,stop)

def interp1(x,y,X):
    assert len(x.shape)==1, 'Original coordinates must be supplied as 1D array.'
    assert len(X.shape)==1, 'New coordinates must be supplied as 1D array.'
    newshape = [X.shape[0]]
    for i in y.shape[1:]: newshape.append(i)
    Y = matplotlib.numerix.zeros(newshape,matplotlib.numerix.typecode(y))
    icurx = 0
    for i in range(X.shape[0]):
        while icurx<x.shape[0] and x[icurx]<X[i]: icurx+=1
        if icurx==0:
            Y[i,:] = y[0,:]
        elif icurx>=x.shape[0]:
            Y[i,:] = y[-1,:]
        else:
            rc = (y[icurx,:]-y[icurx-1,:])/(x[icurx]-x[icurx-1])
            Y[i,:] = y[icurx-1,:] + rc*(X[i]-x[icurx-1])
    return Y

class LinkedFileVariableStore(PlotVariableStore):

    class LinkedFileVariable(PlotVariable):

        def __init__(self,store,data,index):
            self.store = store
            self.data = data
            self.index = index

        def getName(self):
            return self.data[0]

        def getLongName(self):
            return self.data[1]

        def getUnit(self):
            return self.data[2]

        def getDimensions(self):
            filetype = self.store.type
            if filetype=='profilesintime':
                return ('time','z')
            elif filetype=='pointsintime':
                return ('time',)
            else:
                assert False, 'Cannot plot variables from file of unknown type "%s".' % filetype

        def getValues(self,bounds,staggered=False):
            data = self.store.getData()
            timebounds = findindices(bounds[0],data[0])
            if len(data)==2:
                return [data[0][timebounds[0]:timebounds[1]+1],data[1][timebounds[0]:timebounds[1]+1,self.index]]
            elif len(data)==3:
                return [data[0][timebounds[0]:timebounds[1]+1],data[1],data[2][timebounds[0]:timebounds[1]+1,:,self.index]]
            assert False, 'Cannot handle variables with %i dimensions; only know how to deal with 2 or 3 dimensions.' % len(data)

    def __init__(self,node):
        self.vardata = []

        finfo = findDescendantNode(node.templatenode,['fileinfo'])
        self.type = finfo.getAttribute('type')
        
        fvars = findDescendantNode(finfo,['filevariables'])
        if fvars!=None:
            for ch in fvars.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='filevariable':
                    longname = ch.getAttribute('label')
                    unit = ch.getAttribute('unit')
                    name = longname
                    self.vardata.append([name,longname,unit])

        self.datafile = node.getValue()
        self.data = None

    def getVariableNames(self):
        varnames = []
        for data in self.vardata:
            varnames.append(data[0])
        return varnames

    def getVariableLongNames(self):
        vardict = {}
        for data in self.vardata:
            vardict[data[0]] = data[1]
        return vardict

    def getVariable(self,varname):
        index = 0
        for data in self.vardata:
            if data[0]==varname:
                return self.LinkedFileVariable(self,data,index)
            index += 1
        assert False, 'Variable with name "%s" not found in store.' % varname

    def getData(self):
        if self.data!=None: return self.data
        varcount = len(self.vardata)
        
        # Access the data through some read-only file-like object.
        f = self.datafile.getAsReadOnlyFile()

        # Compile regular expression for reading dates.
        datetimere = re.compile('(\d\d\d\d).(\d\d).(\d\d) (\d\d).(\d\d).(\d\d)')

        filetype = self.type
        if filetype=='pointsintime':
            line = f.readline()
            times = []
            values = []
            iline = 1
            while line!='':
                datematch = datetimere.match(line)
                if datematch==None:
                    raise Exception('Line %i does not start with time (yyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
                refvals = map(lambda(i): int(i),datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                curdate = datetime.datetime(*refvals)
                data = line[datematch.end()+1:].split()
                if len(data)<varcount:
                    raise Exception('Line %i contains only %i observations, where %i are expected.' % (iline,len(data),varcount))
                data = map(lambda(i): float(i),data)
                times.append(curdate)
                values.append(data)
                line = f.readline()
                iline += 1
            times = matplotlib.numerix.array(times,matplotlib.numerix.PyObject)
            values = matplotlib.numerix.array(values,matplotlib.numerix.Float32)
            self.data = (times,values)
        elif filetype=='profilesintime':
            line = f.readline()
            times = []
            depths = []
            values = []
            uniquedepths = {}
            iline = 1
            while line!='':
                # Read date & time
                datematch = datetimere.match(line)
                if datematch==None:
                    raise Exception('Line %i does not start with time (yyy-mm-dd hh:mm:ss). Line contents: %s' % (iline,line))
                refvals = map(lambda(i): int(i),datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
                curdate = datetime.datetime(*refvals)

                # Get the number of observations and the depth direction.
                (depthcount,updown) = map(lambda(i): int(i), line[datematch.end()+1:].split())

                # Create arrays that will contains depths and observed values.
                curdepths = matplotlib.numerix.zeros((depthcount,),matplotlib.numerix.Float32)
                curvalues = matplotlib.numerix.zeros((depthcount,varcount),matplotlib.numerix.Float32)
                
                if updown==1:
                    depthindices = range(0,depthcount,1)
                else:
                    depthindices = range(depthcount-1,-1,-1)
                for idepthline in depthindices:
                    line = f.readline()
                    if line=='':
                        raise Exception('Premature end-of-file after line %i; expected %i more observations.' % (iline,depthcount-depthindices.index(idepthline)))
                    iline += 1
                    linedata = map(lambda(i): float(i),line.split())
                    if len(linedata)<varcount+1:
                        raise Exception('Line %i contains only %i value(s), where %i (1 time and %i observations) are expected.' % (iline,len(linedata),varcount+1,varcount))
                    uniquedepths[linedata[0]] = True
                    curdepths[idepthline] = linedata[0]
                    curvalues[idepthline,:] = linedata[1:varcount+1]
                times.append(curdate)
                depths.append(curdepths)
                values.append(curvalues)
                line = f.readline()
                iline += 1
            print 'Data read; starting interpolation...'
            times = matplotlib.numerix.array(times,matplotlib.numerix.PyObject)
            uniquedepths = uniquedepths.keys()
            uniquedepths.sort()
            depthgrid = matplotlib.numerix.array(uniquedepths,matplotlib.numerix.Float32)
            griddedvalues = matplotlib.numerix.zeros((times.shape[0],depthgrid.shape[0],varcount),matplotlib.numerix.Float32)
            for it in range(len(times)):
                griddedvalues[it,:,:] = interp1(depths[it],values[it],depthgrid)
            self.data = (times,depthgrid,griddedvalues)
        else:
            assert False, 'Cannot plot variables from file of unknown type "%s".' % filetype
        f.close()
        return self.data

# Class that represents a GOTM result.
#   Inherits from PlotVariableStore, as it contains variables that can be plotted.
#   Contains a link to the scenario from which the result was created (if available)
class Result(PlotVariableStore):
    reportdirname = 'reporttemplates'
    reportname2path = None

    @staticmethod
    def getReportTemplates():
        if Result.reportname2path==None:
            Result.reportname2path = {}
            sourcedir = os.path.join(os.path.dirname(__file__),Result.reportdirname)
            if os.path.isdir(sourcedir):
                for filename in os.listdir(sourcedir):
                    if filename=='CVS': continue
                    fullpath = os.path.join(sourcedir,filename)
                    if os.path.isdir(fullpath):
                        if os.path.isfile(os.path.join(fullpath,'index.xml')):
                            Result.reportname2path[filename] = fullpath
                        else:
                            print 'WARNING: template directory "%s" does not contain "index.xml"; it will be ignored.' % fullpath
                    else:
                        print 'WARNING: template directory "%s" contains "%s" which is not a directory; the latter will be ignored.' % (sourcedir,filename)
            else:
                print 'WARNING: no report templates will be available, because subdirectory "%s" is not present!' % Result.reportdirname
        return Result.reportname2path

    class ResultVariable(PlotVariable):
        def __init__(self,result,varname):
            PlotVariable.__init__(self)
            self.result = result
            self.varname = str(varname)

        def getName(self):
            return self.varname

        def getLongName(self):
            nc = self.result.getcdf()
            #pycdf return nc.var(self.varname).long_name
            return nc.variables[self.varname].long_name

        def getUnit(self):
            nc = self.result.getcdf()
            #pycdf return nc.var(self.varname).units
            return nc.variables[self.varname].units

        def getDimensions(self):
          nc = self.result.getcdf()
          #pycdf vars = nc.variables()
          #dimnames = vars[self.varname][0]
          #dimcount = len(dimnames)
          #if   dimcount==3:
          #    if dimnames==('time','lat','lon'):
          #        return ('time',)
          #elif dimcount==4:
          #    if (dimnames==('time','z','lat','lon')) or (dimnames==('time','z1','lat','lon')):
          #        return ('time','z')
          #else:
          #  raise Exception('This variable has '+str(dimcount)+' dimensions; I do not know how to handle such variables.')

          dimnames = nc.variables[self.varname].dimensions
          dimcount = len(dimnames)
          if   dimcount==3:
              if dimnames==('time','lat','lon'):
                  return ('time',)
          elif dimcount==4:
              if (dimnames==('time','z','lat','lon')) or (dimnames==('time','z1','lat','lon')):
                  return ('time','z')
          raise Exception('This variable has dimensions %s; I do not know how to handle such variables.' % str(dimnames))

        def getValues(self,bounds,staggered=False):
          nc = self.result.getcdf()
            
          # Get the variable and its dimensions from CDF.

          #pycdf
          #try:
          #    v = nc.var(self.varname)
          #    dims = v.dimensions()
          #except pycdf.CDFError, msg:
          #    print msg
          #    return False
          v = nc.variables[self.varname]
          dims = v.dimensions

          # Get time coordinates and time bounds.
          (t,t_stag) = self.result.getTime()
          timebounds = findindices(bounds[0],t)
          if not staggered:
              t_eff = t[timebounds[0]:timebounds[1]+1]
          else:
              t_eff = t_stag[timebounds[0]:timebounds[1]+2]

          dimcount = len(dims)
          if dimcount==4:
              # Four-dimensional variable: longitude, latitude, depth, time
              (z,z1,z_stag,z1_stag) = self.result.getDepth()
              depthbounds = (0,z.shape[1])
              if dims[1]=='z':
                  if staggered:
                      z_cur = z_stag[timebounds[0]:timebounds[1]+2,depthbounds[0]:depthbounds[1]+2]
                  else:
                      z_cur = z[timebounds[0]:timebounds[1]+1,depthbounds[0]:depthbounds[1]+1]
              elif dims[1]=='z1':
                  if staggered:
                      z_cur = z1_stag[timebounds[0]:timebounds[1]+2,depthbounds[0]:depthbounds[1]+2]
                  else:
                      z_cur = z1[timebounds[0]:timebounds[1]+1,depthbounds[0]+1:depthbounds[1]+2]
              try:
                  dat = v[timebounds[0]:timebounds[1]+1,depthbounds[0]:depthbounds[1]+1,0,0]
              except Exception, e:
                  raise Exception('Unable to read values for NetCDF variable "%s". Error: %s' % (self.varname,str(e)))
              return [t_eff,z_cur,dat]
          elif dimcount==3:
              # Three-dimensional variable: longitude, latitude, time
              try:
                  dat = v[timebounds[0]:timebounds[1]+1,0,0]
              except Exception, e:
                  raise Exception('Unable to read values for NetCDF variable "%s". Error: %s' % (self.varname,str(e)))
              return [t_eff,dat]
          else:
            raise Exception('This variable has dimensions %s; I do not know how to handle such variables.' % str(dimensions))

    def __init__(self):
        self.scenario = None
        self.tempdir = None
        self.datafile = None
        self.nc = None
        self.changed = False

        # Cached coordinates
        self.t = None
        self.t_stag = None
        self.z = None
        self.z1 = None
        self.z_stag = None
        self.z1_stag = None

    def getTempDir(self,empty=False):
        if self.tempdir!=None:
            if empty:
                for f in os.listdir(self.tempdir): 
                    os.remove(os.path.join(self.tempdir,f))
        else:
            self.tempdir = tempfile.mkdtemp('','gotm-')
            print 'Created temporary result directory "'+self.tempdir+'".'
        return self.tempdir

    def save(self,path):
        assert self.datafile!=None, 'The result object was not yet attached to a result file (NetCDF).'

        zfile = zipfile.ZipFile(path,'w',zipfile.ZIP_DEFLATED)

        if self.scenario!=None:
            tempdir = self.getTempDir()
            scenariofile = os.path.join(tempdir,'scenario.gotmscenario')
            self.scenario.saveAll(scenariofile)
            print 'Adding scenario to archive...'
            zfile.write(scenariofile,'scenario.gotmscenario',zipfile.ZIP_STORED)
        
        print 'Adding result to archive...'
        zfile.write(self.datafile,'result.nc')
        
        zfile.close()

        self.changed = False

    def load(self,path):
        # Basic check: does the specified file exist?
        if not os.path.exists(path): raise Exception('File "%s" does not exist.' % path)

        # Open the archive
        zfile = zipfile.ZipFile(path,'r')

        # Get a list of files in the archive, and check whether it contains a scenario and a result.
        files = zfile.namelist()
        if files.count('scenario.gotmscenario')==0:
            raise Exception('The archive "%s" does not contain "scenario.gotmscenario"; it cannot be a GOTM result.' % path)
        if files.count('result.nc')==0:
            raise Exception('The archive "%s" does not contain "result.nc"; it cannot be a GOTM result.' % path)

        # Create a temporary directory to which we can unpack the archive.
        tempdir = self.getTempDir()

        # Unpack the scenario        
        scenariofile = os.path.join(tempdir,'scenario.gotmscenario')
        scenariodata = zfile.read('scenario.gotmscenario')
        f = open(scenariofile,'wb')
        f.write(scenariodata)
        f.close()

        # Unpack the result
        resultfile = os.path.join(tempdir,'result.nc')
        resultdata = zfile.read('result.nc')
        f = open(resultfile,'wb')
        f.write(resultdata)
        f.close()

        # Close the archive
        zfile.close()

        # Load the scenario from file.
        self.scenario = Scenario(templatename=guiscenarioversion)
        self.scenario.loadAll(scenariofile)
        os.remove(scenariofile)

        # Attach the result, try to open the CDF file
        self.datafile = resultfile
        self.getcdf()

        # Reset "changed" status.
        self.changed = False

    def unlink(self):
        if self.nc!=None:
            # Close NetCDF result file.
            self.nc.close()
            self.nc = None
        if self.tempdir!=None:
            # Delete temporary directory.
            print 'Deleting temporary result directory "'+self.tempdir+'".'
            shutil.rmtree(self.tempdir)
            self.tempdir = None

    def attach(self,srcpath,scenario=None,copy=True):
        self.scenario = scenario
        
        if copy:
            # Create a copy of the result file.
            tempdir = self.getTempDir(empty=True)
            datafile = os.path.join(tempdir,'result.nc')
            shutil.copyfile(srcpath,datafile)
        else:
            datafile = srcpath

        # Store link to result file, and try to open the CDF file
        self.datafile = datafile
        self.getcdf()

        self.changed = False

    def getcdf(self):
        if self.nc!=None: return self.nc
        assert self.datafile!=None, 'The result object has not yet been attached to an actual result.'
        try:
          #pycdf self.nc = pycdf.CDF(str(self.datafile))
          self.nc = NetCDFFile(self.datafile)
        #pycdf except pycdf.CDFError, e:
        except Exception, e:
            raise Exception('An error occured while opening the NetCDF file "%s": %s' % (self.datafile,str(e)))
        return self.nc

    def getVariableNames(self,plotableonly=True):
        nc = self.getcdf()

        # Get names of NetCDF variables
        try:
          #pycdf vars = nc.variables()
          vars = nc.variables
          if plotableonly:
              # Only take variables with 3 or 4 dimensions
              varNames = []
              for v in vars.keys():
                  #pycdf dimnames = vars[v][0]
                  dimnames = vars[v].dimensions
                  dimcount = len(dimnames)
                  if   dimcount==3:
                      if dimnames==('time','lat','lon'):
                          varNames += [v]
                  elif dimcount==4:
                      if (dimnames==('time','z','lat','lon')) | (dimnames==('time','z1','lat','lon')):
                          varNames += [v]
          else:
              # Take all variables
              varNames = vars.keys()

        #pycdf except pycdf.CDFError, msg:
        except Exception, e:
            raise Exception('CDFError: '+str(e))

        return varNames

    def getVariableLongNames(self):
      varnames = self.getVariableNames()
      nc = self.getcdf()
      vardict = {}
      for varname in varnames:
          #pycdf varname_str = str(varname)
          #pycdf vardict[varname] = nc.var(varname_str).long_name
          vardict[varname] = nc.variables[varname].long_name
      return vardict

    def getVariable(self,varname,check=True):
        varname = str(varname)
        if check:
            nc = self.getcdf()
            #pycdf vars = nc.variables()
            vars = nc.variables
            if not (varname in vars): return None
        return self.ResultVariable(self,varname)

    def getTimeRange(self):
      nc = self.getcdf()
      #pycdf try:
      #    secs = nc.var('time').get()
      #except pycdf.CDFError, msg:
      #    print msg
      #    return
      secs = nc.variables['time'][:]
      dateref = self.getReferenceDate()
      t1 = dateref + datetime.timedelta(secs[0]/3600/24)
      t2 = dateref + datetime.timedelta(secs[-1]/3600/24)
      return (t1,t2)

    def getDepthRange(self):
      nc = self.getcdf()
      #pycdf try:
      #    z  = nc.var('z').get()
      #    z1 = nc.var('z1').get()
      #except pycdf.CDFError, msg:
      #    print msg
      #    return
      z = nc.variables['z'][:]
      z1 = nc.variables['z1'][:]
      return (min(z[0],z1[0]),max(z[-1],z1[-1]))

    def getTime(self):
        if self.t==None:
            nc = self.getcdf()

            # Get time coordinate (in seconds since reference date)
            #pycdf secs = nc.var('time').get()
            secs = nc.variables['time'][:]
            
            # Convert time-in-seconds to Python datetime objects.
            dateref = self.getReferenceDate()
            t = matplotlib.numerix.zeros((secs.shape[0],),matplotlib.numerix.PyObject)
            for it in range(t.shape[0]):
                t[it] = dateref + datetime.timedelta(secs[it]/3600/24)

            # Create staggered time grid.
            t_stag = matplotlib.numerix.zeros((secs.shape[0]+1,),matplotlib.numerix.PyObject)
            halfdt = datetime.timedelta(seconds=float(secs[1]-secs[0])/2)
            t_stag[0]  = t[0]-halfdt
            t_stag[1:] = t[:]+halfdt
            
            # Cache time grids.
            self.t = t
            self.t_stag = t_stag
            
        return (self.t,self.t_stag)

    def getDepth(self):
        if self.z==None:
            nc = self.getcdf()

            # Get layers heights
            #pycdf h = nc.var('h')[:,:,0,0]
            h = nc.variables['h'][:,:,0,0]
            
            # Get depths of interfaces
            z1 = matplotlib.numerix.cumsum(h[:,:],1)
            z1 = matplotlib.numerix.concatenate((matplotlib.numerix.zeros((z1.shape[0],1),matplotlib.numerix.typecode(z1)),z1),1)
            bottomdepth = z1[0,-1]
            z1 = z1[:,:]-bottomdepth

            # Get depth of layer centers
            z = z1[:,1:z1.shape[1]]-0.5*h

            # Interpolate in time to create staggered grid
            z1_med = matplotlib.numerix.concatenate((matplotlib.numerix.take(z1,(0,),0),z1,matplotlib.numerix.take(z1,(-1,),0)),0)
            z_stag = 0.5 * (z1_med[0:z1_med.shape[0]-1,:] + z1_med[1:z1_med.shape[0],:])
            
            z_med = matplotlib.numerix.concatenate((z,matplotlib.numerix.take(z1,(-1,),1)),1)
            z_med = matplotlib.numerix.concatenate((matplotlib.numerix.take(z_med,(0,),0),z_med,matplotlib.numerix.take(z_med,(-1,),0)),0)
            z1_stag = 0.5 * (z_med[0:z_med.shape[0]-1,:] + z_med[1:z_med.shape[0],:])

            self.z = z
            self.z1 = z1
            self.z_stag = z_stag
            self.z1_stag = z1_stag

        return (self.z,self.z1,self.z_stag,self.z1_stag)

    def getReferenceDate(self):
      # Retrieve reference date/time.
      nc = self.getcdf()
      #pycdf timeunit = nc.var('time').units
      timeunit = nc.variables['time'].units
      datematch = re.compile('(\d\d\d\d)[-\/](\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)').search(timeunit, 1)
      if datematch==None:
          print 'Unable to parse "units" attribute of "time" variable in NetCDF file!'
          return False
      refvals = map(lambda(i): int(i),datematch.group(1,2,3,4,5,6)) # Convert matched strings into integers
      dateref = datetime.datetime(*refvals)
      return dateref

    def generateReport(self,outputpath,templatepath,plotvariables,dpi=150,columncount=2,figuresize=(10,8),callback=None):
        xmldocument = xml.dom.minidom.parse(os.path.join(templatepath,'index.xml'))
        scenario = self.scenario

        steps = float(2+len(plotvariables))
        istep = 0

        # Create output directory if it does not exist yet.
        if not os.path.isdir(outputpath): os.mkdir(outputpath)

        for f in os.listdir(templatepath):
            fullpath = os.path.join(templatepath,f)
            if f.lower()!='index.xml' and os.path.isfile(fullpath): shutil.copy(fullpath,os.path.join(outputpath,f))

        for node in xmldocument.getElementsByTagName('gotm:scenarioproperty'):
            variablepath = node.getAttribute('variable')
            assert variablepath!='', 'gotm:scenarioproperty node in report template lacks "variable" attribute pointing to a location in the scenario.'
            variablenode = scenario.root.getLocation(variablepath.split('/'))
            assert variablenode!=None, 'Unable to locate "%s" in the scenario.' % variablepath
            val = variablenode.getValueAsString()
            node.parentNode.replaceChild(xmldocument.createTextNode(unicode(val)),node)
            node.unlink()

        scenarionodes = xmldocument.getElementsByTagName('gotm:scenario')
        assert len(scenarionodes)<=1, 'Found more than one "gotm:scenario" node in the report template.'
        if len(scenarionodes)>0:
            if callback!=None: callback(istep/steps,'Creating scenario description...')
            scenarionode = scenarionodes[0]
            scentable = scenario.toHtml(xmldocument,hidedefaults=True)
            scenarionode.parentNode.replaceChild(scentable,scenarionode)

        istep += 1

        figuresnodes = xmldocument.getElementsByTagName('gotm:figures')
        assert len(figuresnodes)<=1, 'Found more than one "gotm:figures" node in the report template.'
        if len(figuresnodes)>0:
            figuresnode = figuresnodes[0]
        else:
            figuresnode = None
        if len(plotvariables)>0 and figuresnode!=None:
            mplfigure = matplotlib.figure.Figure(figsize=(figuresize[0]/2.54,figuresize[1]/2.54))
            canvas = matplotlib.backends.backend_agg.FigureCanvasAgg(mplfigure)
            fig = Figure(mplfigure)
            fig.addDataSource('result',self)
            figurestable = xmldocument.createElement('table')
            icurvar = 0
            tr = None
            for pv in plotvariables:
                longname = self.getVariable(pv).getLongName()
                if callback!=None: callback(istep/steps,'Creating figure for %s...' % longname)
                
                if icurvar % columncount == 0:
                    tr = xmldocument.createElement('tr')
                    figurestable.appendChild(tr)
                fig.setUpdating(False)
                fig.clearProperties()
                fig.addVariable(pv)
                fig.setUpdating(True)
                filename = pv+'.png'
                outputfile = os.path.join(outputpath,filename)
                canvas.print_figure(outputfile,dpi=dpi)

                img = xmldocument.createElement('img')
                img.setAttribute('src',filename)
                img.setAttribute('alt',longname)
                img.setAttribute('style','width:%.2fcm' % figuresize[0])
                td = xmldocument.createElement('td')
                td.appendChild(img)
                tr.appendChild(td)

                icurvar = icurvar+1
                istep += 1
            for i in range(columncount - len(tr.childNodes)):
                tr.appendChild(xmldocument.createElement('td'))
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

class MonthFormatter(matplotlib.dates.DateFormatter):
    def __init__(self):
        matplotlib.dates.DateFormatter.__init__(self,'%b')

    def __call__(self, x, pos=None):
        return matplotlib.dates.DateFormatter.__call__(self,x,pos)[0]

class Figure:

    def __init__(self,figure,properties=None):
        self.figure = figure
        self.canvas = figure.canvas

        # Create empty set of properties (these will combine the 'forced' properties, and the automatically
        # chosen defaults for properties that were not explicitly set).
        self.properties = TypedXMLPropertyStore('figuretemplate.xml',None)
        self.properties.addBeforeChangeHandler(self.onBeforeMergedPropertyChange)
        
        # Create store for the explicitly set properties
        self.forcedproperties = TypedXMLPropertyStore('figuretemplate.xml',properties)
        self.forcedproperties.addChangeHandler(self.onExplicitPropertyChanged)

        self.sources = {}
        self.defaultsource = None
        self.updating = True
        self.haschanged = False

        self.ignorechanges = False

    def setUpdating(self,allowupdates):
        if self.updating != allowupdates:
            self.updating = allowupdates
            if allowupdates and self.haschanged: self.update()

    def onBeforeMergedPropertyChange(self,node,value):
        if self.ignorechanges: return True
        
        # The user tried to modify a figure property; redirect this to the store
        # for explicitly set properties.
        self.forcedproperties.setProperty(node.location,value)

        # Do not allow the change of the merged property store (the changed of
        # the explicit-property-store will force a refresh of the merged properties
        # indirectly).
        return False

    def onExplicitPropertyChanged(self,node):
        self.update()

    def clearSources(self):
        self.sources = {}

    def clearVariables(self):
        self.forcedproperties.root.getLocation(['Data']).removeChildren('Series')
        self.update()

    def clearProperties(self):
        self.forcedproperties.setStore(None)
        self.update()

    def setProperties(self,props):
        self.forcedproperties.setStore(props)
        self.update()

    def getPropertiesCopy(self):
        return self.forcedproperties.toxmldom()

    def addDataSource(self,name,obj):
        self.sources[name] = obj
        if self.defaultsource==None: self.defaultsource = name

    def addVariable(self,varname,source=None):
        datanode = self.forcedproperties.root.getLocation(['Data'])
        series = datanode.addChild('Series')
        series.getLocation(['Variable']).setValue(varname)
        if source!=None:
            series.getLocation(['Source']).setValue(source)
        self.update()

    def update(self):
        if not self.updating:
            self.haschanged = True
            return

        self.figure.clear()

        axes = self.figure.add_subplot(111)

        # Get forced axes boundaries (will be None if not set; then we autoscale)
        tmin = self.forcedproperties.getProperty(['TimeAxis','Minimum'])
        tmax = self.forcedproperties.getProperty(['TimeAxis','Maximum'])
        zmin = self.forcedproperties.getProperty(['DepthAxis','Minimum'])
        zmax = self.forcedproperties.getProperty(['DepthAxis','Maximum'])

        # Variables below will store the effective dimension boundaries
        tmin_eff = None
        tmax_eff = None
        zmin_eff = None
        zmax_eff = None

        # Link between dimension name (e.g., "time", "z") and axis (e.g., "x", "y")
        dim2axis = {}

        # We will now adjust the plot properties; disable use of property-change notifications,
        # as those would otherwise call plot.update again, leading to infinite recursion.
        self.ignorechanges = True

        # Shortcuts to the nodes specifying the variables to plot.
        forceddatanode = self.forcedproperties.root.getLocation(['Data'])
        forcedseries = forceddatanode.getLocationMultiple(['Series'])

        # Shortcut to the node that will hold the variables effectively plotted.
        datanode = self.properties.root.getLocation(['Data'])

        # This variable will hold all long names of the plotted variables; will be used to create plot title.
        longnames = []

        iseries = 0
        for forcedseriesnode in forcedseries:
            # Get the name and data source of the variable to plot.
            varname   = forcedseriesnode.getLocation(['Variable']).getValue()
            varsource = forcedseriesnode.getLocation(['Source']).getValue()
            if varsource==None:
                # No data source specified; take default.
                assert self.defaultsource!=None, 'No data source set for variable "%s", but no default source available either.' % varname
                varsource = self.defaultsource
                
            # Get variable object.
            var = self.sources[varsource].getVariable(varname)
            assert var!=None, 'Source "%s" does not contain variable with name "%s".' % (varsource,varname)

            # Copy series information
            newseriesnode = datanode.getNumberedChild('Series',iseries)
            newseriesnode.getLocation(['Variable']).setValue(varname)
            if varsource!=None:
                newseriesnode.getLocation(['Source']).setValue(varsource)

            # Store the variable long name (to be used for building title)
            longnames.append(var.getLongName())

            # Get the (number of) independent dimensions of the current variable.
            dims = var.getDimensions()
            newseriesnode.getLocation(['DimensionCount']).setValue(len(dims))

            # Get the plot type, based on the number of dimensions
            if len(dims)==1:
                plottypenodename = 'PlotType2D'
            elif len(dims)==2:
                plottypenodename = 'PlotType3D'
            else:
                raise Exception('This variable has %i independent dimensions. Can only plot variables with 2 or 3 independent dimensions.' % len(dims))
            plottype = forcedseriesnode.getLocation([plottypenodename]).getValue()
            if plottype==None: plottype=0
            newseriesnode.getLocation([plottypenodename]).setValue(plottype)

            staggered = False
            if plottypenodename=='PlotType3D' and plottype==0: staggered = True

            # Set forced bounds for the different dimensions
            dimbounds = []
            for dimname in dims:
                if dimname=='time':
                    dimbounds.append((tmin,tmax))
                elif dimname=='z':
                    dimbounds.append((zmin,zmax))
                else:
                    raise Exception('Variable has unknown dimension "'+dimname+'".')

            # Get the data
            data = var.getValues(dimbounds,staggered=staggered)

            # Transform to log-scale if needed
            logscale = forcedseriesnode.getLocation(['LogScale']).getValue()
            if logscale==None: logscale = False
            newseriesnode.getLocation(['LogScale']).setValue(logscale)
            if logscale:
                data[-1] = matplotlib.numerix.ma.masked_array(data[-1],data[-1]<=0)
                data[-1] = matplotlib.numerix.ma.log10(data[-1])

            # get label
            label = forcedseriesnode.getLocation(['Label']).getValue()
            if label==None:
                label = var.getLongName()+' ('+var.getUnit()+')'
                if logscale: label = 'log10 '+label
            newseriesnode.getLocation(['Label']).setValue(label)

            for idim in range(len(dims)):
                if len(data[idim].shape)==1:
                    datamin = data[idim][0]
                    datamax = data[idim][-1]
                else:
                    if idim==0:
                        datamin = min(data[idim][0,:])
                        datamax = max(data[idim][-1,:])
                    else:
                        datamin = min(data[idim][:,0])
                        datamax = max(data[idim][:,-1])

                #print dims[idim]+' '+str(data[idim])
                if dims[idim]=='time':
                    # Update effective time bounds
                    if tmin_eff==None or datamin<tmin_eff: tmin_eff=datamin
                    if tmax_eff==None or datamax>tmax_eff: tmax_eff=datamax
                    
                    # Convert time (datetime objects) to time unit used by MatPlotLib
                    data[idim] = matplotlib.dates.date2num(data[idim])
                elif dims[idim]=='z':
                    # Update effective depth bounds
                    if zmin_eff==None or datamin<zmin_eff: zmin_eff=datamin
                    if zmax_eff==None or datamax>zmax_eff: zmax_eff=datamax

            # Plot the data series
            if len(dims)==1:
                # One-dimensional variable; currently this implies dependent on time only.
                xdim = 0

                linewidth = forcedseriesnode.getLocation(['LineWidth']).getValue()
                if linewidth==None: linewidth = .5
                newseriesnode.getLocation(['LineWidth']).setValue(linewidth)

                lines = axes.plot(data[xdim],data[-1],'-',linewidth=linewidth)
                dim2axis[dims[xdim]] = 'x'
                axes.set_ylabel(label)
            elif len(dims)==2:
                # Two-dimensional variable, i.e. dependent on time and depth.
                xdim = 0
                ydim = 1

                dim2axis[dims[xdim]] = 'x'
                dim2axis[dims[ydim]] = 'y'

                X = data[xdim]
                Y = data[ydim]
                Z = data[-1]

                # Get length of coordinate dimensions.
                if len(X.shape)==1:
                    xlength = X.shape[0]
                else:
                    xlength = X.shape[xdim]
                if len(Y.shape)==1:
                    ylength = Y.shape[0]
                else:
                    ylength = Y.shape[ydim]
                
                # Adjust X dimension.
                if len(X.shape)==1:
                    X = matplotlib.numerix.reshape(X,(1,-1))
                    X = matplotlib.numerix.repeat(X, ylength, 0)
                elif xdim<ydim:
                    X = matplotlib.numerix.transpose(X)
                    
                # Adjust Y dimension.
                if len(Y.shape)==1:
                    Y = matplotlib.numerix.reshape(Y,(-1,1))
                    Y = matplotlib.numerix.repeat(Y, xlength, 1)
                elif xdim<ydim:
                    Y = matplotlib.numerix.transpose(Y)
                    
                # Adjust Z dimension.
                if xdim<ydim:
                    # Note: using masked array transpose because values can be masked (e.g. after log-transform)
                    Z = matplotlib.numerix.ma.transpose(Z)

                if plottype==1:
                  pc = axes.contourf(X,Y,Z)
                else:
                  #pc = axes.pcolor(X,Y,Z,shading='flat', cmap=matplotlib.pylab.cm.jet)
                  pc = axes.pcolormesh(X,Y,Z,shading='flat', cmap=matplotlib.pylab.cm.jet)
                  #im.set_interpolation('bilinear')
                cb = self.figure.colorbar(mappable=pc)
                if label!='': cb.set_label(label)

            # Hold all plot properties so we can plot additional data series.
            axes.hold(True)

            iseries += 1

        # Remove unused series (remaining from previous plots that had more data series)
        datanode.removeChildren('Series',first=iseries)

        #axes.autoscale_view()

        # Create and store title
        title = self.forcedproperties.getProperty(['Title'])
        if title==None: title = ', '.join(longnames)
        if title!='': axes.set_title(title)
        self.properties.setProperty(['Title'],title)

        # Store current axes bounds
        if tmin==None: tmin = tmin_eff
        if tmax==None: tmax = tmax_eff
        if zmin==None: zmin = zmin_eff
        if zmax==None: zmax = zmax_eff
        self.properties.setProperty(['TimeAxis', 'Minimum'],tmin)
        self.properties.setProperty(['TimeAxis', 'Maximum'],tmax)
        self.properties.setProperty(['DepthAxis','Minimum'],zmin)
        self.properties.setProperty(['DepthAxis','Maximum'],zmax)

        # Configure time axis (x-axis), if any.
        if 'time' in dim2axis:
            timeaxis = dim2axis['time']
            
            # Obtain label for time axis.
            tlabel = self.forcedproperties.getProperty(['TimeAxis','Label'])
            if tlabel==None: tlabel = 'time'
            self.properties.setProperty(['TimeAxis', 'Label'],tlabel)

            # Configure limits and label of time axis.
            if timeaxis=='x':
                taxis = axes.xaxis
                if tlabel!='': axes.set_xlabel(tlabel)
                axes.set_xlim(matplotlib.dates.date2num(tmin),matplotlib.dates.date2num(tmax))
            elif timeaxis=='y':
                taxis = axes.yaxis
                if tlabel!='': axes.set_ylabel(tlabel)
                axes.set_ylim(matplotlib.dates.date2num(tmin),matplotlib.dates.date2num(tmax))

            # Select tick type and spacing based on the time span to show.
            dayspan = (tmax-tmin).days
            if dayspan/365>10:
              # more than 10 years
              taxis.set_major_locator(matplotlib.dates.YearLocator(base=5))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%Y'))
            elif dayspan/365>1:
              # less than 10 but more than 1 year
              taxis.set_major_locator(matplotlib.dates.YearLocator(base=1))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%Y'))
            elif dayspan>61:
              # less than 1 year but more than 2 months
              taxis.set_major_locator(matplotlib.dates.MonthLocator(interval=1))
              taxis.set_major_formatter(MonthFormatter())
            elif dayspan>7:
              # less than 2 months but more than 1 day
              taxis.set_major_locator(matplotlib.dates.DayLocator(interval=15))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%d %b'))
            elif dayspan>1:
              # less than 1 week but more than 1 day
              taxis.set_major_locator(matplotlib.dates.DayLocator(interval=1))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%d %b'))
            else:
              # less than 1 day
              taxis.set_major_locator(matplotlib.dates.HourLocator(interval=1))
              taxis.set_major_formatter(matplotlib.dates.DateFormatter('%H:%M'))

        # Configure depth axis (y-axis), if any.
        if 'z' in dim2axis:
            zaxis = dim2axis['z']

            # Obtain label for depth axis.
            zlabel = self.forcedproperties.getProperty(['DepthAxis','Label'])
            if zlabel==None: zlabel = 'depth (m)'
            self.properties.setProperty(['DepthAxis', 'Label'],zlabel)

            # Configure limits and label of depth axis.
            if zaxis=='x':
                axes.set_xlim(zmin,zmax)
                if zlabel!='': axes.set_xlabel(zlabel)
            elif zaxis=='y':
                axes.set_ylim(zmin,zmax)
                if zlabel!='': axes.set_ylabel(zlabel)
        self.properties.setProperty(['HasDepthAxis'],'z' in dim2axis)

        # Draw the plot to screen.            
        self.canvas.draw()

        # Re-enable property-change notifications; we are done changing plot properties,
        # and want to be notified if anyone else changes them.
        self.ignorechanges = False
        
        self.haschanged = False
