# This module contains two classes for storing data in an XML tree:
#
# - Store: this class stores variables at locations in the XML tree specified by strings,
#   (e.g. ["root","foo","foo"] for /root/foo/foo) and can natively handle strings, integers,
#   floats, booleans and datetime variables. Conversion between types and from/to the XML
#   storage format is supported if a value type is specified during get/set calls.
#
# - TypedStore: this class combines a XML schema file (XSD-like) with an XML tree in
#   which values are stored. The latter is in fact an encapsulated "Store" described above.
#   Value types therefore do not need to specified in each call; they are obtained from
#   the schema definition. Additionally, the TypedStore supports (conditional) hiding of
#   nodes, notifications before/after changing of node values and node visiblity,
#   a set of default values, arbitrary data streams that are stored aside the XML value
#   tree, encapsulating containers such as ZIP, and many other features.

import datetime
import xml.dom.minidom, os
import zipfile, tarfile, tempfile, shutil, StringIO

import common

# dateformat: date format used for storing datetime objects in XML.
#   Used in conversion of (XML) string to datetime, and vice versa.
dateformat = '%Y-%m-%d %H:%M:%S'

# ------------------------------------------------------------------------------------------
# Store
# ------------------------------------------------------------------------------------------

# Store: class for storing 'properties' (i.e name,value pairs) in
#   hierarchical structure, using in-memory XML DOM. All values are stored as
#   strings, since XML is text-based; strings are converted to and from other
#   types (date, int, float, bool) whenever necessary.
class Store:
    """Class for storing "properties" (i.e name,value pairs) in
    hierarchical structure, using in-memory XML DOM. All values are stored as
    strings, since XML is text-based; strings are converted to and from other
    types (datetime, int, float, bool, etc.) whenever necessary."""

    class DataType:
        """Abstract class for user data types. Methods "load" and "save" MUST be
        implemented by inheriting classes.
        
        The "context" is a dictionary object that allows for data-type-specific
        storage on store level. For instance, it is currently used by DataFile
        objects as source of additional information (i.e., which data store to
        obtain external data files from), and for caching of values.
        """
        def __init__(self):
            pass
        
        @staticmethod
        def load(node,context,template=None):
            assert False, 'This virtual method MUST be overwritten by the inheriting class.'

        def save(self,node,context):
            assert False, 'This virtual method MUST be overwritten by the inheriting class.'

        def preparePersist(self,node,context):
            """Called just before the data store is saved, and can be used to prepare data
            for storage (e.g., the DataFile type checks in this method its source will be
            overwritten by the impending save, and loads it into memory if so)."""
            pass

        def persist(self,node,context):
            """Called when the store is saved to file. It may be used to store additional data
            in the saved store (see again the DataFile object)."""
            pass
            
        def validate(self,callback=None):
            return True
    
    def __init__(self,xmldocument=None,xmlroot=None):
        """Constructor of Store object."""
        if isinstance(xmldocument,basestring):
            assert xmlroot==None,'Path to XML file specified, but also a (already parsed!) root node was supplied. This combination is invalid'
            xmldocument = xml.dom.minidom.parse(xmldocument)

        self.xmldocument = xmldocument
        if xmlroot==None: xmlroot = xmldocument.documentElement
        self.xmlroot = xmlroot
        self.xmlnamespace = self.xmldocument.namespaceURI

        self.context = {}

        # The "filetypes" dictionary below links data type names (strings) to
        # their respective Python classes.
        #   This is particularly relevant for the later TypedStore, which
        #   uses these data type names in its template XML files. Note that this is extensible:
        #   one can simple add other data types to the self.filetypes dictionary; just make sure
        #   their classes are derived from Store.DataType.
        self.filetypes = {'string'  :unicode,
                          'int'     :int,
                          'float'   :float,
                          'bool'    :bool,
                          'datetime':datetime.datetime}

    def getText(self,node):
        """Gets all text directly below an XML element; may consist of multiple text nodes."""
        return common.getNodeText(node)

    def setText(self,node,text):
        """sets text directly below an XML element, using one text node.
        Replaces any existing child text nodes."""
        for ch in node.childNodes:
            if ch.nodeType == ch.TEXT_NODE:
                node.removeChild(ch)
                ch.unlink()
        val = self.xmldocument.createTextNode(text)
        node.insertBefore(val,node.firstChild)

    def setProperty(self,location,value,valuetype=None):
        """Sets specified location (list of ancestor names) to specified value.
        Autoconverts specified value to string format."""
        node = common.findDescendantNode(self.xmlroot,location,create=True)
        assert node!=None, 'Unable to create new child node at "%s".' % location
        return self.setNodeProperty(node,value,valuetype)

    def setNodeProperty(self,node,value,valuetype=None):
        """Sets specified node to specified value.
        Autoconverts specified value to string format."""
        if valuetype!=None:
            if isinstance(valuetype,basestring):
                assert valuetype in self.filetypes, 'unknown type "%s" requested.' % valuetype
                valuetype = self.filetypes[valuetype]
            if not isinstance(value,valuetype): value = valuetype(value)
        if isinstance(value,Store.DataType):
            return value.save(node,self.context)
        else:
            value = self.packValue(value,valuetype)
            if self.getText(node)==value: return False
            self.setText(node,value)
            return True

    def getProperty(self,location,valuetype=str,infonode=None):
        """Gets value at specified location (list of ancestor names).
        Autoconverts value to the type requested (otherwise value = string)."""
        node = common.findDescendantNode(self.xmlroot,location)
        if node==None: return None
        return self.getNodeProperty(node,valuetype=valuetype,infonode=infonode)

    def getNodeProperty(self,node,valuetype=str,infonode=None):
        """Gets value at node. Autoconverts value to the type requested (otherwise value = string)."""
        if isinstance(valuetype,basestring):
            assert valuetype in self.filetypes, 'unknown value type "%s" requested.' % valuetype
            valuetype = self.filetypes[valuetype]
        if issubclass(valuetype,Store.DataType):
            return valuetype.load(node,self.context,infonode)
        else:
            return self.unpackValue(self.getText(node),valuetype=valuetype)

    def preparePersistNode(self,node,valuetype,infonode=None):
        """Prepares the node for being saved to persistent storage. Additional
        information needed by the node should be made available through the
        dictionary present as context attribute of the Store."""
        value = self.getNodeProperty(node,valuetype,infonode=infonode)
        assert isinstance(value,Store.DataType), 'preparePersistNode should on be called for nodes of type Store.DataType.'
        value.preparePersist(node,self.context)
        if isinstance(value,common.referencedobject): value.release()

    def persistNode(self,node,valuetype,infonode=None):
        """Saves the node to persistent storage. Additional information needed
        by the node should be made available through the dictionary present as
        context attribute of the Store."""
        value = self.getNodeProperty(node,valuetype,infonode=infonode)
        assert isinstance(value,Store.DataType), 'persistNode should only be called for nodes of type Store.DataType.'
        value.persist(node,self.context)
        if isinstance(value,common.referencedobject): value.release()

    def clearNodeProperty(self,node):
        """Removes specified node."""
        node.parentNode.removeChild(node)

    def save(self,path):
        """Saves the current property tree to a UTF-8 encoded XML document."""
        common.stripWhitespace(self.xmlroot)
        self.xmldocument.writexml(file(path,'w'),encoding='utf-8',addindent='\t',newl='\n')

    def packValue(self,value,valuetype=None):
        """Converts a value to a string representation suitable for storing in XML.
        The data type of the value may be provided (as class object or string),
        causing the value to be cast to the desired type before being converted
        to string."""
        if valuetype!=None:
            if isinstance(valuetype,basestring):
                assert valuetype in self.filetypes, 'unknown type "%s" requested.' % valuetype
                valuetype = self.filetypes[valuetype]
            if not isinstance(value,valuetype): value = valuetype(value)
        assert not isinstance(value,Store.DataType), 'packValue should not be called for values of type Store.DataType.'
        if isinstance(value,datetime.datetime):
            return value.strftime(dateformat)
        elif isinstance(value,bool):
            if value: return 'True'
            else:     return 'False'
        else:
            return unicode(value)

    def unpackValue(self,value,valuetype=str):
        """Converts string representation of a value to the desired type."""
        if isinstance(valuetype,basestring):
            assert valuetype in self.filetypes, 'unknown value type "%s" requested.' % valuetype
            valuetype = self.filetypes[valuetype]
        assert not issubclass(valuetype,Store.DataType), 'values of type Store.DataType cannot be unpacked from string.'
        if valuetype==datetime.datetime:
            return common.parseDateTime(value,dateformat)
        elif valuetype==bool:
            return (value=='True')
        else:
            return valuetype(value)

class StoreTimeDelta(Store.DataType,datetime.timedelta):
    """Class representing a time span, capable of being stored in/loaded from
    an XML store. Time spans are stored using the XSD duration data type format."""
    
    def __init__(self,*args,**kwargs):
        Store.DataType.__init__(self)
        datetime.timedelta(*args,**kwargs)

    @staticmethod
    def load(node,context,template):
        """Loads the time span value from the specified XML DOM node."""
        text = common.getNodeText(node)
        if text:
            mult = 1
            if text[0]=='-':
                mult = -1
                text = text[1:]
            assert text[0]=='P', 'A stored duration/timedelta should always start with "P".'
            import re
            m = re.match('P(\d+Y)?(\d+M)?(\d+D)?(?:T(\d+H)?(\d+M)?(\d+(?:\.\d*)S)?)?',text)
            assert m!=None, 'The string "%s" is not a valid duration/timedelta.' % text
            grps = m.groups('0x')
            y,m,d,hh,mm = [int(t[:-1]) for t in grps[:-1]]
            ss = float(grps[-1][:-1])
            return StoreTimeDelta(days=mult*(y*365+m*30+d),seconds=mult*(hh*3600+mm*60+ss))
        else:
            return StoreTimeDelta()

    def save(self,node,context):
        """Saves the time span value to the specified XML DOM node."""
        common.setNodeText(node,'P%iDT%fS' % (self.days,self.seconds+self.microseconds/1000000.))
        
    def getAsSeconds(self):
        """Gets the number of seconds in the time span as floating point value."""
        return self.days*86400 + self.seconds + self.microseconds/1000000.
        
    def __float__(self):
        """Returns the number of seconds in the time span as floating point value."""
        return float(self.getAsSeconds())
        
    def __str__(self):
        """Returns a "pretty" string representation of the time span."""
        values = []
        
        # Add days
        if self.days>0:
            values.append([self.days,'day'])
            
        # Divide seconds over hours, minutes, and remaining seconds.
        seconds = self.seconds
        hours = int(seconds/3600)
        if hours>0:
            values.append([hours,'hour'])
            seconds = (seconds % 3600)            
        minutes = int(seconds/60)
        if minutes>0:
            values.append([minutes,'minute'])
            seconds = (seconds % 60)
        if seconds>0:
            values.append([seconds,'second'])
            
        # Add microseconds
        if self.microseconds>0:
            values.append([self.microseconds,'microsecond'])
            
        # Add a trailing "s" for each unit that will have value > 1
        for v in values:
            if v[0]>1: v[1]+='s'
            
        # Combine units into string.
        return ', '.join(['%i %s' % (v[0],v[1]) for v in values])

class StoreColor(Store.DataType):
    """Class representing a color (RGB), capable of being stored in/loaded from
    an XML store."""

    def __init__(self,red=None,green=None,blue=None):
        Store.DataType.__init__(self)
        self.red = red
        self.green = green
        self.blue = blue

    @staticmethod
    def load(node,context,template):
        """Creates a StoreColor object with its value read from the specified XML node."""
        strcolor = common.getNodeText(node)
        if len(strcolor)>0:
            assert len(strcolor)==7 and strcolor[0]=='#', 'Colors must have exactly 7 characters and start with #.'
            strcolor = strcolor[1:]
            return StoreColor(int(strcolor[0:2],16),int(strcolor[2:4],16),int(strcolor[4:6],16))
        else:
            return StoreColor()
            
    @staticmethod
    def fromNormalized(r,g,b):
        """Creates a StoreColor object from normalized color values (between 0 and 1, as used by MatPlotLib)."""
        return StoreColor(int(r*255),int(g*255),int(b*255))
        
    def brighten(self,value):
        """Brightens the color with the specified value (between 0 and 1)."""
        assert value>=0 and value<=1, 'Brighten value must be between 0 and 1.'
        self.red   = int(self.red  +(255.-self.red)  *value)
        self.green = int(self.green+(255.-self.green)*value)
        self.blue  = int(self.blue +(255.-self.blue) *value)
        
    def copy(self):
        """Returns a copy of the StoreColor object."""
        return StoreColor(self.red,self.green,self.blue)

    def save(self,node,context):
        """Saves the value of the StoreColor object to the specified XML node.
        This value can be retrieved later through the static "load" method."""
        assert self.red  ==None or self.red  <=255, 'Color channel red values should not exceed 255.'
        assert self.green==None or self.green<=255, 'Color channel green values should not exceed 255.'
        assert self.blue ==None or self.blue <=255, 'Color channel blue values should not exceed 255.'
        if self.isValid():
            common.setNodeText(node,'#%02x%02x%02x' % (self.red,self.green,self.blue))
        else:
            common.removeNodeChildren(node)
        
    def isValid(self):
        """Returns whether the object currently contains a valid color."""
        return self.red!=None and self.green!=None and self.blue!=None
        
    def getNormalized(self):
        """Returns a tuple with normalized RGB color values (between 0 and 1)."""
        assert self.isValid(), 'Cannot convert color to normalized tuple because the color object is not valid.'
        return (self.red/255.,self.green/255.,self.blue/255.)
        
    def __str__(self):
        """Returns a string representation of the color."""
        if self.isValid():
            return '(%i, %i, %i)' % (self.red,self.green,self.blue)
        else:
            return 'None'
            
    def __eq__(self,other):
        if not isinstance(other,StoreColor): return False
        return self.red==other.red and self.green==other.green and self.blue==other.blue
        
    def __ne__(self,other):
        return not self.__eq__(other)
            

# ------------------------------------------------------------------------------------------
# DataFile
# ------------------------------------------------------------------------------------------

class DataContainer(common.referencedobject):
    """Abtract data container, e.g., a directory or compressed archive.
    Items in the container are identified by name."""
    def __init__(self):
        common.referencedobject.__init__(self)
        
    @staticmethod
    def fromPath(path):
        """Returns a DataContainer for the specified path. Directories, zip files
        and tar.gz files are currently supported containers."""
        if os.path.isdir(path):
            return DataContainerDirectory(path)
        elif os.path.isfile(path):
            if zipfile.is_zipfile(path):
                return DataContainerZip(path)
            elif tarfile.is_tarfile(path):
                return DataContainerTar(path)
            else:
                raise Exception('File "%s" is not a zip or tar archive.' % path)
        else:
            raise Exception('"%s" is not an existing file or directory.' % path)
    
    def listFiles(self):
        """Returns a list of all files in the container."""
        return []

    def getItem(self,name):
        """Returns the item with the given name from the container as DataFile
        object."""
        return None

    def addItem(self,datafile,newname=None):
        """Adds a DataFile object to the container. If a name for the object
        is provided, it is used instead of the name currently associated with
        the supplied DataFile object."""
        return None

    def addFile(self,path,newname=None):
        """Adds a file at the specified path to the container. If a name for
        the object is provided, it is used instead of the name of the source
        file."""
        df = DataContainerDirectory.DataFileFile(path)
        df_added = self.addItem(df,newname)
        df_added.release()
        df.release()

    def persistChanges(self):
        """Makes sure all changes to the container are saved to persistent
        storage. This may involve flushing buffers etc."""
        pass

class DataFile(Store.DataType,common.referencedobject):
    """Abstract Class that encapsulates a data block, which can be a file
    on disk, an item in a zip or tar/gz archive, or a memory block. It can be
    used as data type in the xml stores.
    """
    def __init__(self,name=''):
        common.referencedobject.__init__(self)
        Store.DataType.__init__(self)
        self.name = None
    
    @staticmethod
    def load(node,context,template):
        """Loads the data file from the specified XML node. Currently the XML node
        should contain the name of the data object within its container, and the
        container (DataContainer instance) should be specified through the context
        dictionary.
        """
        assert 'container' in context, 'container key not present in context dictionary.'
        if 'cache' in context:
            uniquename = DataFile.getUniqueNodeName(node)
            cache = context['cache']
            if uniquename in cache: return cache[uniquename].addref()
        container = context['container']
        if container==None: return DataFile()
        name = common.getNodeText(node)
        if name not in container.listFiles(): return DataFile()
        return container.getItem(name)

    def save(self,node,context):
        """Saves the DataFile to the specified XML node. Currently the node will
        contain the name of the datafile within its container (DataContainer
        instance). The container itself is then saved through the persist method
        below. Note: when saved, the ownership of the DataFile object is transfered
        from the calling code to the store object. The calling code should therefore
        not use, or call "release" on the DataFile object afterwards.
        """
        cache = context.setdefault('cache',{})
        uniquename = DataFile.getUniqueNodeName(node)
        if uniquename in cache: cache[uniquename].release()
        cache[uniquename] = self.addref()
        if self.name!=None:
            common.setNodeText(node,self.name)
        else:
            common.setNodeText(node,'')

    def preparePersist(self,node,context):
        """Prepares the data file object for being saved to persistent storage.
        Currently, this method checks whether the upcoming save will overwrite
        the current data file. If so, the data file is first read into memory.
        """
        assert 'targetcontainerpath' in context, 'preparePersist: "targetcontainerpath" not set in XML store context.'
        if not self.isValid(): return
        targetpath = context['targetcontainerpath']
        if self.isBelowPath(targetpath):
            print 'Reading "%s" into memory to prevent it from being overwritten.' % self.name
            memdf = DataFileMemory.fromDataFile(self)
            memdf.save(node,context)
            memdf.release()

    def persist(self,node,context):
        """Saves the data file to persistent storage. The container to save to
        must be specified via the "targetcontainer" key in the context dictionary.
        Additionally, the "donotclaimtarget" may be set to True to prevent future
        use of the new container as source of the data file.
        """
        assert 'targetcontainer' in context, 'persist: "targetcontainer" not set in XML store context.'
        if not self.isValid(): return
        targetcontainer = context['targetcontainer']
        newname = node.localName+'.dat'
        df = targetcontainer.addItem(self,newname)
        if context.get('donotclaimtarget',False):
            common.setNodeText(node,df.name)
        else:
            df.save(node,context)
        df.release()

    @staticmethod
    def getUniqueNodeName(node):
        """Returns a unique name for the specified node, usable as index into
        a container-wide cache.
        """
        path = []
        while node.parentNode.parentNode!=None:
            path.append(node.localName)
            node = node.parentNode
        return '/'.join(path)

    def isValid(self):
        """Returns True if the data file object is valid (has data associated
        with it; False if not.
        """
        return self.name!=None

    def getAsReadOnlyFile(self,textmode=True):
        """Returns the data stored in the data file as a read-only, file-like
        object.
        
        The default implementation reads all data into memory
        (using DataFile.getData), then returns a StringIO object that
        encapsulates the data block in memory.
        
        Deriving classes MUST implement getAsReadOnlyFile and/or getData!
        """
        data = self.getData(textmode=textmode,readonly=True)
        return StringIO.StringIO(data)

    def getData(self,textmode=True,readonly=False):
        """Returns the contents of the data file as a string of bytes.
        
        The default implementation accesses the data through a file-like object
        (obtained through getAsReadOnlyFile), then reads and returns all its
        data.
        
        Deriving classes MUST implement getAsReadOnlyFile and/or getData!
        """
        f = self.getAsReadOnlyFile(textmode=textmode)
        data = f.read()
        f.close()
        return data

    def saveToFile(self,targetpath):
        """Saves the contents of the data file to the specified path in the file
        system.
        
        The default implementation accesses the data through a file-like object
        (obtained through getAsReadOnlyFile), then reads from this stream while
        writing to disk (using shutil.copyfileobj).
        """
        assert self.isValid(), 'saveToFile: DataFile is not valid (i.e., empty).'
        fsource = self.getAsReadOnlyFile(textmode=False)
        ftarget = open(targetpath,'wb')
        shutil.copyfileobj(fsource,ftarget)
        ftarget.close()
        fsource.close()

    def addToZip(self,zfile,filename):
        """Adds the contents of the data file to a ZIP archive. (zipfile.ZipFile)
        
        The default implementation loads all data into memory (via
        DataFile.getData), then writes it all to the archive.
        """
        assert self.isValid(), 'addToZip: DataFile is not valid (i.e., empty).'
        zfile.writestr(str(filename),self.getData(textmode=False,readonly=True))

    def addToTar(self,tfile,filename):
        """Adds the contents of the data file to a tar.gz archive (tarfile.TarFile)
        
        The default implementation loads all data into memory (via
        DataFile.getData), then writes it all to the archive.
        """
        assert self.isValid(), 'addToTar: DataFile is not valid (i.e., empty).'
        data = self.getData(textmode=False,readonly=True)
        tarinfo = tarfile.TarInfo(filename)
        tarinfo.size = len(data)
        import time
        tarinfo.mtime = time.time()
        tfile.addfile(tarinfo,StringIO.StringIO(data))

    def isBelowPath(self,path):
        """Returns True if the data file is located somewhere below the specified
        path.
        
        This is used in DataFile.preparePersist to check whether the data file
        would be overwritten if the specified path is saved to.
        """
        return False

    def getSize(self):
        """Returns the size (in number of bytes) of the data file.
        """
        return None
        # Below an expensive way to get the size. Disabled: if the user really
        # wants this, he should do it himself.
        return len(self.getData(textmode=False,readonly=True))

class DataFileEx(Store.DataType,common.referencedobject):
    @classmethod
    def load(ownclass,valuenode,context,infonode):
        """Creates a DataFileEx object from the data in the specified XML
        node, and the associated data stream located in the container object
        specified in the context dictionary.
        """
        datafile = DataFile.load(valuenode,context,infonode)
        res = ownclass.createObject(datafile,context,infonode,valuenode.localName)
        datafile.release()
        return res
        
    @classmethod
    def createObject(ownclass,datafile,context,infonode,nodename):
        """Returns a DataFileEx object from a data file, XML node with
        metadata and (optionally) a node in an XML schema, specified
        additional information on the data file.
        """
        return ownclass(datafile,context,infonode,nodename)
    
    @classmethod
    def fromNode(ownclass,node,datafile=None,context=None):
        return ownclass.createObject(datafile,context,node.templatenode,node.getId())

    @classmethod    
    def createTypedStore(ownclass):
        """Returns a TypedStore object to be used for the metadata belonging to
        the data file object. Must be overridden by derived classes.
        """
        assert False, 'createTypedStore must be overridden by inheriting class.'
        
    linkedfilename = None
    rootnodename = None

    def __init__(self,datafile=None,context=None,infonode=None,nodename=None):
        common.referencedobject.__init__(self)
        Store.DataType.__init__(self)

        assert nodename!=None or context==None, 'If a context is provided you must also specify a node name. That name will be used to uniquely identify the node in the context.'

        self.nodename = nodename
        
        # Create a global store for metadata if it does not exist yet.
        if context==None: context = {'fake':True}
        linkedfiles = context.setdefault('linkedobjects',{})
        if self.linkedfilename not in linkedfiles:
            # Store for metadata does not exist yet: create it.
            store = self.createTypedStore()
            assert store!=None, 'No typed store returned by createTypedStore.'
            linkedfiles[self.linkedfilename] = store
            
            # If the source container already contains metadata, load them.
            container = context.get('container',None)
            if container!=None:
                if self.linkedfilename in container.listFiles():
                    metadatafile = container.getItem(self.linkedfilename)
                    store.load(metadatafile)
                    metadatafile.release()
        else:
            # Store for metadata exists
            store = linkedfiles[self.linkedfilename]

        # Reference the metadata store (because we will keep a link to it)
        self.store = store.addref()
        
        # If we created a fake context on the spot, it is now keeping a
        # reference to the data store. Undo this, because the context will go
        # out of scope.
        if context.get('fake',False): self.store.release()

        # Initialize data file and meta data.
        self.datafile = None
        self.metadata = None
        
        # Attach the data file object
        # (but make sure any existing meta data is not erased)
        self.keepmetadata = True
        self.setDataFile(datafile)
        self.keepmetadata = False
        
    def setDataFile(self,datafile=None):
        """Attach to the specified data file object, and simultaneously
        erase all currently stored metadata (because it belonged to the
        previous data file.
        """
        # Release previous data file (if any)
        if self.datafile!=None:
            self.datafile.release()
            self.datafile = None
            
        # Set new data file (if any)
        if datafile!=None:
            self.datafile = datafile.addref()

        # Clear meta data (unless we are initializing: then keepmetadata is set).
        if not self.keepmetadata:
            self.store.release()
            self.metadata = None
            self.store = self.createTypedStore()
            
    def getMetaData(self,create=True):
        """Get the node in the TypedStore that stores the metadata for this
        object. Note that the metadata node is cached for further use.
        """
        if self.metadata!=None: return self.metadata
        self.metadata = self.store.root.getChildById(self.rootnodename,self.nodename,create=create)
        return self.metadata

    def getDataFile(self,callback=None):
        """Returns the current data as data file object. This does nothing
        interesting in this implementation, but might be used in derived
        classes to build the data file "lazily", i.e., only when needed.
        """
        return self.datafile.addref()

    def save(self,node,context):
        """Stores current data to the specified XML node. The data stream
        (DataFile object) is stored by name in the container that is
        specified in the context dictionary.
        """
        df = self.getDataFile()
        df.save(node,context)
        df.release()

        self.nodename = node.localName
        
        # Get the global store for metadata in the target container.
        # Create the metadata store if it does not exist yet.
        linkedfiles = context.setdefault('linkedobjects',{})
        if self.linkedfilename not in linkedfiles:
            newstore = self.createTypedStore()
            assert newstore!=None, 'No typed store returned by createTypedStore.'
            linkedfiles[self.linkedfilename] = newstore
        else:
            newstore = linkedfiles[self.linkedfilename]
            
        # Copy old metadata to the target store, and connect to the target
        # store for future reference.
        if newstore is not self.store:
            oldmetadata = self.getMetaData(create=False)
            if oldmetadata!=None:
                newmetadata = newstore.root.getChildById(self.rootnodename,self.nodename,create=True)
                newmetadata.copyFrom(oldmetadata)
            self.store.release()
            self.metadata = None
            self.store = newstore.addref()

    def preparePersist(self,node,context):
        df = self.getDataFile()
        df.preparePersist(node,context)
        df.release()

    def persist(self,node,context):
        df = self.getDataFile()
        df.persist(node,context)
        df.release()
        
    def __str__(self):
        if self.datafile==None or not self.datafile.isValid():
            return ''
        else:
            return self.datafile.name
        
    def unlink(self):
        self.metadata = None
        if self.datafile!=None: self.datafile.release()
        if self.store   !=None: self.store.release()

class DataContainerDirectory(DataContainer):
    """A DataContainer implementation for a directory in the file system.
    """
    
    class DataFileFile(DataFile):
        """A DataFile implementation for a file in the file system. Default
        implementations of DataFile are (implicitly) used where suitable, and
        replaced where a custom implementation is more efficient.
        """
        def __init__(self,path):
            DataFile.__init__(self)
            assert os.path.isfile(path), 'Specified path "%s" is not a file.' % path
            self.path = path
            self.name = os.path.basename(self.path)

            # Create a lock on the file; while we live, its contents should persist.
            self.file = open(self.path,'r')

        def __del__(self):
            self.unlink()

        def getAsReadOnlyFile(self,textmode=True):
            """Returns the data stored in the data file as a read-only, file-like
            object.
            """
            if textmode:
                return open(self.path,'rU')
            else:
                return open(self.path,'rb')

        def saveToFile(self,targetpath):
            """Saves the contents of the data file to the specified path in the file
            system.
            """
            if self.path==targetpath: return
            shutil.copyfile(self.path,targetpath)

        def addToZip(self,zfile,filename):
            """Adds the contents of the data file to a ZIP archive. (zipfile.ZipFile)
            """
            assert self.isValid()
            zfile.write(self.path,str(filename))

        def addToTar(self,tfile,filename):
            """Adds the contents of the data file to a tar.gz archive (tarfile.TarFile)
            """
            assert self.isValid()
            tfile.add(self.path,filename,recursive=False)

        def isBelowPath(self,path):
            """Returns True if the data file is located somewhere below the specified
            path.
            
            This is used in DataFile.preparePersist to check whether the data file
            would be overwritten if the specified path is saved to.
            """
            owndir = os.path.normcase(os.path.dirname(self.path))
            return owndir.startswith(os.path.normcase(path))

        def getSize(self):
            """Returns the size (in number of bytes) of the data file.
            """
            return os.path.getsize(self.path)

        def unlink(self):
            """Destroys the data file object. This closes open streams etc."""
            if self.file==None: return
            self.file.close()
            self.file = None
   
    def __init__(self,path,create=False):
        DataContainer.__init__(self)
        assert os.path.isdir(path) or create
        if not os.path.isdir(path):
            try:
                os.mkdir(path)
            except Exception,e:
                raise Exception('Unable to create directory "%s". Error: %s' % (path,str(e)))
        self.path = path

    def getItem(self,name):
        """Returns specified file from the directory as DataFile object.
        """
        sourcepath = os.path.join(self.path,name)
        if not os.path.isfile(sourcepath): return None
        return self.DataFileFile(sourcepath)

    def addItem(self,datafile,newname=None):
        """Adds a DataFile object to the directory. If a name for the object
        is provided, it is used instead of the name currently associated with
        the supplied DataFile object.
        """
        assert datafile.isValid()
        if newname==None: newname = datafile.name
        targetpath = os.path.join(self.path,newname)
        datafile.saveToFile(targetpath)
        return self.DataFileFile(targetpath)

    def listFiles(self):
        """Returns a list of all files in the directory.
        """
        res = []
        for fn in os.listdir(self.path):
            if os.path.isfile(os.path.join(self.path,fn)): res.append(fn)
        return res

class DataContainerZip(DataContainer):
    """A DataContainer implementation for zip archives.
    """

    class DataFileZip(DataFile):
        """A DataFile implementation for a file in a zip archive. Default
        implementations of DataFile are (implicitly) used where suitable, and
        replaced where a custom implementation is more efficient.
        """
        def __init__(self,zipcontainer,name):
            DataFile.__init__(self)
            self.zipcontainer = zipcontainer
            self.zipcontainer.addref()
            self.name = name

        def __del__(self):
            self.unlink()

        def getData(self,textmode=True,readonly=False):
            """Returns the contents of the data file as a string of bytes.
            """
            assert self.zipcontainer!=None, 'DataFileZip.getData failed; ZIP file has been closed.'
            self.zipcontainer.setMode('r')
            return self.zipcontainer.zfile.read(self.name)

        def isBelowPath(self,path):
            """Returns True if the data file is located somewhere below the specified
            path.
            
            This is used in DataFile.preparePersist to check whether the data file
            would be overwritten if the specified path is saved to.
            """
            assert self.zipcontainer!=None, 'DataFileZip.isBelowPath failed; ZIP file has been closed.'
            owndir = os.path.normcase(self.zipcontainer.path)
            return owndir.startswith(os.path.normcase(path))

        def getSize(self):
            """Returns the size (in number of bytes) of the data file.
            """
            return self.zipcontainer.zfile.getinfo(self.name).file_size

        def unlink(self):
            """Destroys the data file object. This closes open streams etc."""
            if self.zipcontainer==None: return
            self.zipcontainer.release()
            self.zipcontainer = None
    
    def __init__(self,path,mode='r'):
        DataContainer.__init__(self)
        if isinstance(path,basestring):
            assert os.path.isfile(path) or mode=='w', 'Cannot initialize DataContainerZip with supplied path; it does not point to an existing file, but is also not opened for writing.'
        elif isinstance(path,StringIO.StringIO):
            assert mode=='w', 'Can initialize DataContainerZip with StringIO object only in write-only mode.'
        elif isinstance(path,DataFile):
            assert mode=='r', 'Can initialize DataContainerZip with file-like object only in read-only mode.'
        else:
            assert False, 'Cannot initialize DataContainerZip with %s.' % path
        self.mode = None
        self.zfile = None
        self.path = path
        if isinstance(self.path,DataFile): self.path.addref()
        self.setMode(mode)

    def __del__(self):
        self.unlink()

    def unlink(self):
        """Destroys the data container object. This closes open streams,
        closes open files, cleans up, etc.
        """
        if self.zfile==None: return
        self.zfile.close()
        self.zfile = None
        self.mode = None
        if isinstance(self.path,DataFile): self.path.release()
        self.path = None
    
    def getItem(self,name):
        """Returns specified file from the zip archive as DataFile object.
        """
        if name not in self.listFiles(): return None
        return self.DataFileZip(self,name)

    def addItem(self,datafile,newname=None):
        """Adds a DataFile object to the zip archive. If a name for the object
        is provided, it is used instead of the name currently associated with
        the supplied DataFile object.
        """
        if newname==None: newname = datafile.name
        if self.mode=='r': self.setMode('a')
        if isinstance(self.path,StringIO.StringIO):
            print 'Adding "%s" to in-memory archive...' % (newname,)
        else:
            print 'Adding "%s" to archive "%s"...' % (newname,self.path)
        datafile.addToZip(self.zfile,newname)
        return self.DataFileZip(self,newname)

    def listFiles(self):
        """Returns a list of all files in the zip archive.
        """
        return self.zfile.namelist()

    def persistChanges(self):
        """Makes sure all changes to the container are saved to persistent
        storage. This may involve flushing buffers etc.
        """
        if self.zfile==None: return
        if isinstance(self.path,basestring):
            # Immediately re-open the ZIP file so we keep a lock on it.
            self.setMode('r')
        else:
            # No way to re-open the zip file because the source path is not set.
            # (could be a file in memory). Close it to make any changes
            # persistent - the object will be useless after this!!
            self.zfile.close()
            self.zfile = None

    def setMode(self,mode):
        """Switches the mode (read, write, append) in which the zip archive
        is accessed.
        """
        assert mode in ('r','w','a'), 'DataContainerZip.setMode: mode must be "r", "w", or "a" (not "%s").' % mode
        if self.zfile!=None:
            if mode==self.mode: return
            self.zfile.close()
        self.mode = mode
        if isinstance(self.path,StringIO.StringIO):
            # Writing to in-memory data block.
            assert self.mode=='w', 'In-memory data blocks can only be written to, not read from.'
            self.zfile = zipfile.ZipFile(self.path,self.mode,zipfile.ZIP_DEFLATED)
        if isinstance(self.path,DataFile):
            # Reading from generic DataFile object.
            assert self.mode=='r', 'Data file objects can only be accessed as read-only zip file.'
            f = self.path.getAsReadOnlyFile()
            self.zfile = zipfile.ZipFile(f,self.mode,zipfile.ZIP_DEFLATED)
        else:
            # Reading from/writing to file.
            self.zfile = zipfile.ZipFile(self.path,self.mode,zipfile.ZIP_DEFLATED)

class DataContainerTar(DataContainer):
    """A DataContainer implementation for tar and tar.gz archives.
    """
    class DataFileTar(DataFile):
        """A DataFile implementation for a file in a tar archive. Default
        implementations of DataFile are (implicitly) used where suitable, and
        replaced where a custom implementation is more efficient.
        """
        def __init__(self,tarcontainer,name):
            DataFile.__init__(self)
            self.tarcontainer = tarcontainer
            self.tarcontainer.addref()
            self.name = name

        def __del__(self):
            self.unlink()

        def getAsReadOnlyFile(self,textmode=True):
            """Returns the data stored in the data file as a read-only, file-like
            object.
            """
            assert self.tarcontainer!=None, 'DataFileTar.getAsReadOnlyFile failed; TAR file has been closed.'
            self.tarcontainer.setMode('r')
            return self.tarcontainer.tfile.extractfile(self.name)

        def isBelowPath(self,path):
            """Returns True if the data file is located somewhere below the specified
            path.
            
            This is used in DataFile.preparePersist to check whether the data file
            would be overwritten if the specified path is saved to.
            """
            assert self.tarcontainer!=None, 'DataFileTar.isBelowPath failed; TAR file has been closed.'
            owndir = os.path.normcase(self.tarcontainer.path)
            return owndir.startswith(os.path.normcase(path))

        def getSize(self):
            """Returns the size (in number of bytes) of the data file.
            """
            return self.tarcontainer.tfile.getmember(self.name).size

        def unlink(self):
            """Destroys the data file object. This closes open streams etc."""
            if self.tarcontainer == None: return
            self.tarcontainer.release()
            self.tarcontainer = None

    def __init__(self,path,mode='r'):
        DataContainer.__init__(self)
        assert os.path.isfile(path) or mode=='w', 'Cannot initialize DataContainerTar with supplied path; it does not point to an existing file, but is also not opened for writing.'
        self.mode = None
        self.tfile = None
        self.path = path
        self.setMode(mode)

    def __del__(self):
        self.unlink()

    def unlink(self):
        """Destroys the data container object. This closes open streams,
        closes open files, cleans up, etc.
        """
        if self.tfile==None: return
        self.tfile.close()
        self.mode = None
        self.tfile = None
    
    def getItem(self,name):
        """Returns specified file from the zip archive as DataFile object.
        """
        if name not in self.listFiles(): return None
        return self.DataFileTar(self,name)

    def addItem(self,datafile,newname=None):
        """Adds a DataFile object to the tar archive. If a name for the object
        is provided, it is used instead of the name currently associated with
        the supplied DataFile object.
        """
        if newname==None: newname = datafile.name
        if self.mode=='r': self.setMode('a')
        print 'Adding "%s" to archive "%s"...' % (newname,self.path)
        datafile.addToTar(self.tfile,newname)
        return self.DataFileTar(self,newname)

    def listFiles(self):
        """Returns a list of all files in the tar archive.
        """
        return self.tfile.getnames()

    def persistChanges(self):
        """Makes sure all changes to the container are saved to persistent
        storage. This may involve flushing buffers etc.
        """
        if self.tfile==None: return
        self.setMode('r')

    def setMode(self,mode):
        """Switches the mode (read, write) in which the tar archive
        is accessed.
        """
        assert mode in ('r','w'), 'DataContainerZip.setMode: mode must be "r" or "w" (not "%s").' % mode
        if mode=='w': mode='w:gz'
        if self.tfile!=None:
            if mode==self.mode: return
            self.tfile.close()
        self.mode = mode
        self.tfile = tarfile.open(str(self.path),self.mode)

class DataFileXmlNode(DataFile):
    """A DataFile implementation for an XML DOM node. Default
    implementations of DataFile are (implicitly) used where suitable, and
    replaced where a custom implementation is more efficient.
    """
    def __init__(self,xmlnode,name=''):
        DataFile.__init__(self)
        self.xmlnode = xmlnode
        self.name = name

    def __del__(self):
        self.unlink()

    def getData(self,textmode=True,readonly=False):
        """Returns the contents of the XML node as a string of bytes (XML).
        """
        return self.xmlnode.toxml('utf-8')

    def saveToFile(self,targetpath):
        """Saves the contents of the XML node to a file at the specified path.
        UTF-8 encoding is used.
        """
        import codecs
        f = codecs.open(targetpath,'w','utf-8')
        self.xmlnode.writexml(f,encoding='utf-8')
        f.close()

    def unlink(self):
        """Destroys the data file and performs clean up.
        """
        self.xmlnode = None
        self.name = None

class DataFileMemory(DataFile):
    """A DataFile implementation for an in-memory data block. Default
    implementations of DataFile are (implicitly) used where suitable, and
    replaced where a custom implementation is more efficient.
    """
    def __init__(self,data,name):
        DataFile.__init__(self)
        self.data = data
        self.name = name

    def __del__(self):
        self.unlink()

    @staticmethod
    def fromDataFile(df):
        """Creates a DataFileMemory object from another data file.
        (this means the contents of the other file is read completely into memory)
        """
        return DataFileMemory(df.getData(),df.name)

    def getAsReadOnlyFile(self,textmode=True):
        """Returns the data stored in the data file as a read-only, file-like
        object.
        """
        return StringIO.StringIO(self.data)

    def getData(self,textmode=True,readonly=False):
        """Returns the contents of the object as a string of bytes.
        """
        if readonly:
            return self.data
        else:
            return self.data[:]

    def unlink(self):
        """Destroys the data file and performs clean up.
        """
        self.data = None
        self.name = None

    def getSize(self):
        """Returns the size (in number of bytes) of the data file.
        """
        return len(self.data)

class Schema:
    """Class for managing XML-based schemas, used to define TypedStore objects.
    Supports caching of schemas (based on file path), parsing of schemas
    (i.e., inserting linked templates, resolving dependencies), and provides
    access to the main properties (version and root of the XML tree).
    """
    cache = {}
    
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
    
    def __init__(self,source):
        
        # The template can be specified as a DOM object, or as string (i.e. path to XML file)
        path = ''
        if isinstance(source,basestring):
            path = source
            if not os.path.isfile(source):
                raise Exception('XML schema file "%s" does not exist.' % source)
            self.dom = xml.dom.minidom.parse(source)
        elif isinstance(source,xml.dom.minidom.Document):
            self.dom = source
        else:
            assert False, 'Supplied argument must either be a string or an XML DOM tree. Got %s.' % source
            
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
                linkedpath = os.path.abspath(os.path.join(os.path.dirname(sourcepath),link.getAttribute('path')))
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
                    newnode = common.copyNode(ch,linkparent,targetdoc=dom)
            else:
                newnode = common.copyNode(templateroot,linkparent,targetdoc=dom,name='element',before=link)
                
                # Copy attributes and children of link node to new node.
                for key in link.attributes.keys():
                    if key not in ('path','template','skiproot'):
                        newnode.setAttribute(key,link.getAttribute(key))
                for ch in link.childNodes:
                    common.copyNode(ch,newnode,targetdoc=dom)
                    
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
        if root==None: root=self.dom.documentElement
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
                    
    # getTemplateNode: obtains template node at given path
    # (path specification consists of array of node ids)
    def getNodeFromPath(self,path,root=None):
        """Obtains DOM node in schema at specified path. If a reference node
        is provided, the path is assumed to be relative to the reference node.
        If no reference node is provided, the path is assumed absolute, that is,
        relative to the schema root element."""
        if root==None: root=self.dom.documentElement
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
        while node.parentNode.parentNode!=None:
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
        if absourcepath==None: '/'.join(self.getPathFromNode(sourcenode))
        
        refnode = None
        if targetpath[0]!='/': refnode = sourcenode.parentNode
        splittargetpath = targetpath.split('/')
        targetnode = self.getNodeFromPath(splittargetpath,root=refnode)
        assert targetnode!=None, 'Cannot locate target node "%s" for node "%s".' % (targetpath,absourcepath)
        
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
        deplist = common.findDescendantNode(node,['dependentvariables'],create=True)
        depnode = self.dom.createElementNS(deplist.namespaceURI,'dependentvariable')
        depnode.setAttribute('path',dependantnodepath)
        depnode.setAttribute('type',type)
        deplist.appendChild(depnode)

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
    def __init__(self,store,showhidden=True,omitgroupers=False):
        self.showhidden = showhidden
        self.omitgroupers = omitgroupers
        self.blockNotifyOfHiddenNodes = (not showhidden)
        self.processDefaultChange = 0

        self.eventhandlers = {}

        store.connectInterface(self)
        
    def unlink(self):
        assert self.eventhandlers!=None, 'unlink called on TypedStoreInterface for the second time.'
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
            assert node.futureindex!=None, 'Could not find node "%s" in children of supposed parent, but future index was also not set. Data: %s' % (node,node.valuenode.toxml('utf-8'))
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

        if tr!=None and hidedefaults:
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
        if 'beforeVisibilityChange' not in self.eventhandlers: return
        if self.blockNotifyOfHiddenNodes and self.getParent(node).isHidden(): return
        if self.blockNotifyOfHiddenNodes and (not showhide) and node.isHidden(): return
        if self.omitgroupers and node.grouponly:
            for ch in self.getChildren(node): self.eventhandlers['beforeVisibilityChange'](ch,shownew,showhide)
        else:
            self.eventhandlers['beforeVisibilityChange'](node,shownew,showhide)

    def afterVisibilityChange(self,node,shownew,showhide):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        if 'afterVisibilityChange' not in self.eventhandlers: return
        if self.blockNotifyOfHiddenNodes and self.getParent(node).isHidden(): return
        if self.blockNotifyOfHiddenNodes and (not showhide) and node.isHidden(): return
        if self.omitgroupers and node.grouponly:
            for ch in self.getChildren(node): self.eventhandlers['afterVisibilityChange'](ch,shownew,showhide)
        else:
            self.eventhandlers['afterVisibilityChange'](node,shownew,showhide)

    def afterStoreChange(self):
        if 'afterStoreChange' not in self.eventhandlers: return
        self.eventhandlers['afterStoreChange']()

    def onBeforeChange(self,node,newvalue):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        if 'beforeChange' not in self.eventhandlers: return True
        if node.isHidden() and self.blockNotifyOfHiddenNodes: return True
        return self.eventhandlers['beforeChange'](node,newvalue)

    def onChange(self,node,feature):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        if 'afterChange' not in self.eventhandlers: return
        if node.isHidden() and self.blockNotifyOfHiddenNodes: return
        self.eventhandlers['afterChange'](node,feature)

    def onDefaultChange(self,node,feature):
        assert isinstance(node,Node), 'Supplied object is not of type "Node" (but "%s").' % node
        assert node.isValid(), 'Supplied node %s is invalid (has already been destroyed).' % node
        if self.processDefaultChange==1 or (self.processDefaultChange==0 and not node.hasValue()):
            self.onChange(node,feature)

class Node:
    def __init__(self,controller,templatenode,valuenode,location,parent):
        assert templatenode.hasAttribute('name'),'Schema node %s lacks "name" attribute.' % location

        self.controller = controller
        self.store = controller.store
        self.templatenode = templatenode
        self.valuenode = valuenode
        self.location = tuple(location)
        self.parent = parent
        self.children = []
        self.futureindex = None
        self.visible = (not self.templatenode.hasAttribute('hidden'))
        self.grouponly = self.templatenode.hasAttribute('grouponly')

        ids = []
        for templatechild in self.templatenode.childNodes:
            if templatechild.nodeType==templatechild.ELEMENT_NODE and templatechild.localName=='element':
                childid = templatechild.getAttribute('name')
                ids.append(childid)
                
                # Get all value nodes that correspond to the current template child.
                valuechildren = ()
                if not templatechild.hasAttribute('maxoccurs'):
                    # The node always occurs exactly one time.
                    if self.valuenode!=None:
                        valuechildren = (common.findDescendantNode(self.valuenode,[childid]),)
                    else:
                        valuechildren = (None,)
                elif self.valuenode!=None:
                    # The node may occur zero or more times.
                    maxoccurs = int(templatechild.getAttribute('maxoccurs'))
                    valuechildren = common.findDescendantNodes(self.valuenode,[childid])
                    assert len(valuechildren)<=maxoccurs, 'Number of children is greater than the imposed maximum (%i).' % maxoccurs

                # Create nodes for all value nodes found.                     
                childloc = list(self.location) + [childid]
                for valuechild in valuechildren:
                    self.children.append(Node(self.controller,templatechild,valuechild,childloc,parent=self))

        # Check for existing value nodes that are not in the template.
        if self.valuenode!=None:
            for ch in [ch for ch in self.valuenode.childNodes if ch.nodeType==ch.ELEMENT_NODE and ch.localName not in ids and not ch.hasAttribute('metadata')]:
                print 'WARNING! Value "%s" below "%s" was unexpected and will be ignored.' % (ch.localName,self.location)
                self.valuenode.removeChild(ch)

    def __str__(self):
        """Returns a string representation of the node.
        """
        return str(self.location)

    def destroy(self):
        """Deallocates all variables of the node, breaking circular
        references.
        """
        for ch in self.children:
            if ch!=None: ch.destroy()
        self.location = ()
        self.children = []
        self.parent = None
        self.templatenode = None
        self.valuenode = None
        self.store = None
        
    def isValid(self):
        """Determines whether the node is valid. Returns False only if
        "destroy" has been called.
        """
        return self.store != None

    def getValue(self,usedefault=False):
        """Returns the typed value of the node. This function returns
        None if the node does not have a value yet, and throws an error
        if the node cannot have a value (i.e., it is a container only).
        """
        value = None
        if self.valuenode!=None:
            valuetype = self.templatenode.getAttribute('type')
            assert valuetype!='', 'getValue was used on node without type (%s); canHaveValue should have showed that this node does not have a type.' % self
            value = self.store.getNodeProperty(self.valuenode,valuetype=valuetype,infonode=self.templatenode)
        if value==None and usedefault: value = self.getDefaultValue()
        return value
        
    def hasValue(self):
        if self.valuenode==None: return False
        value = self.getValue()
        if value==None: return False
        if isinstance(value,common.referencedobject): value.release()
        return True

    def getDefaultValue(self):
        """Returns the default value of the node. This function returns
        None if no default value if available, which applies also if
        a default store has not been specified.
        """
        defaultstore = self.controller.defaultstore
        if defaultstore==None: return None
        defaultnode = defaultstore.mapForeignNode(self)
        if defaultnode==None: return None
        return defaultnode.getValue()
        
    def hasDefaultValue(self):
        value = self.getValue()
        defvalue = self.getDefaultValue()
        hasdef = (value==None or value==defvalue)
        if isinstance(value,   common.referencedobject): value.release()
        if isinstance(defvalue,common.referencedobject): defvalue.release()
        return hasdef

    def setValue(self,value):
        """Sets the typed value of the node. Returns True if the value
        of the node was changed, False if it was not changed. Changes
        may be prevented by an attached interface disallowing the change.
        """
        if value==None:
            self.clearValue()
            return

        curval = self.getValue()
        changed = False
        if curval!=value:
            if self.controller.onBeforeChange(self,value):
                valuetype = self.templatenode.getAttribute('type')
                if self.valuenode==None: self.createValueNode()
                changed = self.store.setNodeProperty(self.valuenode,value,valuetype)
                self.controller.onChange(self,'value')
        if isinstance(curval,common.referencedobject): curval.release()
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
            if deleteclones: self.removeAllChildren()
            for ch in self.children:
                if not ch.clearValue(recursive=True,skipreadonly=skipreadonly,deleteclones=deleteclones):
                    cleared = False
            
        # Do not clear if (1) it is already cleared (result: success), (2) it is
        # read-only and the user wants to respect that (result: failure),
        # or (3) it is the root node (result: failure).
        if self.valuenode==None: return True
        if (skipreadonly and self.isReadOnly()) or self.parent==None: return False
        
        # Clear if (1) this node can have no value - it must occur, and (2) the attached interfaces approve.
        if cleared and (not self.canHaveClones()) and self.controller.onBeforeChange(self,None):
            self.store.clearNodeProperty(self.valuenode)
            self.valuenode = None
            self.controller.onChange(self,'value')
            return True
        else:
            return False

    def getValueAsString(self,addunit = True,usedefault = False):
        """Returns a user-readable string representation of the value of the node.
        
        For built-in data types the conversion to user-readable string is
        automatic; for custom data types their __unicode__ method is called.
        (the default implementation of which calls __str__ in turn)
        """
        templatenode = self.templatenode
        fieldtype = templatenode.getAttribute('type')
        value = self.getValue(usedefault=usedefault)
        if value==None: return ''
        if fieldtype=='datetime':
            value = value.strftime(common.datetime_displayformat)
        if fieldtype=='bool':
            if value:
                value = 'Yes'
            else:
                value = 'No'
        elif fieldtype=='select':
            # Get label of currently selected option
            optionsroot = common.findDescendantNode(templatenode,['options'])
            if optionsroot==None: raise Exception('Variable with "select" type lacks "options" element below.')
            for ch in optionsroot.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option':
                    if value==int(ch.getAttribute('value')):
                        # We found the currently selected option; its label will serve as displayed value.
                        value = ch.getAttribute('label')
                        break
        else:
            strvalue = unicode(value)
            if isinstance(value,common.referencedobject): value.release()
            value = strvalue

        # Append unit specifier (if available)
        if addunit:
            unit = self.getUnit()
            if unit!=None: value = value + ' ' + unit

        return value

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
                assert id==None or child.getSecondaryId()!=id, 'Node with the specified id already exists.'
                index = curindex
                templatenode = child.templatenode
                existingcount += 1
            elif index!=-1:
                # We are at the end of the list of nodes with the specified name. Stop.
                break
                
        # If no insert position was specified, append at the end
        if position==None: position = existingcount
        
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
        self.createValueNode()
        
        # Find the XML document
        doc = self.valuenode
        while doc.parentNode!=None: doc=doc.parentNode
        assert doc.nodeType==doc.DOCUMENT_NODE, 'Could not find DOM document node. Node "%s" does not have a parent.' % doc.tagName

        # Create the value node for the current child
        node = doc.createElementNS(self.valuenode.namespaceURI,childname)
        if id!=None: node.setAttribute('id',id)
        
        # Insert the value node
        if position>=existingcount:
            valuenode = self.valuenode.appendChild(node)
        else:
            valuenode = self.valuenode.insertBefore(node,self.children[index].valuenode)
            
        # Create the child (template + value)
        child = Node(self.controller,templatenode,valuenode,list(self.location)+[childname],parent=self)
        assert child.canHaveClones(), 'Cannot add another child "%s" because there can exist only one child with this name.' % childname
        child.updateVisibility(recursive=True,notify=False)
        
        # Insert the child, and notify attached interfaces.
        child.futureindex = index
        self.controller.beforeVisibilityChange(child,True,False)
        self.children.insert(index,child)
        self.controller.afterVisibilityChange(child,True,False)
        child.futureindex = None
        
        # Return the newly inserted child.
        return child
        
    def createValueNode(self):
        """Creates the (empty) value node, and creates value nodes for
        all ancestors that lacks a value node as well.
        """
        if self.valuenode!=None: return
        parents = []
        root = self
        while root.valuenode==None:
            parents.insert(0,root)
            root = root.parent
        valueroot = root.valuenode
        for par in parents:
            valueroot = common.findDescendantNode(valueroot,[par.getId()],create=True)
        self.valuenode = valueroot

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
                if last!=None and ichildpos>last: return
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
        if pos==None: pos = self.children.index(node)
        node.removeAllChildren(optionalonly=False)
        self.controller.beforeVisibilityChange(node,False,False)
        self.children.pop(pos)
        if node.valuenode!=None:
            self.store.clearNodeProperty(node.valuenode)
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
        assert self.valuenode!=None, 'The value node has not been set; this node cannot be optional.'
        return self.valuenode.getAttribute('id')

    def getValueType(self):
        """Returns the value type of the node; an empty string is returned
        if the node cannot have a value.
        """
        return self.templatenode.getAttribute('type')
        
    def getUnit(self):
        """Returns the unit of the node; None is returned if the node
        does not have a unit specified.
        """
        if not self.templatenode.hasAttribute('unit'): return None
        unit = self.templatenode.getAttribute('unit')
        if unit[0]=='[' and unit[-1]==']':
            node = self.parent[unit[1:-1]]
            if node==None: return None
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
        if ret==None:
            if detail==2 and templatenode.hasAttribute('description'):
                ret = templatenode.getAttribute('description')
            elif detail>=1 and minimumdetail<=1 and templatenode.hasAttribute('label'):
                ret = templatenode.getAttribute('label')
            elif minimumdetail==0:
                ret = self.getId()
        if ret!=None and capitalize: ret = ret[0].upper() + ret[1:]
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
                assert self.parent!=None,'Cannot go up one level because we are at the root.'
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
        while node!=None:
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
        owntype = self.getValueType()
        if isinstance(valuetype,basestring):
            assert not allowderived, 'Cannot looks for nodes with derived classes if original class is specified as string.'
            if owntype==valuetype: res.append(self)
        else:
            owntype = self.store.filetypes.get(owntype,None)
            if allowderived and owntype!=None:
                if issubclass(owntype,valuetype): res.append(self)
            elif owntype==valuetype:
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
            if value==None: res.append(self)
            if isinstance(value,common.referencedobject): value.release()
        for ch in self.children:
            res += ch.getEmptyNodes()
        return res

    def updateVisibility(self,recursive=False,notify=True):
        """Updates the dynamic visibility of the node by re-evaluating
        the conditions imposed by the template on the node's visibility.
        """
        templatenode = self.templatenode
        cond = common.findDescendantNode(templatenode,['condition'])
        if cond!=None:
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
                if isinstance(curval,common.referencedobject): curval.release()

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
            if child==None: continue
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
class TypedStore(common.referencedobject):

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
        if name==None: name = 'default'
        if cls.defaultname2scenarios==None: cls.defaultname2scenarios = {}
        version2store = cls.defaultname2scenarios.setdefault(name,{})
        if version in version2store:
            # We have the requested default with the requested version in our cache; return it.
            return version2store[version]
        elif 'source' not in version2store:
            # We do not have any version of the requested default; first obtain the source version.
            path = cls.getDefaultValues().get(name,None)
            if path==None: return None
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
        if schemapath==None:
            raise Exception('Unable to locate XML schema file for "%s".' % schemaname)
        store = cls(schemapath,*args,**kwargs)
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
        return cls.fromSchemaName(version,valuedom,**kwargs)

    def __init__(self,schema,valueroot=None,otherstores={},adddefault=True):
        
        common.referencedobject.__init__(self)

        if not isinstance(schema,Schema): schema = Schema.create(schema)
        self.schema = schema

        # Get schema version
        self.version = self.schema.getVersion()
        self.originalversion = None

        # Events
        self.interfaces = []

        self.otherstores = otherstores
        for v in self.otherstores.itervalues(): v.addref()

        # Link to original source (if any)
        self.path = None

        # Clear store variables
        self.store = None
        self.defaultstore = None
        self.defaultinterface = None
        self.root = None
        
        # Add store with default values if requested and available.
        if adddefault:
            defscenario = self.getDefault(None,self.version)
            if defscenario!=None: self.setDefaultStore(defscenario,updatevisibility=False)

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
        if self.root!=None: self.root.destroy()
        self.root = None

        # Release container
        self.setContainer(None)
        
        # Release default store
        if self.defaultstore!=None:
            self.defaultstore.disconnectInterface(self.defaultinterface)
            self.defaultinterface = None
            self.defaultstore.release()
            self.defaultstore = None

        # Release any linked objects
        if self.store!=None and 'linkedobjects' in self.store.context:
            for v in self.store.context['linkedobjects'].itervalues(): v.release()
            del self.store.context['linkedobjects']
            
        # Release any linked stores
        for v in self.otherstores.itervalues(): v.release()

        self.store = None
        
        # Release all interfaces
        for i in self.interfaces: i.unlink()
        self.interfaces = []

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
        if 'cache' in self.store.context:
            for v in self.store.context['cache'].itervalues(): v.release()
            del self.store.context['cache']
        if self.store.context.get('container',None)!=None:
            self.store.context['container'].release()
        if container!=None: container.addref()
        self.store.context['container'] = container

    def setStore(self,valueroot):
        """Provides an XML DOM tree with values for the TypedStore. This
        replaces any existing values. The values can be specified as a
        path to an XML file (i.e., a string), an XML document, or an XML
        node. None may be specified instead to clear the store of all values.
        """
        if self.root!=None: self.root.destroy()

        if self.store!=None and 'linkedobjects' in self.store.context:
            for n,v in self.store.context['linkedobjects'].iteritems():
                assert isinstance(v,common.referencedobject), 'Linked file %s is not of type common.referencedobject.' % n
                v.release()
            del self.store.context['linkedobjects']

        templateroot = self.schema.getRoot()

        assert valueroot==None or isinstance(valueroot,basestring) or isinstance(valueroot,xml.dom.Node), 'Supplied value root must None, a path to an XML file, or an XML node, but is %s.' % valueroot

        valuedom = None
        if valueroot==None:
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
            while valuedom.parentNode!=None: valuedom = valuedom.parentNode
            assert valuedom.nodeType==valuedom.DOCUMENT_NODE, 'Could not find DOM document node.'

        storeversion = valueroot.getAttribute('version')
        assert storeversion==self.version, 'Versions of the xml schema ("%s") and and the xml values ("%s") do not match.' % (self.version,storeversion)
                    
        self.store = Store(valuedom,xmlroot=valueroot)
        self.store.filetypes.update({'select'  :int,
                                     'file'    :DataFile,
                                     'duration':StoreTimeDelta,
                                     'color'   :StoreColor,
                                     'fontname':str})
        self.store.filetypes.update(self.getCustomTypes())
        self.root = Node(self,templateroot,self.store.xmlroot,[],None)
        self.changed = False
        self.setContainer(None)
        
        # Update the visibility of all nodes - based on conditions
        # Disable individual notifications because the single "storechanged" event emitted
        # below replaces them)
        self.root.updateVisibility(recursive=True,notify=False)
        
        # Notify attached interface about the store change.
        self.afterStoreChange()
    
    @classmethod
    def getCustomTypes(ownclass):
        return {}

    def setDefaultStore(self,store,updatevisibility=True):
        """Attached a TypedStore object with default values. The attached
        store MUST use the same schema as the store that is attached to.
        """
        assert self.version==store.version,'Version of supplied default store must match version of current store.'
        if self.defaultstore!=None:
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
        for v in self.store.context.get('linkedobjects',{}).itervalues():
            if isinstance(v,TypedStore) and v.hasChanged(): return True
        return False

    def resetChanged(self):
        """Resets the "changed" status of the store to "unchanged".
        See also "hasChanged".
        """
        self.changed = False
        for v in self.store.context.get('linkedobjects',{}).itervalues():
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
        assert currentnode.parent==None, 'Location does not describe complete path to root. Currently at %s.' % currentnode
        
        # Now find the same location in our own store.
        currentnode = self.root
        for (name,index) in zip(foreignnode.location,indices):
            if isinstance(index,int):
                currentnode = currentnode.getChildByNumber(name,index)
            else:
                currentnode = currentnode.getChildById(name,index)
            if currentnode==None: return None
            
        return currentnode
        
    def persist(self,callback=None):
        """Directs all custom nodes to store their custom contents in a container."""
        nodes = [node for node in self.root.getNodesByType(Store.DataType,True) if node.valuenode!=None]
        progslicer = common.ProgressSlicer(callback,len(nodes))
        for node in nodes:
            progslicer.nextStep(node.getText(1))
            self.store.persistNode(node.valuenode,node.getValueType(),node.templatenode)

    def preparePersist(self):
        """Prepares custom nodes for being stored on disk.
        
        This functionality is used by DataFile objects to read all
        data from the source archive before it is overwritten by
        an in-place save.
        """
        nodes = self.root.getNodesByType(Store.DataType,True)
        for node in nodes:
            if node.valuenode!=None:
                self.store.preparePersistNode(node.valuenode,node.getValueType(),node.templatenode)

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
            assert node!=None, 'Cannot locate dependency "%s" for node "%s".' % (nodeCondition.getAttribute('variable'),ownernode)

            # Get the current value of the variable we depend on
            curvalue = node.getValue(usedefault=True)

            # If the node in question currently does not have a value, we cannot check the condition;
            # just return 'valid'.
            if curvalue==None: return True

            # Get the reference value we will compare against
            valuetype = node.getValueType()
            refvalue = self.store.unpackValue(nodeCondition.getAttribute('value'),valuetype)

            # Compare
            if condtype=='eq':
                result = (curvalue==refvalue)
            else:
                result = (curvalue!=refvalue)
                
            if isinstance(curvalue,common.referencedobject): curvalue.release()
            
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
        assert self.defaultstore!=None, 'Cannot fill missing values with defaults because no default store has been specified.'
        if skiphidden:
            for n in self.root.getEmptyNodes():
                if not n.isHidden():
                    defvalue = n.getDefaultValue()
                    n.setValue(defvalue)
                    if isinstance(defvalue,common.referencedobject): defvalue.release()
        else:
            self.root.copyFrom(self.defaultstore.root,replace=False)

    def clearValidationHistory(self,nodes=None):
        if nodes==None:
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
        if nodes==None: nodes = self.root.getDescendants()
            
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
            if value==None:
                emptynodes.append(node)
            elif isinstance(value,Store.DataType):
                customnodes.append(node)
            elif type=='select':
                selectnodes.append(node)
            elif type=='int':
                intnodes.append(node)
            elif type=='float':
                floatnodes.append(node)
            if isinstance(value,common.referencedobject): value.release()
        
        # Find used nodes that have not been set, and lack a default value.
        for node in emptynodes:
            if node.isHidden(): continue
            validity[node] = False
            errors.append('variable "%s" has not been set.' % node.getText(1))

        # Find used file nodes that have not been supplied with data.
        visiblecustomnodes = [node for node in customnodes if not node.isHidden()]
        progslicer = common.ProgressSlicer(callback,len(visiblecustomnodes))
        for node in visiblecustomnodes:
            progslicer.nextStep('validating '+node.getText(detail=1))
            value = node.getValue(usedefault=usedefault)
            if not value.validate(callback=progslicer.getStepCallback()):
                validity[node] = False
                errors.append('variable "%s" is set to an invalid value.' % node.getText(1))
            if isinstance(value,common.referencedobject): value.release()

        # Find nodes of type "select" that have been set to an invalid (non-existing) option.
        for node in selectnodes:
            value = node.getValue(usedefault=usedefault)
            optionsroot = common.findDescendantNode(node.templatenode,['options'])
            assert optionsroot!=None, 'Schema node %s is of type "select", but lacks the "options" child node.' % node
            opt = 0
            for ch in optionsroot.childNodes:
                if ch.nodeType==ch.ELEMENT_NODE and ch.localName=='option' and value==int(ch.getAttribute('value')):
                    opt = 1
                    if not ch.hasAttribute('disabled'): opt = 2
                    break
            if opt!=2:
                if repair==2 or (repair==1 and node.isHidden()):
                    node.setValue(node.getDefaultValue())
                elif opt==1:
                    validity[node] = False
                    errors.append('variable "%s" is set to option "%s", which is currently disabled (perhaps not yet implemented).' % (node.getText(1),ch.getAttribute('label')))
                else:
                    validity[node] = False
                    errors.append('variable "%s" is set to non-existent option %i.' % (node.getText(1),value))

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
        if convertor==None:
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
                if revconv!=None:
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
    def addConvertor(cls,convertorclass,addsimplereverse=False):
        """Registers the specified convertor class. The source and target version that
        the convertor supports are part of the convertor class supplied, and are therefore
        not specified explicitly. The 'addsimplereverse" option will additionally register
        a simple class for back conversion, via addDefaultConvertor."""
        sourceid = convertorclass.fixedsourceid
        targetid = convertorclass.fixedtargetid
        assert sourceid!=None, 'Error! Specified convertor class lacks a source identifier.'
        assert targetid!=None, 'Error! Specified convertor class lacks a target identifier.'
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
        if cls.getConvertor(sourceid,targetid)!=None:
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
                if requireplatform==None or requireplatform==platform:
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
        assert isinstance(container,DataContainer), 'Argument must be data container object.'
        return cls.storefilename in container.listFiles()

    def save(self,path):
        """Saves the values as XML, to the specified path. A file saved in this manner
        might be loaded again through the "load" method."""
        return self.store.save(path)

    def load(self,path):
        """Loads values from an existing XML file. This file may have been saved with the
        "save" method, or it may be taken from a container saved with the "saveAll" method.
        
        If the version of the XML file does not match the version of the store, conversion
        is attempted."""
        if isinstance(path,DataFile):
            f = path.getAsReadOnlyFile()
            valuedom = xml.dom.minidom.parse(f)
            f.close()
        else:
            if not os.path.isfile(path):
                raise Exception('Specified path "%s" does not exist, or is not a file.' % path)
            valuedom = xml.dom.minidom.parse(path)

        version = valuedom.documentElement.getAttribute('version')
        if self.version!=version:
            # The version of the saved store does not match the version of this store object; convert it.
            print 'Value file "%s" has version "%s"; starting conversion to "%s".' % (path,version,self.version)
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
        if targetversion!=None and self.version!=targetversion:
            progslicer = common.ProgressSlicer(callback,3)
        
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
            progslicer = common.ProgressSlicer(callback,2)

            # First: fill nodes that are not set with the default value.
            if fillmissing: self.fillMissingValues()

            # Before opening the target container, allow nodes to prepare for saving to the specified path.
            # Specifically, nodes will read all files that might be overwritten into memory.
            if isinstance(path,basestring):
                self.store.context['targetcontainerpath'] = path
                self.preparePersist()
                del self.store.context['targetcontainerpath']

            # Open target container
            if isinstance(path,basestring):
                if targetisdir:
                    container = DataContainerDirectory(path,create=True)
                else:
                    container = DataContainerZip(path,mode='w')
            elif isinstance(path,StringIO.StringIO):
                container = DataContainerZip(path,mode='w')
                claim = False
            else:
                assert False,'Supplied target must be a path to file or directory, or a StringIO object.'

            # Allow all nodes to add custom data to the target container. This can change the values
            # in the XML store, and must therefore be done before the store is added to the container.
            self.store.context['targetcontainer'] = container
            self.store.context['donotclaimtarget'] = (not claim)
            progslicer.nextStep('adding data streams')
            self.persist(progslicer.getStepCallback())
            del self.store.context['donotclaimtarget']
            
            # Add any other objects that were linked to the store by a node
            # of custom type (e.g. DataFileEx)
            for name,linkedfile in self.store.context.get('linkedobjects',{}).iteritems():
                assert isinstance(linkedfile,TypedStore), 'Do not know how to add linked file %s of type %s to container.' % (name,str(type(linkedfile)))
                df = DataFileXmlNode(linkedfile.store.xmldocument)
                df_added = container.addItem(df,name)
                df_added.release()
                df.release()

            # Add XML store to the container
            progslicer.nextStep('saving configuration')
            df = DataFileXmlNode(self.store.xmldocument)
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
        container typically has been saved through the "saveAll" method."""
        if isinstance(path,basestring):
            container = DataContainer.fromPath(path)
        elif isinstance(path,DataContainer):
            container = path.addref()
        elif isinstance(path,DataFile):
            container = DataContainerZip(path)
        else:
            assert False,'Supplied source must be a path, a data container object or a data file object.'

        # Get list of files in source container.
        files = container.listFiles()

        # Check for existence of main file.
        if self.storefilename not in files:
            raise Exception('The specified source does not contain "%s" and can therefore not be a %s.' % (self.storefilename,self.storetitle))
            
        if callback!=None: callback(0.,'parsing XML')

        # Read and parse the store XML file.
        datafile = container.getItem(self.storefilename)
        f = datafile.getAsReadOnlyFile()
        storedom = xml.dom.minidom.parse(f)
        f.close()
        datafile.release()
        
        # Get the schema version that the store values file matches.
        version = storedom.documentElement.getAttribute('version')
        if self.version!=version:
            # The version of the saved scenario does not match the version of this scenario object; convert it.
            print '%s "%s" has version "%s"; starting conversion to "%s".' % (self.storetitle,path,version,self.version)
            if callback!=None: callback(0.5,'converting scenario')
            tempstore = self.fromSchemaName(version)
            tempstore.loadAll(container)
            if callback==None:
                tempstore.convert(self)
            else:
                tempstore.convert(self,callback=lambda p,s: callback(.5+.5*p,'converting scenario: '+s))
            tempstore.release()
            self.originalversion = version
        else:
            # Attach the parsed scenario (XML DOM).
            self.setStore(storedom)
            self.setContainer(container)

        container.release()

        # Store source path.
        if isinstance(path,basestring):
            self.path = path
        else:
            self.path = None

        if callback!=None: callback(1.,'done')

    def toXml(self,enc='utf-8'):
        """Returns the values as an XML string, with specified encoding."""
        return self.store.xmldocument.toxml(enc)

    def toXmlDom(self,target=None):
        """Obtains a copy of the values as XML DOM tree. Values are appended to a newly
        created XML document, or to the specified target node, if present."""
        return common.copyNode(self.store.xmlroot,target)

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
        changed."""
        # Map node in default store to node in our own store.
        ownnode = self.mapForeignNode(defaultnode)
        if ownnode==None: return

        # Emit change event
        for i in self.interfaces: i.onDefaultChange(ownnode,feature)

        # If the default is being used: update (visibility of) nodes that depend on the changed node.
        if not ownnode.hasValue(): self.updateDependantNodes(ownnode)

    def onChange(self,node,feature):
        """Called internally after a property (e.g., value, unit) of a node has changed.
        The string "feature" specifies which property has changed, e.g., "value", "unit"."""
        # Register that we changed.
        self.changed = True

        # Emit change event
        for i in self.interfaces: i.onChange(node,feature)

        # Update (visibility of) nodes that depend on the changed node.
        self.updateDependantNodes(node)

    def updateDependantNodes(self,node):
        """Called internally after the value of the specified node has changed.
        This method then looks up all nodes that depend on the value of the specified
        node, and emits events if their visibility/unit/... changes in turn."""
        # Get nodes that depend on the changed node; if there are none, exit.
        deps = common.findDescendantNode(node.templatenode,['dependentvariables'])
        if deps==None: return

        # Now build a list of the dependant nodes; currently hidden nodes first, currently visible
        # nodes last, so that when we iterate over the list and switch visibilities first extra nodes
        # will appear, and only later some are removed (this prevents unnecessary automatic scrolling in GUI)
        depnodes = []
        for d in common.findDescendantNodes(deps,['dependentvariable']):
            varpath = d.getAttribute('path')
            
            if varpath[0]!='/':
                refnode = node.parent
            else:
                refnode = self.root
            varnode = refnode[varpath]
            assert varnode!=None, 'Unable to locate node "%s" at %s.' % (varpath,refnode)
            
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
            if not i.onBeforeChange(node,newvalue): return False
        return True

    def afterStoreChange(self):
        """Called internally after the store changes, i.e., all values have changed."""
        for i in self.interfaces: i.afterStoreChange()

    def beforeVisibilityChange(self,node,visible,showhide=True):
        """Called internally before a node is hidden (showhide=True) or deleted (showhide=False)."""
        for i in self.interfaces: i.beforeVisibilityChange(node,visible,showhide)

    def afterVisibilityChange(self,node,visible,showhide=True):
        """Called internally after a node is hidden (showhide=True) or deleted (showhide=False)."""
        for i in self.interfaces: i.afterVisibilityChange(node,visible,showhide)

def versionStringToInt(versionstring):
    """Converts a "major.minor.build" version string to a representative integer."""
    (major,minor,build) = versionstring.split('.')
    return int(major)*256*256 + int(minor)*256 + int(build)

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
        if sourceid==None:
            if self.fixedsourceid==None:
                raise Exception('Convertor class was created without explicit version identifiers, but also lacks default identifiers.')
            sourceid = self.fixedsourceid
            targetid = self.fixedtargetid
        else:
            if self.fixedsourceid!=None:
                raise Exception('Convertor class was created with explicit version identifiers, but also has default identifiers.')
        
        self.sourceid = sourceid
        self.targetid = targetid

        (self.sourcename,self.sourceversion) = sourceid.split('-')
        (self.targetname,self.targetversion) = targetid.split('-')

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
        nodes to ther defaults (this list is also specified by inheriting
        classes)."""
        # Try simple deep copy: nodes with the same name and location in both
        # source and target store will have their value copied.
        target.root.copyFrom(source.root)

        # Handle explicit one-to-one links between source nodes and target nodes.
        for (sourcepath,targetpath) in self.links:
            sourcenode = source[sourcepath]
            if sourcenode==None:
                raise Exception('Cannot locate node "%s" in source.' % '/'.join(sourcepath))
            targetnode = target[targetpath]
            if targetnode==None:
                raise Exception('Cannot locate node "%s" in target.' % '/'.join(targetpath))
            targetnode.copyFrom(sourcenode)

        # Reset target nodes to defaults where that was explicitly specified.
        if len(self.defaults)>0:
            defscen = target.getDefault(None,target.version)
            for path in self.defaults:
                sourcenode = defscen[path]
                if sourcenode==None:
                    raise Exception('Cannot locate node "%s" in default.')
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
        if callback!=None:
            stepcallback = lambda p,s: callback((istep+p)/nsteps,s)
        else:
            stepcallback = None
        for istep in range(nsteps-1):
            convertor = self.chain[istep]
            temptargetid = convertor.targetid
            if callback!=None: callback(float(istep)/nsteps,'converting to version "%s".' % temptargetid)
            print 'Converting to temporary target "%s".' % temptargetid
            temptarget = source.fromSchemaName(temptargetid)
            temptargets.append(temptarget)
            convertor.convert(source,temptarget,callback=stepcallback)
            source = temptarget
        convertor = self.chain[-1]
        istep = nsteps-1
        if callback!=None: callback(float(istep)/nsteps,'converting to version "%s".' % target.version)
        print 'Converting to final target "%s".' % target.version
        convertor.convert(source,target,callback=stepcallback)
        for temptarget in temptargets: temptarget.release()

