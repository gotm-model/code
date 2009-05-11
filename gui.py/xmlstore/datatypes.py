import datetime,re,os,os.path,zipfile,tarfile,shutil,StringIO

import util

types = {}

def register(name,datatype):
    assert name not in types, 'Data type %s has already been registered.' % datatype
    types[name] = datatype


class DataType:
    """Abstract class for user data types. Derived classes must implement
    virtual methods "load" and "save".
    
    The "context" is a dictionary object that allows for data-type-specific
    storage on store level. For instance, it is currently used by DataFile
    objects as source of additional information (i.e., which data store to
    obtain external data files from), and for caching of values.
    """
    def __init__(self):
        pass
    
    @classmethod
    def load(ownclass,node,context,template=None):
        assert False, 'Method "load" MUST be implemented by the inheriting class.'

    def save(self,node,context):
        assert False, 'Method "save" MUST be implemented by the inheriting class.'

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
        
    def toPrettyString(self):
        """Returns a pretty string representation of the object."""
        return unicode(self)

class DataTypeSimple(DataType):
    """Data type that can be completely represented by a single string.
    This string will be used to store the value as a single text node in XML.
    Also, because a single string representation exists, it can be used in
    conditions and as the base data type of a "select" element.
    
    Derived classes must implement fromXmlString and toXmlString, rather
    than load and save.
    """

    @classmethod
    def load(ownclass,node,context,template=None):
        return ownclass.fromXmlString(util.getNodeText(node),context,template)

    @staticmethod
    def fromXmlString(string,context,template=None):
        """Loads the object from an XML string.
        
        The object returned by this static method will normally be a member of the class
        to which the static method belongs. However, this is not enforced: an object of
        another class may be returned (e.g., a Python primitive object such as int, float,
        str). This will work provided that the class can be initialized from this object
        alone. For an eample of this approach see the primitive data types Int, Float, String
        below.
        """
        assert False, 'Method "fromXmlString" MUST be implemented by the inheriting class.'

    def save(self,node,context):
        string = self.toXmlString(context)
        if util.getNodeText(node)==string: return False
        if string is not None:
            util.setNodeText(node,string)
        else:
            util.removeNodeChildren(node)
        return True

    def toXmlString(self,context):
        assert False, 'Method "toXmlString" MUST be implemented by the inheriting class.'

class DataTypePrimitive(DataTypeSimple):
    """Primitive data type that can be represented by a single Python object
    (the value), as well as a single string. The value must be specified upon
    initialization.
    
    Default conversion to an XML string is provided by a simple method that
    calls "unicode" with the Python object as argument.
    """
    def __init__(self,value):
        self.value = value
    def toXmlString(self,context):
        return unicode(self.value)
    def toPrettyString(self):
        return unicode(self.value)
    def __cmp__(self,other):
        if isinstance(other,DataTypePrimitive):
            othervalue = other.value
        else:
            othervalue = other
        return cmp(self.value,othervalue)

class Int(DataTypePrimitive):
    """Integer data type.
    """
    def __init__(self,value):
        DataTypePrimitive.__init__(self,int(value))
    @staticmethod
    def fromXmlString(string,context,template=None):
        return int(string)
register('int',Int)

class Float(DataTypePrimitive):
    """Floating point data type.
    """
    def __init__(self,value):
        DataTypePrimitive.__init__(self,float(value))
    @staticmethod
    def fromXmlString(string,context,template=None):
        return float(string)
register('float',Float)

class Bool(DataTypePrimitive):
    """Boolean data type.
    """
    def __init__(self,value):
        DataTypePrimitive.__init__(self,bool(value))
    @staticmethod
    def fromXmlString(string,context,template=None):
        return (string=='True')
    def toXmlString(self,context):
        if self.value: return 'True'
        else:          return 'False'
    def toPrettyString(self):
        if self.value: return 'Yes'
        else:          return 'No'
register('bool',Bool)

class String(DataTypePrimitive):
    """String data type.
    """
    def __init__(self,value):
        DataTypePrimitive.__init__(self,unicode(value))
    @staticmethod
    def fromXmlString(string,context,template=None):
        return string
register('string',String)

class DateTime(DataTypePrimitive):
    """Date + time data type.
    """
    reDateTime = re.compile('(\d{4})-(\d\d)-(\d\d) (\d\d):(\d\d):(\d\d)')
    def __init__(self,value):
        assert isinstance(value,datetime.datetime),'Value is not of type datetime.datetime.'
        DataTypePrimitive.__init__(self,value)
    @staticmethod
    def fromXmlString(string,context,template=None):
        match = DateTime.reDateTime.match(string)
        if match is None: raise Exception('Cannot parse "%s" as datetime object.' % string)
        c = map(int,match.groups())
        return datetime.datetime(c[0],c[1],c[2],c[3],c[4],c[5],tzinfo=util.utc)
    def toXmlString(self,context):
        dt = self.value
        return '%04i-%02i-%02i %02i:%02i:%02i' % (dt.year,dt.month,dt.day,dt.hour,dt.minute,dt.second)
    def toPrettyString(self):
        return util.formatDateTime(self.value)
register('datetime',DateTime)

class TimeDelta(DataTypeSimple,datetime.timedelta):
    """Class representing a time span, capable of being stored in/loaded from
    an XML store. Time spans are stored using the XSD duration data type format.
    """
    
    def __init__(self,*args,**kwargs):
        DataTypeSimple.__init__(self)
        datetime.timedelta.__init__(self,*args,**kwargs)

    @staticmethod
    def fromXmlString(text,context,template):
        """Loads the time span value from the specified string.
        """
        if text:
            # Handle negative timedeltas: process optional leading minus sign
            mult = 1
            if text[0]=='-':
                mult = -1
                text = text[1:]
                
            assert text[0]=='P', 'A stored duration/timedelta should always start with "P".'
            import re
            m = re.match('P(\d+Y)?(\d+M)?(\d+D)?(?:T(\d+H)?(\d+M)?(\d+(?:\.\d*)S)?)?',text)
            assert m is not None, 'The string "%s" is not a valid duration/timedelta.' % text
            grps = m.groups('0x')
            y,m,d,hh,mm = [int(t[:-1]) for t in grps[:-1]]
            ss = float(grps[-1][:-1])
            return TimeDelta(days=mult*(y*365+m*30+d),seconds=mult*(hh*3600+mm*60+ss))
        else:
            return TimeDelta()

    def toXmlString(self,context):
        """Returns a string representation of the time span value, suitable for saving to XML.
        This string can be parsed through the static "fromXmlString" method.
        """
        return 'P%iDT%fS' % (self.days,self.seconds+self.microseconds/1000000.)
        
    def getAsSeconds(self):
        """Gets the number of seconds in the time span as floating point value.
        """
        return self.days*86400 + self.seconds + self.microseconds/1000000.
        
    def __float__(self):
        """Returns the number of seconds in the time span as floating point value.
        """
        return float(self.getAsSeconds())
        
    def __cmp__(self,other):
        return cmp(self.getAsSeconds(),other.getAsSeconds())
        
    def toPrettyString(self):
        """Returns a "pretty" string representation of the time span.
        """
        values = []
        
        # Add days
        if self.days>0:
            values.append([self.days,'day'])
            
        # Divide seconds over hours, minutes, and remaining seconds.
        hours,seconds = divmod(self.seconds,3600)
        if hours>0: values.append([hours,'hour'])
        minutes,seconds = divmod(seconds,60)
        if minutes>0: values.append([minutes,'minute'])
        if seconds>0: values.append([seconds,'second'])
            
        # Add microseconds
        if self.microseconds>0:
            values.append([self.microseconds,'microsecond'])
            
        # Add a trailing "s" for each unit that will have value > 1
        for v in values:
            if v[0]>1: v[1]+='s'
            
        # Combine units into string.
        return ', '.join(['%i %s' % (v[0],v[1]) for v in values])
register('duration',TimeDelta)

class Color(DataTypeSimple):
    """Class representing a color (RGB), capable of being stored in/loaded from
    an XML store.
    """

    def __init__(self,red=None,green=None,blue=None):
        DataTypeSimple.__init__(self)
        self.red = red
        self.green = green
        self.blue = blue

    @staticmethod
    def fromXmlString(strcolor,context,template):
        """Creates a Color object with its value read from the specified string.
        """
        if len(strcolor)>0:
            assert len(strcolor)==7 and strcolor[0]=='#', 'Colors must have exactly 7 characters and start with #.'
            strcolor = strcolor[1:]
            return Color(int(strcolor[0:2],16),int(strcolor[2:4],16),int(strcolor[4:6],16))
        else:
            return Color()
            
    @staticmethod
    def fromNormalized(r,g,b):
        """Creates a Color object from normalized color values (between 0 and 1, as used by MatPlotLib).
        """
        return Color(int(r*255),int(g*255),int(b*255))
        
    def brighten(self,value):
        """Brightens the color with the specified value (between 0 and 1).
        """
        assert value>=0 and value<=1, 'Brighten value must be between 0 and 1.'
        self.red   = int(self.red  +(255.-self.red)  *value)
        self.green = int(self.green+(255.-self.green)*value)
        self.blue  = int(self.blue +(255.-self.blue) *value)
        
    def copy(self):
        """Returns a copy of the Color object.
        """
        return Color(self.red,self.green,self.blue)

    def toXmlString(self,context):
        """Returns a string representation of the value of the Color object,
        suitable from saving to XML.
        This string can be parsed through the static "fromXmlString" method.
        """
        assert self.red   is None or self.red  <=255, 'Color channel red values should not exceed 255.'
        assert self.green is None or self.green<=255, 'Color channel green values should not exceed 255.'
        assert self.blue  is None or self.blue <=255, 'Color channel blue values should not exceed 255.'
        if self.isValid():
            return '#%02x%02x%02x' % (self.red,self.green,self.blue)
        else:
            return ''
        
    def isValid(self):
        """Returns whether the object currently contains a valid color.
        """
        return self.red is not None and self.green is not None and self.blue is not None
        
    def getNormalized(self,nocolor='none'):
        """Returns a tuple with normalized RGB color values (between 0 and 1).
        """
        if not self.isValid(): return nocolor
        return (self.red/255.,self.green/255.,self.blue/255.)
        
    def toPrettyString(self):
        """Returns a pretty string representation of the color.
        """
        if self.isValid():
            return '(%i, %i, %i)' % (self.red,self.green,self.blue)
        else:
            return 'none'
            
    def __eq__(self,other):
        if not isinstance(other,Color): return False
        return self.red==other.red and self.green==other.green and self.blue==other.blue
        
    def __ne__(self,other):
        return not self.__eq__(other)
register('color',Color)

class DataContainer(util.referencedobject):
    """Abstract data container, e.g., a directory or compressed archive.
    Items in the container are identified by name."""
    def __init__(self):
        util.referencedobject.__init__(self)
        self.path = None
        
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

class DataFile(DataType,util.referencedobject):
    """Abstract Class that encapsulates a data block, which can be a file
    on disk, an item in a zip or tar/gz archive, or a memory block. It can be
    used as data type in the xml stores.
    """
    def __init__(self,name=''):
        util.referencedobject.__init__(self)
        DataType.__init__(self)
        self.name = None
    
    @classmethod
    def load(ownclass,node,context,template):
        """Loads the data file from the specified XML node. Currently the XML node
        should contain the name of the data object within its container, and the
        container (DataContainer instance) should be specified through the context
        dictionary.
        """
        assert 'container' in context, 'container key not present in context dictionary.'
        cache = context.setdefault('cache',{})
        uniquename = DataFile.getUniqueNodeName(node)
        if uniquename in cache: return cache[uniquename].addref()
        container = context['container']
        if container is None: return DataFile()
        name = util.getNodeText(node)
        if name not in container.listFiles(): return DataFile()
        df = container.getItem(name)
        cache[uniquename] = df.addref()
        return df

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
        self.addref()
        if uniquename in cache: cache[uniquename].release()
        cache[uniquename] = self
        if self.name is not None:
            util.setNodeText(node,self.name)
        else:
            util.setNodeText(node,'')

    def preparePersist(self,node,context):
        """Prepares the data file object for being saved to persistent storage.
        Currently, this method checks whether the upcoming save will overwrite
        the current data file. If so, the data file is first read into memory.
        """
        assert 'targetcontainerpath' in context, 'preparePersist: "targetcontainerpath" not set in XML store context.'
        if not self.isValid(): return
        targetpath = context['targetcontainerpath']
        if self.isBelowPath(targetpath):
            #print 'Reading "%s" into memory to prevent it from being overwritten.' % self.name
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
            util.setNodeText(node,df.name)
        else:
            df.save(node,context)
        df.release()

    @staticmethod
    def getUniqueNodeName(node):
        """Returns a unique name for the specified node, usable as index into
        a container-wide cache.
        """
        path = []
        while node.parentNode.parentNode is not None:
            path.append(node.localName)
            node = node.parentNode
        return '/'.join(path)

    def isValid(self):
        """Returns True if the data file object is valid (has data associated
        with it; False if not.
        """
        return self.name is not None

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

class DataFileEx(DataType,util.referencedobject):
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
        util.referencedobject.__init__(self)
        DataType.__init__(self)

        assert nodename is not None or context is None, 'If a context is provided you must also specify a node name. That name will be used to uniquely identify the node in the context.'

        self.nodename = nodename
        
        # Create a global store for metadata if it does not exist yet.
        if context is None: context = {'fake':True}
        linkedfiles = context.setdefault('linkedobjects',{})
        if self.linkedfilename not in linkedfiles:
            # Store for metadata does not exist yet: create it.
            store = self.createTypedStore()
            assert store is not None, 'No typed store returned by createTypedStore.'
            linkedfiles[self.linkedfilename] = store
            
            # If the source container already contains metadata, load them.
            container = context.get('container',None)
            if container is not None:
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
        if self.datafile is not None:
            self.datafile.release()
            self.datafile = None
            
        # Set new data file (if any)
        if datafile is not None:
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
        if self.metadata is not None: return self.metadata
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
            assert newstore is not None, 'No typed store returned by createTypedStore.'
            linkedfiles[self.linkedfilename] = newstore
        else:
            newstore = linkedfiles[self.linkedfilename]
            
        # Copy old metadata to the target store, and connect to the target
        # store for future reference.
        if newstore is not self.store:
            oldmetadata = self.getMetaData(create=False)
            if oldmetadata is not None:
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
        if self.datafile is None or not self.datafile.isValid():
            return ''
        else:
            return self.datafile.name
        
    def unlink(self):
        self.metadata = None
        if self.datafile is not None: self.datafile.release()
        if self.store    is not None: self.store.release()

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
            if self.file is None: return
            self.file.close()
            self.file = None
   
    def __init__(self,path,create=False):
        DataContainer.__init__(self)
        assert os.path.isdir(path) or create, 'Supplied path is not an existing directory and we are not allowed to create a new directory.'
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
        if newname is None: newname = datafile.name
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
            assert self.zipcontainer is not None, 'DataFileZip.getData failed; ZIP file has been closed.'
            self.zipcontainer.setMode('r')
            return self.zipcontainer.zfile.read(self.name)

        def isBelowPath(self,path):
            """Returns True if the data file is located somewhere below the specified
            path.
            
            This is used in DataFile.preparePersist to check whether the data file
            would be overwritten if the specified path is saved to.
            """
            assert self.zipcontainer is not None, 'DataFileZip.isBelowPath failed; ZIP file has been closed.'
            if self.zipcontainer.path is None: return False
            owndir = os.path.normcase(self.zipcontainer.path)
            return owndir.startswith(os.path.normcase(path))

        def getSize(self):
            """Returns the size (in number of bytes) of the data file.
            """
            return self.zipcontainer.zfile.getinfo(self.name).file_size

        def unlink(self):
            """Destroys the data file object. This closes open streams etc."""
            if self.zipcontainer is None: return
            self.zipcontainer.release()
            self.zipcontainer = None
    
    def __init__(self,source,mode='r'):
        DataContainer.__init__(self)
        if isinstance(source,basestring):
            assert os.path.isfile(source) or mode=='w', 'Cannot initialize DataContainerZip with supplied path; it does not point to an existing file, but is also not opened for writing.'
        elif isinstance(source,StringIO.StringIO):
            assert mode=='w', 'Can initialize DataContainerZip with StringIO object only in write-only mode.'
        elif isinstance(source,DataFile):
            assert mode=='r', 'Can initialize DataContainerZip with file-like object only in read-only mode.'
        else:
            assert False, 'Cannot initialize DataContainerZip with %s.' % source
        self.mode = None
        self.zfile = None
        self.source = source
        if isinstance(self.source,DataFile):
            self.source.addref()
        elif isinstance(self.source,basestring):
            self.path = self.source
        self.setMode(mode)

    def __del__(self):
        self.unlink()

    def unlink(self):
        """Destroys the data container object. This closes open streams,
        closes open files, cleans up, etc.
        """
        if self.zfile is None: return
        self.zfile.close()
        self.zfile = None
        self.mode = None
        if isinstance(self.source,DataFile): self.source.release()
        self.source = None
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
        if newname is None: newname = datafile.name
        if self.mode=='r': self.setMode('a')
        #if isinstance(self.source,StringIO.StringIO):
        #    print 'Adding "%s" to in-memory archive...' % (newname,)
        #else:
        #    print 'Adding "%s" to archive "%s"...' % (newname,self.path)
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
        if self.zfile is None: return
        if isinstance(self.source,basestring):
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
        if self.zfile is not None:
            if mode==self.mode: return
            self.zfile.close()
        self.mode = mode
        if isinstance(self.source,StringIO.StringIO):
            # Writing to in-memory data block.
            assert self.mode=='w', 'In-memory data blocks can only be written to, not read from.'
            self.zfile = zipfile.ZipFile(self.source,self.mode,zipfile.ZIP_DEFLATED)
        if isinstance(self.source,DataFile):
            # Reading from generic DataFile object.
            assert self.mode=='r', 'Data file objects can only be accessed as read-only zip file.'
            f = self.source.getAsReadOnlyFile()
            self.zfile = zipfile.ZipFile(f,self.mode,zipfile.ZIP_DEFLATED)
        else:
            # Reading from/writing to file.
            self.zfile = zipfile.ZipFile(self.source,self.mode,zipfile.ZIP_DEFLATED)

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
            assert self.tarcontainer is not None, 'DataFileTar.getAsReadOnlyFile failed; TAR file has been closed.'
            self.tarcontainer.setMode('r')
            return self.tarcontainer.tfile.extractfile(self.name)

        def isBelowPath(self,path):
            """Returns True if the data file is located somewhere below the specified
            path.
            
            This is used in DataFile.preparePersist to check whether the data file
            would be overwritten if the specified path is saved to.
            """
            assert self.tarcontainer is not None, 'DataFileTar.isBelowPath failed; TAR file has been closed.'
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
        assert isinstance(path,basestring), 'DataContainerTar must be initialized with a path to a exisitng/to-be-created tar/gz file.'
        assert os.path.isfile(path) or mode=='w', 'The path supplied to DataContainerTar does exist, and can therefore not be opened for reading.'
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
        if self.tfile is None: return
        self.tfile.close()
        self.mode = None
        self.tfile = None
        self.path = None
    
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
        if newname is None: newname = datafile.name
        if self.mode=='r': self.setMode('a')
        #print 'Adding "%s" to archive "%s"...' % (newname,self.path)
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
        if self.tfile is None: return
        self.setMode('r')

    def setMode(self,mode):
        """Switches the mode (read, write) in which the tar archive
        is accessed.
        """
        assert mode in ('r','w'), 'DataContainerZip.setMode: mode must be "r" or "w" (not "%s").' % mode
        if mode=='w': mode='w:gz'
        if self.tfile is not None:
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