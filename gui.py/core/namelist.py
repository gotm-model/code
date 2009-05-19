import re

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

class NamelistSubstitutions(object):
    subs_re = None

    def __init__(self,valuesfile):
        self.subs = []
        
        # Commonly used regular expression (for parsing substitions in the .values file)
        if NamelistSubstitutions.subs_re is None:
            NamelistSubstitutions.subs_re = re.compile('\s*s/(\w+)/(.+)/')
        
        # If the supplied argument is a string, it should be a path to a file.
        # Try to open it. Otherwise the supplied argument should be a file-like object.
        ownfile = False
        if isinstance(valuesfile,basestring):
            try:
                valuesfile = open(valuesfile,'rU')
            except Exception,e:
                raise NamelistParseException('Cannot open .values file "%s". Error: %s' % (path,str(e)))
            ownfile = True
            
        line = valuesfile.readline()
        while line!='':
            m = self.subs_re.match(line)
            if m is not None:
                #pat = re.compile(m.group(1),re.IGNORECASE)
                #self.subs.append((pat,m.group(2)))
                self.subs.append((m.group(1),m.group(2)))
            line = valuesfile.readline()
            
        # If we opened the file ourselves, close it.
        if ownfile:
            valuesfile.close()

    def substitute(self,text):
        for (old,new) in self.subs:
            #text = old.sub(new,text)
            text = text.replace(old,new)
        return text

class Namelist(object):

    varassign_re   = None
    varstopchar_re = None

    def __init__(self,name,data,filepath=None):
        self.name = name
        self.data = data
        self.filepath = filepath
        
        # Identify the different items in the namelist
        ipos = 0
        items = []
        while ipos<len(self.data):
            ch = self.data[ipos]
            if ch.isspace():
                ipos += 1
            elif ch in '"\'(':
                closer = ch
                if ch=='(': closer = ')'
                iend = self.data.find(ch,ipos+1)
                if iend==-1: raise Exception('Opening %s is not matched by closing %s.' % (ch,closer))
                items.append(self.data[ipos:iend+1])
                ipos = iend+1
            elif ch in '*=,':
                items.append(ch)
                ipos += 1
            elif ch.isalnum() or ch in '._-+':
                iend = ipos+1
                while iend<len(self.data) and (self.data[iend].isalnum() or self.data[iend] in '._-+'): iend+=1
                items.append(self.data[ipos:iend])
                ipos = iend
            elif ch=='/':
                break
            else:
                print self.data
                raise Exception('Unknown character %s found in namelist.' % ch)
                ipos += 1
                
        i = 0
        self.assignments = []
        while items:
            varname = items.pop(0)
            equals = items.pop(0)
            if equals!='=': raise Exception('Equals sign (=) excepted after variable name %s.' % varname)
            values,valuerequired = [],True
            while items and (len(items)==1 or items[1]!='='):
                value = items.pop(0)
                if value==',':
                    # Field separator - ignore if a value preceded it, otherwise interpret it as null value.
                    if valuerequired:
                        values.append(None)
                    else:
                        valuerequired = True
                elif value.isdigit() and len(items)>=2 and items[0]=='*':
                    # Multiplication: n*value
                    items.pop(0)
                    values += [items.pop(0)]*int(value)
                    valuerequired = False
                else:
                    # Normal value
                    values.append(value)
                    valuerequired = False
            if len(values)==1: values = values[0]
            self.assignments.append((varname,values))

    def __iter__(self):
        return self

    def next(self):
        ret = self.getNextVariable()
        if ret is None: raise StopIteration
        return ret

    def getNextVariable(self):
        if not self.assignments: return None
        return self.assignments.pop(0)

    def isEmpty(self):
        return not self.assignments

class NamelistFile(object):
    commentchar_re  = None
    namelistname_re = None
    stopchar_re     = None

    def __init__(self,nmlfile,subs=[]):
        # Commonly used regular expressions, for:
        #   - locating the start of comments, or opening quotes.
        #   - locating the start of namelist (ampersand followed by list name).
        #   - locating the end of a namelist, i.e. a slash, or opening quotes.
        if NamelistFile.commentchar_re is None:
            NamelistFile.commentchar_re  = re.compile('[!#"\']')
            NamelistFile.namelistname_re = re.compile('\s*&\s*(\w+)\s*')
            NamelistFile.stopchar_re     = re.compile('[/"\']')
    
        ownfile = False
        if isinstance(nmlfile,basestring):
            ownfile = True
            
            # Attempt to open namelist file and read all data
            try:
                nmlfile = open(nmlfile,'rU')
            except Exception,e:
                raise NamelistParseException('Cannot open namelist file. Error: %s' % (str(e),),path)
            self.path = nmlfile
        else:
            self.path = ''

        try:
            
            self.data = ''
            line = nmlfile.readline()
            iline = 1
            while line!='':

                # Strip comments, i.e. on every line, remove everything after (and including) the first exclamation
                # mark; ignore text between single and double quotes.
                ipos = 0
                match = self.commentchar_re.search(line,pos=ipos)
                while match is not None:
                    ch = match.group(0)
                    if ch=='\'' or ch=='"':
                        ipos = match.end(0)
                        inextquote = line.find(ch,ipos)
                        if inextquote==-1:
                            raise NamelistParseException('Line %i: opening quote %s was not matched by end quote.' % (iline,ch),path)
                        ipos = inextquote+1
                    else:
                        # Found start of comment; only keep everything preceding the start position.
                        line = line[:match.start(0)]+'\n'
                        break
                    match = self.commentchar_re.search(line,pos=ipos)

                self.data += line
                line = nmlfile.readline()
                iline += 1
        finally:
            if ownfile: nmlfile.close()

        # Make substitutions (if any).
        for sub in subs:
            self.data = sub.substitute(self.data)

    def parseNextNamelist(self,expectedlist=None):
        match = self.namelistname_re.match(self.data)
        if match is None:
            raise NamelistParseException('No namelist found; expected ampersand followed by namelist name.',self.path)
        name = match.group(1)
        if expectedlist is not None and name!=expectedlist:
            raise NamelistParseException('Expected namelist "%s", but found "%s".' % (expectedlist,name),self.path,expectedlist)
        istart = match.end(0)
        ipos = istart
        while True:
            match = self.stopchar_re.search(self.data,pos=ipos)
            if match is None:
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
