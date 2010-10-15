# User-configurable settings
sourcedir = '../dist'
exe = 'gotmgui.exe'

# Import modules
import os,re,sys,codecs
import pythoncom    # Needed for GUID generation

# Check for version number on command line
if len(sys.argv)!=2:
    print 'buildmsi.py takes one required argument: the version number of the application (x.x.x).'
    sys.exit(2)

# Additional internal settings
version = sys.argv[1]
output = 'files.wxs'
indent = '  '

# Find WiX utilities
if 'WIX' not in os.environ:
    print 'Cannot find environment variable "WIX". Is WIX (http://wix.sourceforge.net) installed?'
    sys.exit(1)
path_candle = os.path.join(os.environ['WIX'],'bin','candle.exe')
path_light  = os.path.join(os.environ['WIX'],'bin','light.exe')
assert os.path.isfile(path_candle),'Cannot find WiX utility candle.exe at "%s".' % path_candle
assert os.path.isfile(path_light ),'Cannot find WiX utility light.exe at "%s".' % path_light

# Get full path to executable
exe = os.path.normpath(os.path.join(sourcedir,exe))

# Function for enumerating items in the specified directory
# To be called recursively.
compids = []
def enumfiles(f,dir):
    for name in os.listdir(dir):
        fullpath = os.path.normpath(os.path.join(dir,name))
        fileid = fullpath
        if fileid.startswith('..\\'): fileid = fileid[3:]
        fileid = re.sub('\W','_',fileid)
        if os.path.isfile(fullpath):
            guid = str(pythoncom.CreateGuid())[1:-1]
            f.writeline('<Component Id="%s" Guid="%s">' % (fileid,guid),addindent=1)
            f.writeline('<File Id="%s" Name="%s" KeyPath="yes"/>' % (fileid,os.path.basename(fullpath)))
            if fullpath==exe: f.writeline('<?include exeinfo.wxi ?>')
            f.writeline('</Component>',addindent=-1)
            compids.append(fileid)
        elif os.path.isdir(fullpath):
            f.writeline('<Directory Id="%s" Name="%s">' % (fileid,os.path.basename(fullpath)),addindent=1)
            enumfiles(f,fullpath)
            f.writeline('</Directory>',addindent=-1)

# Class represing WXS include files.
# Automaticvally adds begin and end tag for Wix and Fragment, and handles indentation.
class WxsInclude:
    def __init__(self,path):
        self.f = codecs.open(path,'w','utf-8')
        self.indent = 0
        self.writeline('<?xml version="1.0" encoding="utf-8"?>')
        self.writeline('<Wix xmlns="http://schemas.microsoft.com/wix/2006/wi">',addindent=1)
        self.writeline('<Fragment>',addindent=1)
        
    def close(self):
        self.writeline('</Fragment>',addindent=-1)
        self.writeline('</Wix>',addindent=-1)
        self.f.close()
        
    def writeline(self,string,addindent=0):
        if addindent<0: self.indent += addindent
        self.f.write(self.indent*indent+string+'\n')
        if addindent>0: self.indent += addindent

f_files = WxsInclude(output)

f_files.writeline('<DirectoryRef Id="TARGETDIR">',addindent=1)
enumfiles(f_files,sourcedir)
f_files.writeline('</DirectoryRef>',addindent=-1)

f_files.writeline('<ComponentGroup Id="GOTMComponents">',addindent=1)
for compid in compids:
    f_files.writeline(indent+'<ComponentRef Id="%s" />' % compid)
f_files.writeline('</ComponentGroup>',addindent=-1)

f_files.close()

import subprocess

ret = subprocess.call((path_candle,'gotmgui.wxs','files.wxs','vcredist.wxs','-dVersion=%s' % version))
if ret!=0:
    print 'CANDLE failed with return code %i: exiting.' % ret
    sys.exit(1)

ret = subprocess.call((path_light,'gotmgui.wixobj','files.wixobj','vcredist.wixobj','-b','../dist','-o','gotmgui-%s.msi' % version))
if ret!=0:
    print 'LIGHT failed with return code %i: exiting.' % ret
    sys.exit(1)
