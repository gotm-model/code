# We need 'common' to create temporary scenario's during multiple-step conversions.
# This creates a circular dependency, but Python does not seem to mind.
import common

# This dictionary will hold (sourcename -> sourceconversions) entries.
# Use: convertorsfrom[sourcename][targetname] return a class that can convert sourcename into targetname.
convertorsfrom = {}

# versionStringToInt(versionstring)
#   Converts a "major.minor.build" version string to a representative integer.
def versionStringToInt(versionstring):
    (major,minor,build) = versionstring.split('.')
    return int(major)*256*256 + int(minor)*256 + int(build)

# hasConvertor(sourceid,targetid)
#   Checks if a conversion route between the specified versions is available.
#   Both direct and indirect (via another scenario version) routes are ok.
def hasConvertor(sourceid,targetid):
    # Try direct conversion
    if getConvertor(sourceid,targetid)!=None:
        return True

    print 'Searching for indirect conversion routes from '+sourceid+' to '+targetid+'.'
    indirectroutes = findIndirectConversion(sourceid,targetid,depth='  ')
    for indirect in indirectroutes:
        print indirect
    print 'Found '+str(len(indirectroutes))+' indirect routes.'

    return len(indirectroutes)>0

# getConvertor(sourceid,targetid,directonly=False)
#   Returns a convertor object, capable of converting between the specified versions.
#   Use directonly=True to retrieve only direct conversion routes.
#   Return None if no convertor is available that meets the specified criteria.
def getConvertor(sourceid,targetid,directonly=False):
    # Try direct route first.
    if (sourceid in convertorsfrom) and (targetid in convertorsfrom[sourceid]):
        convertorclass = convertorsfrom[sourceid][targetid]
        return convertorclass()

    # Direct route not available, try indirect routes
    if not directonly:
        indirectroutes = findIndirectConversion(sourceid,targetid,depth='  ')
        if len(indirectroutes)>0:
            indirectroutes.sort(key=len)
            return ConvertorChain(indirectroutes[0])

    # No route available.
    return None

# rankSources(targetid,sourceids,requireplatform=None)
#   Rank a set of supplied versions/identifiers according to platform (i.e. gotmgui, gotm) and version
#   Rank criterion is 'closeness' (in version and platform) to the reference targetid.
def rankSources(targetid,sourceids,requireplatform=None):
    (targetplatform,targetversion) = targetid.split('-')
    targetversion = versionStringToInt(targetversion)

    # Decompose source ids into name and (integer) version, but only take source we can actually convert to the target version.
    sourceinfo = []
    for sid in sourceids:
        if sid==targetid or hasConvertor(sid,targetid):
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

# findIndirectConversion(sourceid,targetid)
#   [internal use only!]
#   Return conversion routes between sourceid and targetid, avoiding versions in disallowed.
#   The depth argument is used for debug output only.
def findIndirectConversion(sourceid,targetid,disallowed=[],depth=''):
    next = convertorsfrom.get(sourceid,{}).keys()
    routes = []
    curdisallowed = disallowed[:]+[sourceid]
    for curnext in next:
        if curnext in curdisallowed: continue
        if curnext==targetid:
            routes.append([sourceid,curnext])
        else:
            childroutes = findIndirectConversion(curnext,targetid,curdisallowed,depth=depth+'  ')
            for cr in childroutes:
                routes.append([sourceid]+cr)
    return routes

# addConvertor(convertorclass)
#   [internal use only!]
#   Register a convertor class.
def addConvertor(convertorclass):
    sourceid = convertorclass.fixedsourceid
    targetid = convertorclass.fixedtargetid
    if sourceid==None:
        raise Exception('Error! Specified convertor class lacks a source identifier.')
    if targetid==None:
        raise Exception('Error! Specified convertor class lacks a target identifier.')
    if sourceid not in convertorsfrom: convertorsfrom[sourceid] = {}
    if targetid in convertorsfrom[sourceid]:
        raise Exception('Error! A class for converting from "%s" to "%s" was already specified previously.' % (sourceid,targetid))
    convertorsfrom[sourceid][targetid] = convertorclass

# Convertor
#   Base class for conversion; derive custom convertors from this class.
class Convertor:
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

        self.inplace = True
        self.targetownstemp = True

        self.links = []
        self.defaults = []
        self.registerLinks()

    def registerLinks(self):
        pass

    def convert(self,source,target):
        if self.inplace:
            target.tempdir = source.tempdir
            if self.targetownstemp:
                source.tempdirowner = False
            else:
                target.tempdirowner = False

        # Try simple deep copy: nodes with the same name and location in both
        # source and target scenario will have their value copied.
        target.root.copyFrom(source.root)

        # Handle one-to-one links between source nodes and target nodes.
        for (sourcepath,targetpath) in self.links:
            if isinstance(sourcepath,str): sourcepath = sourcepath.split('/')
            if isinstance(targetpath,str): targetpath = targetpath.split('/')
            sourcenode = source.root.getLocation(sourcepath)
            if sourcenode==None:
                raise Exception('Cannot locate node "%s" in source scenario.' % '/'.join(sourcepath))
            targetnode = target.root.getLocation(targetpath)
            if targetnode==None:
                raise Exception('Cannot locate node "%s" in target scenario.' % '/'.join(targetpath))
            targetnode.copyFrom(sourcenode)

        # Reset target nodes to defaults where applicable.
        if len(self.defaults)>0:
            defscen = common.Scenario.getDefault(None,target.version)
            for path in self.defaults:
                if isinstance(path,str): path = path.split('/')
                sourcenode = defscen.root.getLocation(path)
                if sourcenode==None:
                    raise Exception('Cannot locate node "%s" in scenario.')
                targetnode = target.root.getLocation(path)
                targetnode.copyFrom(sourcenode)

    def reverseLinks(self):
        newlinks = []
        for (sourcepath,targetpath) in self.links:
            newlinks.append((targetpath,sourcepath))
        return newlinks

# ConvertorChain
#   Generic class for multiple-step conversions
#   Conversion steps are specified at initialization as a list of scenario ids,
#   i.e. [sourceid, intermediateid1, intermediateid2, ... , targetid]
class ConvertorChain(Convertor):
    def __init__(self,chain):
        Convertor.__init__(self,chain[0],chain[-1])
        self.chain = []
        for istep in range(len(chain)-1):
            convertor = getConvertor(chain[istep],chain[istep+1],directonly=True)
            self.chain.append(convertor)

    def convert(self,source,target):
        for convertor in self.chain:
            convertor.targetownstemp = self.targetownstemp
        
        temptargets = []
        for istep in range(len(self.chain)-1):
            convertor = self.chain[istep]
            temptargetid = convertor.targetid
            print 'Converting to temporary target "'+temptargetid+'".'
            temptarget = common.Scenario(templatename=temptargetid)
            temptargets.append(temptarget)
            convertor.convert(source,temptarget)
            source = temptarget
        convertor = self.chain[-1]
        print 'Converting to final target "'+target.version+'".'
        convertor.convert(source,target)
        for temptarget in temptargets: temptarget.unlink()

# ========================================================================================
# Here start custom convertors!
# ========================================================================================

class Convertor_gotm_3_2_4_to_gotm_3_3_2(Convertor):
    fixedsourceid = 'gotm-3.2.4'
    fixedtargetid = 'gotm-3.3.2'
    
    def registerLinks(self):
        self.links = [('/gotmmean/meanflow/charnok',    '/gotmmean/meanflow/charnock'),
                      ('/gotmmean/meanflow/charnok_val','/gotmmean/meanflow/charnock_val')]

    def convert(self,source,target):
        Convertor.convert(self,source,target)

        # Initialize oxygen profile namelist in obs.inp with a set of defaults.
        target.setProperty(['obs','o2_profile','o2_prof_method'],0)
        target.setProperty(['obs','o2_profile','o2_units'],0)
        target.setProperty(['obs','o2_profile','o2_prof_file'],'')
addConvertor(Convertor_gotm_3_2_4_to_gotm_3_3_2)

class Convertor_gotm_3_3_2_to_gotm_3_2_4(Convertor):
    fixedsourceid = 'gotm-3.3.2'
    fixedtargetid = 'gotm-3.2.4'

    def registerLinks(self):
        self.links = Convertor_gotm_3_2_4_to_gotm_3_3_2().reverseLinks()

    def convert(self,source,target):
        Convertor.convert(self,source,target)

        # Note: we implicitly lose the oxygen profile namelist in obs.inp; GOTM 3.2.4 does not support it.
addConvertor(Convertor_gotm_3_3_2_to_gotm_3_2_4)

class Convertor_gotm_3_3_2_to_gotm_4_0_0(Convertor):
    fixedsourceid = 'gotm-3.3.2'
    fixedtargetid = 'gotm-4.0.0'

    def registerLinks(self):
        self.links = [('/obs/ext_pressure/PressMethod','/obs/ext_pressure/ext_press_mode')]

        self.defaults = ['/obs/wave_nml','/bio','/bio_npzd','/bio_iow','/bio_sed','/bio_fasham']

    def convert(self,source,target):
        Convertor.convert(self,source,target)
addConvertor(Convertor_gotm_3_3_2_to_gotm_4_0_0)

class Convertor_gotm_4_0_0_to_gotm_3_3_2(Convertor):
    fixedsourceid = 'gotm-4.0.0'
    fixedtargetid = 'gotm-3.3.2'

    def registerLinks(self):
        self.links = Convertor_gotm_3_3_2_to_gotm_4_0_0().reverseLinks()

    def convert(self,source,target):
        Convertor.convert(self,source,target)

        # Note: we implicitly lose the wind wave profile namelist in obs.inp; GOTM 3.3.2 does not support it.
addConvertor(Convertor_gotm_4_0_0_to_gotm_3_3_2)

class Convertor_gotm_4_0_0_to_gotmgui_0_5_0(Convertor):
    fixedsourceid = 'gotm-4.0.0'
    fixedtargetid = 'gotmgui-0.5.0'

    def registerLinks(self):
        self.links = [('/gotmrun/model_setup/title',      '/title'),
                      ('/gotmrun/model_setup/dt',         '/timeintegration/dt'),
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
                      ('/kpp/kpp',                        '/gotmturb/kpp')]
    
    def convert(self,source,target):
        Convertor.convert(self,source,target)

        target.setProperty(['meanflow','z0s'],target.getProperty(['meanflow','z0s_min']))

        dt = target.getProperty(['timeintegration','dt'])
        target.setProperty(['output','dtsave'],dt*source.getProperty(['gotmrun','output','nsave']))

        # Note: we implicitly lose output settings out_fmt, out_dir and out_fn; the GUI scenario
        # does not support (or need) these.
addConvertor(Convertor_gotm_4_0_0_to_gotmgui_0_5_0)

class Convertor_gotmgui_0_5_0_to_gotm_4_0_0(Convertor):
    fixedsourceid = 'gotmgui-0.5.0'
    fixedtargetid = 'gotm-4.0.0'

    def registerLinks(self):
        self.links = Convertor_gotm_4_0_0_to_gotmgui_0_5_0().reverseLinks()
    
    def convert(self,source,target):
        Convertor.convert(self,source,target)

        if not source.getProperty(['meanflow','charnock']):
            target.setProperty(['gotmmean','meanflow','z0s_min'],source.getProperty(['meanflow','z0s']))

        dt = source.getProperty(['timeintegration','dt'])
        target.setProperty(['gotmrun','output','nsave'],int(source.getProperty(['output','dtsave'])/dt))

        # Add output path and type (not present in GUI scenarios)
        target.setProperty(['gotmrun','output','out_fmt'],2)
        target.setProperty(['gotmrun','output','out_dir'],'.')
        target.setProperty(['gotmrun','output','out_fn'],'result')
addConvertor(Convertor_gotmgui_0_5_0_to_gotm_4_0_0)

