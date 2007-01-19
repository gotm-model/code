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
        return convertorclass(sourceid,targetid)

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

# addConvertor(sourceid,targetid,convertorclass)
#   [internal use only!]
#   Register a convertor class, instances of with can convert between the specified sourceid and targetid.
def addConvertor(sourceid,targetid,convertorclass):
    if sourceid not in convertorsfrom: convertorsfrom[sourceid] = {}
    if targetid in convertorsfrom[sourceid]:
        raise Exception('Error! A class for converting between '+sourceid+' and '+targetid+' was already specified previously.')
    convertorsfrom[sourceid][targetid] = convertorclass

# Convertor
#   Base class for conversion; derive custom convertors from this class.
class Convertor:
    def __init__(self,sourceid,targetid):
        self.sourceid = sourceid
        self.targetid = targetid

        (self.sourcename,self.sourceversion) = sourceid.split('-')
        (self.targetname,self.targetversion) = targetid.split('-')

        self.sourceversionint = versionStringToInt(self.sourceversion)
        self.targetversionint = versionStringToInt(self.targetversion)

        self.inplace = True
        self.targetownstemp = True
    
    def convert(self,source,target):
        # Try simple deep copy: nodes with the same name and location in both
        # source and target scenario will have their value copied.
        if self.inplace:
            target.tempdir = source.tempdir
            if self.targetownstemp:
                source.tempdirowner = False
            else:
                target.tempdirowner = False
        target.root.copyFrom(source.root)

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
    def convert(self,source,target):
        Convertor.convert(self,source,target)
        target.setProperty(['gotmmean','meanflow','charnock'],    source.getProperty(['gotmmean','meanflow','charnok']))
        target.setProperty(['gotmmean','meanflow','charnock_val'],source.getProperty(['gotmmean','meanflow','charnok_val']))

        # Initialize oxygen profile namelist in obs.inp with a set of defaults.
        target.setProperty(['obs','o2_profile','o2_prof_method'],0)
        target.setProperty(['obs','o2_profile','o2_units'],0)
        target.setProperty(['obs','o2_profile','o2_prof_file'],'')
addConvertor('gotm-3.2.4','gotm-3.3.2',Convertor_gotm_3_2_4_to_gotm_3_3_2)

class Convertor_gotm_3_3_2_to_gotm_3_2_4(Convertor):
    def convert(self,source,target):
        Convertor.convert(self,source,target)
        target.setProperty(['gotmmean','meanflow','charnok'],    source.getProperty(['gotmmean','meanflow','charnock']))
        target.setProperty(['gotmmean','meanflow','charnok_val'],source.getProperty(['gotmmean','meanflow','charnock_val']))
        # Note: we implicitly lose the oxygen profile namelist in obs.inp; GOTM 3.2.4 does not support it.
addConvertor('gotm-3.3.2','gotm-3.2.4',Convertor_gotm_3_3_2_to_gotm_3_2_4)

class Convertor_gotm_3_3_2_to_gotm_4_0_0(Convertor):
    def convert(self,source,target):
        Convertor.convert(self,source,target)

        target.setProperty(['obs','ext_pressure','ext_press_mode'], source.getProperty(['obs','ext_pressure','PressMethod']))

        # Initialize wind wave namelist in obs.inp with a set of defaults.
        target.setProperty(['obs','wave_nml','wave_method'],0)
        target.setProperty(['obs','wave_nml','wave_file'],'')
        target.setProperty(['obs','wave_nml','Hs'],0)
        target.setProperty(['obs','wave_nml','Tz'],0)
        target.setProperty(['obs','wave_nml','phiw'],0)
addConvertor('gotm-3.3.2','gotm-4.0.0',Convertor_gotm_3_3_2_to_gotm_4_0_0)

class Convertor_gotm_4_0_0_to_gotm_3_3_2(Convertor):
    def convert(self,source,target):
        Convertor.convert(self,source,target)
        target.setProperty(['obs','ext_pressure','PressMethod'], source.getProperty(['obs','ext_pressure','ext_press_mode']))
        # Note: we implicitly lose the wind wave profile namelist in obs.inp; GOTM 3.3.2 does not support it.
addConvertor('gotm-4.0.0','gotm-3.3.2',Convertor_gotm_4_0_0_to_gotm_3_3_2)

class Convertor_gotm_4_0_0_to_gotmgui_0_5_0(Convertor):
    def convert(self,source,target):
        Convertor.convert(self,source,target)
        target.setProperty(['title'],source.getProperty(['gotmrun','model_setup','title']))
        target.setProperty(['timeintegration','dt'   ],source.getProperty(['gotmrun','model_setup','dt']))
        target.setProperty(['timeintegration','cnpar'],source.getProperty(['gotmrun','model_setup','cnpar']))
        target.root.getLocation(['station']).copyFrom(source.root.getLocation(['gotmrun','station']))
        target.root.getLocation(['time'   ]).copyFrom(source.root.getLocation(['gotmrun','time'   ]))
        target.root.getLocation(['output' ]).copyFrom(source.root.getLocation(['gotmrun','output' ]))
        target.setProperty(['grid','nlev'       ],source.getProperty(['gotmrun','model_setup','nlev']))
        
        target.setProperty(['grid','grid_method'],source.getProperty(['gotmmean','meanflow','grid_method']))
        target.setProperty(['grid','ddu'        ],source.getProperty(['gotmmean','meanflow','ddu']))
        target.setProperty(['grid','ddl'        ],source.getProperty(['gotmmean','meanflow','ddl']))
        target.setProperty(['grid','grid_file'  ],source.getProperty(['gotmmean','meanflow','grid_file']))
        target.root.getLocation(['meanflow']).copyFrom(source.root.getLocation(['gotmmean','meanflow']))
        target.setProperty(['meanflow','z0s'],source.getProperty(['gotmmean','meanflow','z0s_min']))
        
        target.root.getLocation(['airsea'  ]).copyFrom(source.root.getLocation(['airsea',  'airsea']))
        target.root.getLocation(['gotmturb']).copyFrom(source.root.getLocation(['gotmturb','turbulence']))
addConvertor('gotm-4.0.0','gotmgui-0.5.0',Convertor_gotm_4_0_0_to_gotmgui_0_5_0)

class Convertor_gotmgui_0_5_0_to_gotm_4_0_0(Convertor):
    def convert(self,source,target):
        Convertor.convert(self,source,target)

        # Convert gotmrun namelists
        target.setProperty(['gotmrun','model_setup','title'],source.getProperty(['title']))
        target.setProperty(['gotmrun','model_setup','dt'   ],source.getProperty(['timeintegration','dt']))
        target.setProperty(['gotmrun','model_setup','cnpar'],source.getProperty(['timeintegration','cnpar']))
        target.root.getLocation(['gotmrun','station']).copyFrom(source.root.getLocation(['station']))
        target.root.getLocation(['gotmrun','time'   ]).copyFrom(source.root.getLocation(['time'   ]))
        target.root.getLocation(['gotmrun','output' ]).copyFrom(source.root.getLocation(['output' ]))
        target.setProperty(['gotmrun','model_setup','nlev'     ],source.getProperty(['grid','nlev'       ]))
        
        target.root.getLocation(['gotmmean','meanflow']).copyFrom(source.root.getLocation(['meanflow']))
        target.setProperty(['gotmmean','meanflow','grid_method'],source.getProperty(['grid','grid_method']))
        target.setProperty(['gotmmean','meanflow','ddu'        ],source.getProperty(['grid','ddu'        ]))
        target.setProperty(['gotmmean','meanflow','ddl'        ],source.getProperty(['grid','ddl'        ]))
        target.setProperty(['gotmmean','meanflow','grid_file'  ],source.getProperty(['grid','grid_file'  ]))
        if not source.getProperty(['meanflow','charnock']):
            target.setProperty(['gotmmean','meanflow','z0s_min'],source.getProperty(['meanflow','z0s']))

        target.root.getLocation(['airsea',  'airsea'  ]).copyFrom(source.root.getLocation(['airsea']))
        target.root.getLocation(['gotmturb','turbulence']).copyFrom(source.root.getLocation(['gotmturb']))

        # Add output path and type (not present in GUI scenarios)
        target.setProperty(['gotmrun','output','out_fmt'],2)
        target.setProperty(['gotmrun','output','out_dir'],'.')
        target.setProperty(['gotmrun','output','out_fn'],'result')
addConvertor('gotmgui-0.5.0','gotm-4.0.0',Convertor_gotmgui_0_5_0_to_gotm_4_0_0)
