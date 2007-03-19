import xmlstore
import sys, os.path

class SettingsStore(xmlstore.TypedStore):
    def __init__(self):
        settingspath = self.getSettingsPath()
        if not os.path.isfile(settingspath): settingspath = None
        xmlstore.TypedStore.__init__(self,'schemas/settings/gotmgui.xml',settingspath)

    @staticmethod    
    def getSettingsPath():
        if sys.platform == 'win32':
            from win32com.shell import shellcon, shell
            appdata = shell.SHGetFolderPath(0, shellcon.CSIDL_APPDATA, 0, 0)
            return os.path.join(appdata,'GOTM','settings.xml')
        else:
            return os.path.expanduser('~/.gotm.gui')

    def save(self):
        if not self.changed: return
        settingspath = self.getSettingsPath()
        settingsdir = os.path.dirname(settingspath)
        if not os.path.isdir(settingsdir): os.mkdir(settingsdir)
        xmlstore.TypedStore.save(self,settingspath)
