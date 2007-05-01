Cscript WiRunSQL.vbs ".\Local install\setup.msi" "UPDATE `Property` SET `Property`.`Value`='ALL' WHERE `Property`.`Property`='FolderForm_AllUsers'"
Cscript WiRunSQL.vbs ".\Local install\setup.msi" "DELETE FROM `Property` WHERE `Property`.`Property`='ARPCONTACT'"
