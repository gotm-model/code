' Windows Installer utility to execute SQL statements against an installer database
' For use with Windows Scripting Host, CScript.exe or WScript.exe
' Copyright (c) Microsoft Corporation. All rights reserved.
' Demonstrates the script-driven database queries and updates
'
Option Explicit

Const msiOpenDatabaseModeTransact = 1
Const msiViewModifyReplace = 4

Dim argNum, argCount:argCount = Wscript.Arguments.Count
If (argCount < 4) Then
	Wscript.Echo "Windows Installer utility to change primary key in a record in an installer database." &_
		vbLf & " The 1st argument specifies the path to the MSI database, relative or full path" &_
		vbLf & " The 2nd argument specifies the SQL SELECT query to execute - must be in double quotes" &_
		vbLf & " The 3rd argument specifies the name of the column to change" &_
		vbLf & " The 4th argument specifies the new value"
	Wscript.Quit 1
End If

' Connect to Windows installer object
On Error Resume Next
Dim installer : Set installer = Nothing
Set installer = Wscript.CreateObject("WindowsInstaller.Installer") : CheckError

' Open database
Dim databasePath:databasePath = Wscript.Arguments(0)
Dim database : Set database = installer.OpenDatabase(databasePath, msiOpenDatabaseModeTransact) : CheckError

' Process SQL statements
Dim query, view, colinfo, column, record
query = Wscript.Arguments(1)
Set view = database.OpenView(query) : CheckError
view.Execute : CheckError

Set colinfo = view.ColumnInfo(0) : CheckError
For column = 0 To colinfo.FieldCount
	If colinfo.StringData(column)=Wscript.Arguments(2) Then Exit For
Next
WScript.Echo "Column "&Wscript.Arguments(2)&" at index "&column

Do
	Set record = view.Fetch : CheckError
	If record Is Nothing Then Exit Do
	record.StringData(column) = Wscript.Arguments(3)
	view.Modify msiViewModifyReplace,record : CheckError
Loop
database.Commit : CheckError
Wscript.Quit 0

Sub CheckError
	Dim message, errRec
	If Err = 0 Then Exit Sub
	message = Err.Source & " " & Hex(Err) & ": " & Err.Description
	If Not installer Is Nothing Then
		Set errRec = installer.LastErrorRecord
		If Not errRec Is Nothing Then message = message & vbLf & errRec.FormatText
	End If
	Fail message
End Sub

Sub Fail(message)
	Wscript.Echo message
	Wscript.Quit 2
End Sub
