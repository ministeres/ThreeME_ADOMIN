Option Explicit

Dim xlApp, xlBook

Set xlApp = CreateObject("Excel.Application")
Set xlBook = xlApp.Workbooks.Open(CreateObject("Scripting.FileSystemObject").GetParentFolderName(WScript.ScriptFullName) & "\Template.xlsm", 0, True)
xlApp.Run "ImportCSV"
xlBook.Close
xlApp.Quit

Set xlBook = Nothing
Set xlApp = Nothing

WScript.Quit