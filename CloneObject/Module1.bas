Attribute VB_Name = "Module1"

'Public Const BaseAddress = &H11000000


'Public Declare Function GlobalHandle Lib "kernel32" (wMem As Any) As Long
'Public Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
'Public Declare Function GlobalReAlloc Lib "kernel32" (ByVal hMem As Long, ByVal dwBytes As Long, ByVal wFlags As Long) As Long


'Public Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long


'Public Declare Function vbaObjSet Lib "msvbvm60.dll" Alias "__vbaObjSet" _
'    (dstObject As Any, ByVal srcObjPtr As Long) As Long
    
'Public Declare Function GetModuleHandle Lib "kernel32" Alias "GetModuleHandleA" (ByVal lpModuleName As String) As Long
'Public Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long


Public Declare Sub CopyMemory Lib "kernel32" Alias "RtlMoveMemory" (Dest As Any, Source As Any, ByVal Size As Long)
Public Declare Sub RtlMoveMemory Lib "kernel32" (Dest As Long, Source As Long, ByVal Size As Long)
'Private Declare Function WriteProcessMemory Lib "kernel32" (ByVal hProcess As Long, ByVal lpBaseAddress As Any, ByVal lpBuffer As Any, ByVal nSize As Long, ByVal lpNumberOfBytesWritten As Long) As Long


Option Explicit

Private Declare Function ReadProcessMemory Lib "kernel32" (ByVal hProcess As Long, ByVal lpBaseAddress As Any, lpBuffer As Any, ByVal nSize As Long, lpNumberOfBytesWritten As Long) As Long
Public Declare Function vbaObjSetAddref Lib "msvbvm60.dll" Alias "__vbaObjSetAddref" (dstObject As Any, ByVal srcObjPtr As Long) As Long

Public Declare Function GetCurrentProcessId Lib "kernel32" () As Long
Private Declare Function OpenProcess Lib "kernel32" (ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, ByVal dwProcessId As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" (ByVal hObject As Long) As Long
Private Const PROCESS_ALL_ACCESS As Long = &H1F0FFF

Public Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Public Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalFlags Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalSize Lib "kernel32" (ByVal hMem As Long) As Long

Public Sub Main()

    Dim try As Object 'create a object to be cloned
    Set try = New Class1

    Dim too As Object 'this is where the clone will be

    try.Info = 10 'set some info in the object

    try.Test "Original object" 'run a function element of the object

    Dim lSize As Long 'get the objects size
    lSize = GlobalSize(ObjPtr(try))

    Dim hMem As Long 'allocate new memory for another object
    hMem = GlobalAlloc(GlobalFlags(ObjPtr(try)), lSize)

    Dim pHandle As Long 'open current process memory for reading
    pHandle = OpenProcess(PROCESS_ALL_ACCESS, False, GetCurrentProcessId)

    'this readprocessmemory is not available on all Windows OS, there is a write too
    If ReadProcessMemory(pHandle, ObjPtr(try), ByVal hMem, lSize, 0) <> 0 Then

        'successful reading, set a reference to the new object memory
        vbaObjSetAddref too, hMem

    End If

    CloseHandle pHandle 'close the open process handle

    too.Test "Cloned object" 'show that it is the same as the original object

    too.Info = 26 'try out our cloned object with new info

    too.Test "Changed clones" 'by running a functional element of the object

    try.Test "Still original" 'show that the previous object is not changed

    too.Test "While clones" 'our cloned object holds its value not original

    Set too = Nothing 'free the reference to the clone
    GlobalFree hMem 'be sure and free up the memory

    Set try = Nothing 'clean up our original object
    
End Sub
