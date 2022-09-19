Attribute VB_Name = "modCommon"
#Const modCommon = -1
Option Explicit
Option Compare Binary

Option Private Module

Private Type POINTAPI
        X As Long
        Y As Long
End Type

Private Type Msg
    hwnd As Long
    Message As Long
    wParam As Long
    lParam As Long
    time As Long
    pt As POINTAPI
End Type

Private Const PM_NOREMOVE = &H0
Private Const PM_REMOVE = &H1
Private Const PM_NOYIELD = &H2

Private Declare Function TranslateMessage Lib "user32" (lpMsg As Msg) As Long
Private Declare Function DispatchMessage Lib "user32" Alias "DispatchMessageA" (lpMsg As Msg) As Long
Private Declare Function PeekMessage Lib "user32" Alias "PeekMessageA" (lpMsg As Msg, ByVal hwnd As Long, ByVal wMsgFilterMin As Long, ByVal wMsgFilterMax As Long, ByVal wRemoveMsg As Long) As Long
Private Declare Function GetMessage Lib "user32" Alias "GetMessageA" (lpMsg As Msg, ByVal hwnd As Long, ByVal wMsgFilterMin As Long, ByVal wMsgFilterMax As Long) As Long

Public Declare Function GetCurrentThreadId Lib "kernel32" () As Long

Public Declare Function EnumThreadWindows Lib "user32" (ByVal dwThreadId As Long, ByVal lpfn As Long, ByVal lParam As Long) As Long
Public Declare Function EnumChildWindows Lib "user32" (ByVal hWndParent As Long, ByVal lpEnumFunc As Long, ByVal lParam As Long) As Long
Public Declare Function EnumWindows Lib "user32" (ByVal lpEnumFunc As Long, ByVal lParam As Long) As Boolean

Public Declare Function GetCurrentProcessId Lib "kernel32" () As Long
Public Declare Function GetWindowThreadProcessId Lib "user32" (ByVal hwnd As Long, lpdwProcessId As Long) As Long
Private Declare Function GetSystemDirectory Lib "kernel32" Alias "GetSystemDirectoryA" (ByVal path As String, ByVal cbBytes As Long) As Long
Private Declare Function GetModuleFileName Lib "kernel32" Alias "GetModuleFileNameA" (ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long
Private Declare Function GetProcAddress Lib "kernel32" (ByVal hModule As Long, ByVal lpProcName As String) As Long
Private Declare Function GetCurrentProcess Lib "kernel32" () As Long

Public Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Private Declare Function IsWindow Lib "user32" (ByVal hwnd As Long) As Long

Private Declare Function IsWow64Process Lib "kernel32" (ByVal hProc As Long, ByRef bWow64Process As Boolean) As Long
Private Declare Function GetWindowText Lib "user32" Alias "GetWindowTextA" (ByVal hwnd As Long, ByVal lpString As String, ByVal cch As Long) As Long
Private Declare Function GetModuleHandle Lib "kernel32" Alias "GetModuleHandleA" (ByVal lpModuleName As String) As Long

Private doStack As Long
Private gMsg As Msg

Private IsDebugState As Variant
Private IsDebugCount As Long
Private IsDebugHwnds As String

#If Not modMemory Then
Public Declare Sub RtlMoveMemory Lib "kernel32" (ByRef Dest As Any, ByRef Source As Any, ByVal Length As Long)
#End If

#If Not modMemory Then

Public Const GMEM_FIXED = &H0
Public Const GMEM_MOVEABLE = &H2
Public Const GPTR = &H40
Public Const GHND = &H42

Public Declare Function GlobalReAlloc Lib "kernel32" (ByVal hMem As Long, ByVal dwBytes As Long, ByVal wFlags As Long) As Long
Public Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Public Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
Public Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long

#End If
Public Function repeat(ByVal Number As Integer, ByVal Expression As String) As String
    If Number > 1 Then
        Dim cnt As Integer
        For cnt = Number To 1 Step -1
            repeat = repeat & Expression
        Next
    ElseIf Number = 1 Then
        repeat = Expression
    End If
End Function
Public Function RootOf(ByVal Base As Double, ByVal Exponent As Double) As Double
    RootOf = Base ^ (1 / Exponent)
End Function

Public Function InvertNum(ByVal Value As Long, Optional ByVal Whole As Long = 100, Optional ByVal Unit As Long = 1)
 InvertNum = -(Whole / Unit) + -(Value / Unit) + ((Whole / Unit) * 2)
End Function

Public Sub Swap(ByRef Var1, ByRef Var2, Optional ByRef Var3, Optional ByRef Var4, Optional ByRef Var5, Optional ByRef Var6)
    Dim Var0
    If (VBA.IsObject(Var1) Or VBA.TypeName(Var1) = "Nothing") Or _
        (VBA.IsObject(Var2) Or VBA.TypeName(Var2) = "Nothing") Then
        
        If IsMissing(Var3) Then
            Set Var0 = Var1
            Set Var1 = Var2
            Set Var2 = Var0
        ElseIf IsMissing(Var4) Then
            Set Var0 = Var1
            Set Var1 = Var2
            Set Var2 = Var3
            Set Var3 = Var0
        ElseIf IsMissing(Var5) Then
            Set Var0 = Var1
            Set Var1 = Var2
            Set Var2 = Var3
            Set Var3 = Var4
            Set Var4 = Var0
        ElseIf IsMissing(Var6) Then
            Set Var0 = Var1
            Set Var1 = Var2
            Set Var2 = Var3
            Set Var3 = Var4
            Set Var4 = Var5
            Set Var5 = Var0
        Else
            Set Var0 = Var1
            Set Var1 = Var2
            Set Var2 = Var3
            Set Var3 = Var4
            Set Var4 = Var5
            Set Var5 = Var6
            Set Var6 = Var0
        End If
    
    Else
        
        If IsMissing(Var3) Then
            Var0 = Var1
            Var1 = Var2
            Var2 = Var0
        ElseIf IsMissing(Var4) Then
            Var0 = Var1
            Var1 = Var2
            Var2 = Var3
            Var3 = Var0
        ElseIf IsMissing(Var5) Then
            Var0 = Var1
            Var1 = Var2
            Var2 = Var3
            Var3 = Var4
            Var4 = Var0
        ElseIf IsMissing(Var6) Then
            Var0 = Var1
            Var1 = Var2
            Var2 = Var3
            Var3 = Var4
            Var4 = Var5
            Var5 = Var0
        Else
            Var0 = Var1
            Var1 = Var2
            Var2 = Var3
            Var3 = Var4
            Var4 = Var5
            Var5 = Var6
            Var6 = Var0
        End If
    End If
End Sub


#If Not modMemory Then

Public Function ArraySize(InArray, Optional ByVal InBytes As Boolean = False) As Long
On Error GoTo dimerror

    Static dimcheck As Long

    If UBound(InArray) = -1 Or LBound(InArray) = -1 Then
        ArraySize = 0
    Else
        ArraySize = (UBound(InArray) + -CInt(Not CBool(-LBound(InArray)))) * IIf(InBytes, LenB(InArray(LBound(InArray))), 1)
    End If
    Exit Function
startover:
    Err.Clear
    On Error GoTo -1
    On Error GoTo 0
    On Error GoTo dimerror
    If UBound(InArray, dimcheck) = -1 Or LBound(InArray, dimcheck) = -1 Then
        ArraySize = 0
    Else
        ArraySize = (UBound(InArray, dimcheck) + -CInt(Not CBool(-LBound(InArray, dimcheck)))) * IIf(InBytes, LenB(InArray(LBound(InArray, dimcheck), LBound(InArray, dimcheck - 1))), 1)
    End If
    
    Exit Function
dimerror:
    If dimcheck = 0 Then
        dimcheck = 2
        Err.Clear
        GoTo startover
    End If
    ArraySize = 0
End Function

Public Function Convert(Info)
    Dim N As Long
    Dim out() As Byte
    Dim Ret As String
    Select Case VBA.TypeName(Info)
        Case "String"
            If Len(Info) > 0 Then
                ReDim out(0 To Len(Info) - 1) As Byte
                For N = 0 To Len(Info) - 1
                    out(N) = Asc(Mid(Info, N + 1, 1))
                Next
            Else
                ReDim out(-1 To -1) As Byte
            End If
            Convert = out
        Case "Byte()"
            If (ArraySize(Info) > 0) Then
                On Error GoTo dimcheck
                For N = LBound(Info) To UBound(Info)
                    Ret = Ret & Chr(Info(N))
                Next
            End If
            Convert = Ret
    End Select
    Exit Function
dimcheck:
    If Err Then Err.Clear
    For N = LBound(Info, 2) To UBound(Info, 2)
        Ret = Ret & Chr(Info(0, N))
    Next
    Convert = Ret
End Function
#End If

#If Not modBitValue Then

Public Function ByteBound() As Byte
    ByteBound = &HFF&
End Function
Public Function IntBound() As Long
    IntBound = &HFFFF&
End Function
Public Function LongBound() As Double
'16777216=color depth 24bit
'2147483647=long depth 32bit

'CDbl(16777216) * CDbl(1024)=17179869184
'17179869184 / 8= 2147483648

'A 4-byte integer ranging in value from -2,147,483,648 to 2,147,483,647. The ampersand (&) type-declaration character represents a Long in Visual Basic.


    LongBound = CDbl(2147483647) ''CLng("2147483647") '&H11000000 '&H10FFFFFF
End Function
Public Function HighBound() As Variant
    HighBound = CDec("92233723685477")
End Function

Public Function LoLong(ByVal lParam As Double) As Long
    If (lParam And &H10FFFFFF) > &H8800000 Then
        LoLong = (lParam And &H10FFFFFF) - (&H11000000 * (lParam \ &H11000000))
    Else
        LoLong = lParam And &H10FFFFFF
    End If
End Function

Public Function HiLong(ByVal lParam As Double) As Long
    If ((lParam And &HEF000000) \ &H11000000) + (lParam Mod &H11000000) < 0 Then
        HiLong = -(((lParam And &HEF000000) \ &H11000000) + (lParam Mod &H11000000))
    Else
        HiLong = ((lParam And &HEF000000) \ &H11000000)
    End If
End Function

Public Function HiInt(ByVal lParam As Long) As Integer
    If (lParam And &HFFFF&) > &H7FFF Then
        HiInt = (lParam And &HFFFF&) - &H10000
    Else
        HiInt = lParam And &HFFFF&
    End If
End Function

Public Function LoInt(ByVal lParam As Long) As Integer
    LoInt = (lParam And &HFFFF0000) \ &H10000
End Function

Public Function HiByte(ByVal wParam As Integer) As Byte
    HiByte = wParam \ &H100 And &HFF&
End Function

Public Function LoByte(ByVal wParam As Integer) As Byte
    LoByte = wParam And &HFF&
End Function

#End If

Public Function IsDesignMode() As Boolean
    'returns true if the title bar of the vbide debugger of the calling app
    'has the "[design]" caption immediate following "Microsoft Visual Basic"
    Dim TmpHwnds As String
    IsDebugHwnds = ""
    IsDebugState = 0
    EnumWindows AddressOf IsDebuggingWinEvents, GetCurrentProcessId
    Do Until (IsDebugState <> 0) Or ((IsDebugHwnds = TmpHwnds & IsDebugHwnds) And (TmpHwnds = ""))
        TmpHwnds = TmpHwnds & IsDebugHwnds
        DoEvents
        If InStr(IsDebugHwnds, TmpHwnds) > 0 Then
            IsDebugHwnds = TmpHwnds
            TmpHwnds = IsDebugHwnds & IsDebugHwnds
        Else
            TmpHwnds = ""
        End If
    Loop
    IsDesignMode = (IsDebugState = 1)

End Function
Public Function IsRunningMode() As Boolean
    'returns true if the title bar of the vbide debugger of the calling app
    'has the "[running]" caption immediate following "Microsoft Visual Basic"
    
    'note: run and running are different, as running is used on automation startup
    'but not during automation execution, for instance a property read or write
    'will have run as if the applicaiton has, while really during compile automate,
    'while the opening and closing of a control in design mode will state runnnig
    'use iscompiler/iscompiled/isdebugger to determine whether a designtime or
    'compile time difference of automation event happens, vs. running or compiled.
    'these vbide/debug functions will cause a performance loss when heavily used
    'as they all remain during compile and are for prothedic sized app needings
    'where ability pathology anomilies of run/compile run, will be ajar different
    'but nessisary to the scope of a architecture of plenty of programmed needs
    Dim TmpHwnds As String
    IsDebugHwnds = ""
    IsDebugState = 0
    EnumWindows AddressOf IsDebuggingWinEvents, GetCurrentProcessId
    Do Until (IsDebugState <> 0) Or ((IsDebugHwnds = TmpHwnds & IsDebugHwnds) And (TmpHwnds = ""))
        TmpHwnds = TmpHwnds & IsDebugHwnds
        DoEvents
        If InStr(IsDebugHwnds, TmpHwnds) > 0 Then
            IsDebugHwnds = TmpHwnds
            TmpHwnds = IsDebugHwnds & IsDebugHwnds
        Else
            TmpHwnds = ""
        End If
    Loop
    IsRunningMode = (IsDebugState = 2)

End Function
Public Function IsRunMode() As Boolean
    'returns true if the title bar of the vbide debugger of the calling app
    'has the "[run]" caption immediate following "Microsoft Visual Basic"
    
    'note: run and running are different, as running is used on automation startup
    'but not during automation execution, for instance a property read or write
    'will have run as if the applicaiton has, while really during compile automate,
    'while the opening and closing of a control in design mode will state runnnig
    'use iscompiler/iscompiled/isdebugger to determine whether a designtime or
    'compile time difference of automation event happens, vs. running or compiled.
    'these vbide/debug functions will cause a performance loss when heavily used
    'as they all remain during compile and are for prothedic sized app needings
    'where ability pathology anomilies of run/compile run, will be ajar different
    'but nessisary to the scope of a architecture of plenty of programmed needs
    
    Dim TmpHwnds As String
    IsDebugHwnds = ""
    IsDebugState = 0
    EnumWindows AddressOf IsDebuggingWinEvents, GetCurrentProcessId
    Do Until (IsDebugState <> 0) Or ((IsDebugHwnds = TmpHwnds & IsDebugHwnds) And (TmpHwnds = ""))
        TmpHwnds = TmpHwnds & IsDebugHwnds
        DoEvents
        If InStr(IsDebugHwnds, TmpHwnds) > 0 Then
            IsDebugHwnds = TmpHwnds
            TmpHwnds = IsDebugHwnds & IsDebugHwnds
        Else
            TmpHwnds = ""
        End If
    Loop
    IsRunMode = (IsDebugState = 3)

End Function
Public Function IsBreakMode() As Boolean
    'returns true if the title bar of the vbide debugger of the calling app
    'has the "[break]" caption immediate following "Microsoft Visual Basic"
    Dim TmpHwnds As String
    IsDebugHwnds = ""
    IsDebugState = 0
    EnumWindows AddressOf IsDebuggingWinEvents, GetCurrentProcessId
    Do Until (IsDebugState <> 0) Or ((IsDebugHwnds = TmpHwnds & IsDebugHwnds) And (TmpHwnds = ""))
        TmpHwnds = TmpHwnds & IsDebugHwnds
        DoEvents
        If InStr(IsDebugHwnds, TmpHwnds) > 0 Then
            IsDebugHwnds = TmpHwnds
            TmpHwnds = IsDebugHwnds & IsDebugHwnds
        Else
            TmpHwnds = ""
        End If
    Loop
    IsBreakMode = (IsDebugState = 4)
End Function

Private Function IsDebuggingWinEvents(ByVal hwnd As Long, ByVal lParam As Long) As Boolean
    
    Dim txt As String
    Dim lSize As Long
    txt = Space$(255)
    lSize = Len(txt)
    Call GetWindowText(hwnd, txt, lSize)
    If lSize > 0 Then
        txt = Trim(Replace(Left$(txt, lSize), Chr(0), ""))
    End If
    IsDebugHwnds = IsDebugHwnds & hwnd & " "
    If VBA.TypeName(IsDebugState) = "String" Then
        If (InStr(1, txt, IsDebugState, vbTextCompare) > 0) Then
            IsDebugState = "TRUE"
        Else
            IsDebuggingWinEvents = (Not (IsDebugState = "TRUE"))
            EnumChildWindows hwnd, AddressOf IsDebuggingWinChildEvents1, lParam
        End If
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [design]", vbTextCompare) > 0) Then
        IsDebugState = 1
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [running]", vbTextCompare) > 0) Then
        IsDebugState = 2
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [run]", vbTextCompare) > 0) Then
        IsDebugState = 3
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [break]", vbTextCompare) > 0) Then
        IsDebugState = 4
    Else
        IsDebuggingWinEvents = (Not (IsDebugState <> 0))
        EnumChildWindows hwnd, AddressOf IsDebuggingWinChildEvents1, lParam
    End If

End Function
Private Function IsDebuggingWinChildEvents1(ByVal hwnd As Long, ByVal lParam As Long) As Boolean
    
    Dim txt As String
    Dim lSize As Long
    txt = Space$(255)
    lSize = Len(txt)
    Call GetWindowText(hwnd, txt, lSize)
    If lSize > 0 Then
        txt = Trim(Replace(Left$(txt, lSize), Chr(0), ""))
    End If
    IsDebugHwnds = IsDebugHwnds & hwnd & " "
    If VBA.TypeName(IsDebugState) = "String" Then
        If (InStr(1, txt, IsDebugState, vbTextCompare) > 0) Then
            IsDebugState = "TRUE"
        Else
            IsDebuggingWinChildEvents1 = (Not (IsDebugState = "TRUE"))
            'EnumChildWindows hwnd, AddressOf IsDebuggingWinChildEvents2, lParam
        End If
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [design]", vbTextCompare) > 0) Then
        IsDebugState = 1
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [running]", vbTextCompare) > 0) Then
        IsDebugState = 2
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [run]", vbTextCompare) > 0) Then
        IsDebugState = 3
    ElseIf (InStr(1, txt, "Microsoft Visual Basic [break]", vbTextCompare) > 0) Then
        IsDebugState = 4
    Else
        IsDebuggingWinChildEvents1 = (Not (IsDebugState <> 0))
        'EnumChildWindows hwnd, AddressOf IsDebuggingWinChildEvents2, lParam
    End If
    
End Function
'Private Function IsDebuggingWinChildEvents2(ByVal hwnd As Long, ByVal lParam As Long) As Boolean
'
'    Dim txt As String
'    Dim lSize As Long
'    txt = Space$(255)
'    lSize = Len(txt)
'    Call GetWindowText(hwnd, txt, lSize)
'    If lSize > 0 Then
'        txt = Trim(Replace(Left$(txt, lSize), Chr(0), ""))
'    End If
'    IsDebugHwnds = IsDebugHwnds & hwnd & " "
'    If TypeName(IsDebugState) = "String" Then
'        If (InStr(txt, IsDebugState) > 0) Then
'            IsDebugState = "TRUE"
'        Else
'            IsDebuggingWinChildEvents2 = (Not (IsDebugState = "TRUE"))
'            EnumChildWindows hwnd, AddressOf IsDebuggingWinChildEvents1, lParam
'        End If
'    ElseIf (InStr(txt, "Microsoft Visual Basic [design]") > 0) Then
'        IsDebugState = 1
'    ElseIf (InStr(txt, "Microsoft Visual Basic [running]") > 0) Then
'        IsDebugState = 2
'    ElseIf (InStr(txt, "Microsoft Visual Basic [run]") > 0) Then
'        IsDebugState = 3
'    ElseIf (InStr(txt, "Microsoft Visual Basic [break]") > 0) Then
'        IsDebugState = 4
'    Else
'        IsDebuggingWinChildEvents2 = (Not (IsDebugState <> 0))
'        EnumChildWindows hwnd, AddressOf IsDebuggingWinChildEvents1, lParam
'    End If
'
'End Function

Public Function System64Bit() As Boolean
    Dim Handle As Long
    Dim is64Bit As Boolean
    is64Bit = False
    Handle = GetProcAddress(GetModuleHandle("kernel32"), "IsWow64Process")
    If Handle <> 0 Then
        IsWow64Process GetCurrentProcess(), is64Bit
    End If
    System64Bit = is64Bit
End Function


Public Function FileSize(ByVal fname As String) As Double
    Dim myFso As Object
    Set myFso = CreateObject("Scripting.FileSystemObject")
    Dim f As Object
    Set f = myFso.GetFile(fname)
    FileSize = f.Size
    Set f = Nothing
    Set myFso = Nothing
End Function

Public Function Nor(exp1, exp2)
    Nor = (((((Not exp1) Or (Not exp1)) = ((Not -exp2) And (Not -exp2)))) And ((((Not exp1) And (Not exp1)) = ((Not -exp2) Or (Not -exp2)))))
End Function

Public Sub DoTasks()
    
    Static dMsg As Msg
    Do While PeekMessage(dMsg, -1, 0, 0, PM_REMOVE + PM_NOYIELD)
        TranslateMessage dMsg
        DispatchMessage dMsg
    Loop
            
    If PeekMessage(dMsg, 0, 0, 0, PM_NOREMOVE + PM_NOYIELD) Then
        doStack = doStack + 1
        If doStack = 1 Then
            EnumWindows AddressOf WinEvents, GetCurrentProcessId
            Do While PeekMessage(dMsg, 0, 0, 0, PM_REMOVE + PM_NOYIELD)
                TranslateMessage dMsg
                DispatchMessage dMsg
                 EnumWindows AddressOf WinEvents, GetCurrentProcessId
            Loop
            EnumWindows AddressOf WinEvents, -4
        End If
        doStack = doStack - 1
        EnumWindows AddressOf WinEvents, -2
    ElseIf doStack = 1 Then
        EnumWindows AddressOf WinEvents, -1
    Else
        EnumWindows AddressOf WinEvents, -3
    End If
    
End Sub

Private Function WinEvents(ByVal hwnd As Long, ByVal lParam As Long) As Boolean
    Dim pId As Long
    Static wMsg As Msg
    If (lParam <= 0) And (lParam >= -3) Then
        If PeekMessage(wMsg, hwnd, 0, 0, PM_REMOVE + PM_NOYIELD) Then
            Do
                TranslateMessage wMsg
                DispatchMessage wMsg
            Loop While PeekMessage(wMsg, 0, 0, 0, PM_REMOVE + PM_NOYIELD)
        ElseIf (lParam >= -1) Then
            Sleep 1
        End If
        If (lParam = -3) Then Sleep 1
        If doStack <= 1 Then DoEvents
    ElseIf (lParam = -4) Then
        
        If (doStack > 1) And (doStack < 3) Then
            If PeekMessage(wMsg, 0, 0, 0, PM_NOREMOVE + PM_NOYIELD) Then
                Do
                    Sleep 0
                Loop While PeekMessage(wMsg, hwnd, 0, 0, PM_NOREMOVE + PM_NOYIELD)
            End If
        End If
    Else
        Dim nMsg As Msg
        GetWindowThreadProcessId hwnd, pId
        If (pId = lParam) And IsWindow(hwnd) Then
            If PeekMessage(nMsg, 0, 0, 0, PM_REMOVE + PM_NOYIELD) Then
                Do
                    TranslateMessage nMsg
                    DispatchMessage nMsg
                Loop While PeekMessage(nMsg, hwnd, 0, 0, PM_REMOVE + PM_NOYIELD)
            End If
            WinEvents = True
        End If
    End If
End Function


Public Sub DebugClear()
    If PathExists(AppPath & "debug.txt", True) Then Kill AppPath & "debug.txt"
End Sub

Public Sub DebugPrint(ByVal Msg As String)
#If VBIDE = -1 Then
    Dim FileNum As Integer
    FileNum = FreeFile
    Open AppPath & "debug.txt" For Append As #FileNum
        Print #FileNum, Msg
    Close #FileNum
#End If
    Debug.Print Msg
End Sub

Public Function SysPath() As String
    Dim winDir As String
    Dim Ret As Long
    winDir = String(45, Chr(0))
    Ret = GetSystemDirectory(winDir, 45)
    winDir = Trim(Replace(winDir, Chr(0), ""))
    If Right(winDir, 1) <> "\" Then winDir = winDir + "\"
    SysPath = winDir
End Function

Public Function AppPath(Optional ByVal RootEXEOf As Boolean = False) As String
    Dim nLen As String
    Dim lpTemp As String
    If RootEXEOf Then
        lpTemp = Space(256)
        nLen = GetModuleFileName(0&, lpTemp, Len(lpTemp))
        lpTemp = GetFilePath(Trim(Left(lpTemp, nLen)))
        If Right(lpTemp, 1) <> "\" Then lpTemp = lpTemp & "\"
    Else
        lpTemp = IIf((Right(App.path, 1) = "\"), App.path, App.path & "\")
    End If
#If VBIDE Then
    If LCase(Right(lpTemp, 10)) = "\projects\" Then
        lpTemp = Left(lpTemp, Len(lpTemp) - 10) & "\Binary\" 'Replace(lpTemp, "\Projects\", "\Binary\", , , vbTextCompare)
     End If
#End If
    AppPath = lpTemp
End Function

Public Function AppEXE(Optional ByVal TitleOnly As Boolean = True, Optional ByVal RootEXEOf As Boolean = False) As String

    Dim lpTemp As String
    Dim nLen As Long
    lpTemp = Space(256)
    If RootEXEOf Then
        nLen = GetModuleFileName(0&, lpTemp, Len(lpTemp))
        lpTemp = Left(lpTemp, nLen)
    Else
        nLen = GetModuleFileName(GetModuleHandle(App.EXEName), lpTemp, Len(lpTemp))
        lpTemp = Left(lpTemp, nLen)
    End If
    If TitleOnly And InStrRev(lpTemp, "\") > 0 Then
        lpTemp = Mid(lpTemp, InStrRev(lpTemp, "\") + 1)
    End If
    If TitleOnly And InStrRev(lpTemp, ".") = Len(lpTemp) - 3 Then
        lpTemp = Left(lpTemp, InStrRev(lpTemp, ".") - 1)
    End If
    AppEXE = Trim(lpTemp)
End Function

Public Function AppVersion(Optional Short As Boolean = False) As String
    If Not Short Then
        AppVersion = App.Major & "." & App.Minor & "." & App.Revision
    Else
        AppVersion = App.Major & "." & App.Minor
    End If
End Function

Public Function IsCompiled() As Boolean
    'determines the possession of the running project if whether or not
    'the active process instance is in a compiled sense of machine code
    '/d VBIDE=0, or if it is in uncompiled pass CondComp="VBIDE=-1" if
    'neither those conditions are met it still attempts to affirm by
    'way of neither 100% accuracy of sub modulated checking the EXE
    IsCompiled = True

#If VBIDE = 0 Then
    On Error GoTo NotComp
    IsCompiled = (GetFileTitle(LCase(Trim(App.EXEName))) = GetFileTitle(LCase(Trim(AppEXE(True, False)))))
    If Not IsCompiled Then
        Debug.Print 1 / 0
    End If
#Else
     IsCompiled = False
     Exit Function
#End If
    On Error GoTo NotComp
    If Not IsCompiled Then
        Debug.Print 1 / 0
    End If
    Exit Function
NotComp:
    IsCompiled = False
End Function

Public Function IsCompiler() As Boolean
    'Determine if the running executable is the development environment
    '(which also is true if when in automation on command line compiles)
    'or the running executable name of the application is not iscompiler
    If ("vb6" = LCase(AppEXE(True, True))) Then
        IsCompiler = True
    Else
        IsCompiler = False
'
'        IsDebugState = App.Title
'        Dim TmpHwnds As String
'        IsDebugHwnds = ""
'        EnumWindows AddressOf IsDebuggingWinEvents, GetCurrentProcessId
'        Do Until (IsDebugState <> App.Title) Or ((IsDebugHwnds = TmpHwnds & IsDebugHwnds) And (TmpHwnds = ""))
'            TmpHwnds = TmpHwnds & IsDebugHwnds
'            DoEvents
'            If InStr(IsDebugHwnds, TmpHwnds) > 0 Then
'                IsDebugHwnds = TmpHwnds
'                TmpHwnds = IsDebugHwnds & IsDebugHwnds
'            Else
'                TmpHwnds = ""
'            End If
'        Loop
'        IsCompiler = (IsDebugState = "TRUE")
    End If
End Function
Public Function IsDebugger(Optional ByVal AppTitle As String = "") As Boolean
    'seeks for the projects running status parent, which during running or debugging
    'is the application itself, not nested compiled modules, is in the VBIDE started
    Static Ret As Integer
    'If ret = 0 Then
        Dim nLen As String
        Dim lpTemp As String
        lpTemp = Space(256)
        nLen = GetModuleFileName(0&, lpTemp, Len(lpTemp))
        lpTemp = Left(lpTemp, nLen)
        If (InStrRev(LCase(lpTemp), "vb6.exe") > 0) Then
            Ret = -2
            If AppTitle = "" Then AppTitle = App.title
            If AppTitle <> "" Then
                IsDebugState = AppTitle & " - Microsoft Visual Basic"
                Dim TmpHwnds As String
                IsDebugHwnds = ""
                EnumWindows AddressOf IsDebuggingWinEvents, GetCurrentProcessId
                Do Until (LCase(Left(IsDebugState, Len(AppTitle))) <> LCase(AppTitle)) Or ((IsDebugHwnds = TmpHwnds & IsDebugHwnds) And (TmpHwnds = ""))
                    TmpHwnds = TmpHwnds & IsDebugHwnds
                    DoEvents
                    If InStr(IsDebugHwnds, TmpHwnds) > 0 Then
                        IsDebugHwnds = TmpHwnds
                        TmpHwnds = IsDebugHwnds & IsDebugHwnds
                    Else
                        TmpHwnds = ""
                    End If
                Loop
                Ret = CInt(CBool((IsDebugState = "TRUE"))) - 1
            End If
        Else
            Ret = -1
        End If
   'End If
    IsDebugger = CBool(Ret + 1)
End Function

'Public Function Toggler(ByRef Value As Variant, Optional ByVal Inverse As Boolean = False, Optional ByVal Whole As Long = 1) As Variant
'    Toggler = (-CInt(CBool(Value)) + -IIf(Inverse, Whole, Abs(Value))) + -CInt(Not CBool(-Value + -IIf(Inverse, Abs(Value)))) '* ((-Abs(Whole) + -Value) + (Abs(Whole) * 2)) * (Abs(Whole) - (Whole + 1))
'End Function

Public Function TimedOut(ByVal Tim As Single, ByVal sec As Integer)
    TimedOut = ((Timer - Tim) > sec)
End Function

Public Function BoolToCheck(ByVal bBool As Boolean) As Integer
    BoolToCheck = -CInt(bBool)
End Function

Public Function CheckToBool(ByVal bBool As Boolean) As Boolean
    CheckToBool = CBool(bBool)
End Function

Public Function ClearCollection(ByRef ColList, Optional ByVal IsObjects As Boolean = False, Optional ByVal SetNothing As Boolean = False)
    If Not ColList Is Nothing Then
        If IsObjects Then
            Dim obj As Object
            Do Until ColList.count = 0
                Set obj = ColList(1)
                ColList.Remove 1
                Set obj = Nothing
            Loop
        Else
            Do Until ColList.count = 0
                ColList.Remove 1
            Loop
        End If
        If SetNothing Then Set ColList = Nothing
    End If
End Function

Public Function TrimStrip(ByVal TheStr As String, ByVal TheChar As String) As String
    TrimStrip = LTrimStrip(RTrimStrip(TheStr, TheChar), TheChar)
End Function
Public Function LTrimStrip(ByVal TheStr As String, ByVal TheChar As String) As String
    Do While Left(TheStr, Len(TheChar)) = TheChar
        TheStr = Mid(TheStr, Len(TheChar) + 1)
    Loop
    LTrimStrip = TheStr
End Function
Public Function RTrimStrip(ByVal TheStr As String, ByVal TheChar As String) As String
    Do While Right(TheStr, Len(TheChar)) = TheChar
        TheStr = Left(TheStr, Len(TheStr) - Len(TheChar))
    Loop
    RTrimStrip = TheStr
End Function

Public Function NextArg(ByVal TheParams As String, ByVal TheSeperator As String, Optional ByVal Compare As VbCompareMethod = vbBinaryCompare, Optional ByVal TrimResult As Boolean = True) As String
    If TrimResult Then
        If InStr(1, TheParams, TheSeperator, Compare) > 0 Then
            NextArg = Trim(Left(TheParams, InStr(1, TheParams, TheSeperator, Compare) - 1))
        Else
            NextArg = Trim(TheParams)
        End If
    Else
        If InStr(1, TheParams, TheSeperator, Compare) > 0 Then
            NextArg = Left(TheParams, InStr(1, TheParams, TheSeperator, Compare) - 1)
        Else
            NextArg = TheParams
        End If
    End If
End Function

Public Function RemoveArg(ByVal TheParams As String, ByVal TheSeperator As String, Optional ByVal Compare As VbCompareMethod = vbBinaryCompare, Optional ByVal TrimResult As Boolean = True) As String
    If TrimResult Then
        If InStr(1, TheParams, TheSeperator, Compare) > 0 Then
            RemoveArg = Trim(Mid(TheParams, InStr(1, TheParams, TheSeperator, Compare) + Len(TheSeperator), Len(TheParams) - Len(TheSeperator)))
        Else
            RemoveArg = ""
        End If
    Else
        If InStr(1, TheParams, TheSeperator, Compare) > 0 Then
            RemoveArg = Mid(TheParams, InStr(1, TheParams, TheSeperator, Compare) + Len(TheSeperator), Len(TheParams) - Len(TheSeperator))
        Else
            RemoveArg = ""
        End If
    End If
End Function

Public Function RemoveNextArg(ByRef TheParams As Variant, ByVal TheSeperator As String, Optional ByVal Compare As VbCompareMethod = vbBinaryCompare, Optional ByVal TrimResult As Boolean = True) As String
    If TrimResult Then
        If InStr(1, TheParams, TheSeperator, Compare) > 0 Then
            RemoveNextArg = Trim(Left(TheParams, InStr(1, TheParams, TheSeperator, Compare) - 1))
            TheParams = Trim(Mid(TheParams, InStr(1, TheParams, TheSeperator, Compare) + Len(TheSeperator)))
        Else
            RemoveNextArg = Trim(TheParams)
            TheParams = ""
        End If
    Else
        If InStr(1, TheParams, TheSeperator, Compare) > 0 Then
            RemoveNextArg = Left(TheParams, InStr(1, TheParams, TheSeperator, Compare) - 1)
            TheParams = Mid(TheParams, InStr(1, TheParams, TheSeperator, Compare) + Len(TheSeperator))
        Else
            RemoveNextArg = TheParams
            TheParams = ""
        End If
    End If
End Function
Public Function NextQuotedArg(ByVal TheParams As String, Optional ByVal BeginQuote As String = """", Optional ByVal EndQuote As String = """", Optional ByVal Embeded As Boolean = False, Optional ByVal Compare As VbCompareMethod = vbBinaryCompare) As String
    NextQuotedArg = RemoveQuotedArg(TheParams, BeginQuote, EndQuote, Embeded, Compare)
End Function
Public Function RemoveQuotedArg(ByRef TheParams As String, Optional ByVal BeginQuote As String = """", Optional ByVal EndQuote As String = """", Optional ByVal Embeded As Boolean = False, Optional ByVal Compare As VbCompareMethod = vbBinaryCompare) As String
    Dim retval As String
    Dim X As Long
    X = InStr(1, TheParams, BeginQuote, Compare)
    If (X > 0) And (X < Len(TheParams)) Then
        If (InStr(X + Len(BeginQuote), TheParams, EndQuote, Compare) > 0) Then
            If (Not Embeded) Or (EndQuote = BeginQuote) Then
                retval = Mid(TheParams, X + Len(BeginQuote))
                TheParams = Left(TheParams, X - 1) & Mid(retval, InStr(1, retval, EndQuote, Compare) + Len(EndQuote))
                retval = Left(retval, InStr(1, retval, EndQuote, Compare) - 1)
            Else
                Dim L As Long
                Dim Y As Long
                L = 1
                Y = X
                Do Until L = 0
                    If (InStr(Y + Len(BeginQuote), TheParams, BeginQuote, Compare) > 0) And (InStr(Y + Len(BeginQuote), TheParams, BeginQuote, Compare) < InStr(Y + Len(BeginQuote), TheParams, EndQuote, Compare)) Then
                        L = L + 1
                        Y = InStr(Y + Len(BeginQuote), TheParams, BeginQuote, Compare)
                    ElseIf (InStr(Y + Len(BeginQuote), TheParams, EndQuote, Compare) > 0) Then
                        L = L - 1
                        Y = InStr(Y + Len(EndQuote), TheParams, EndQuote, Compare)
                    Else
                        Y = Len(TheParams)
                        L = 0
                    End If
                Loop
                retval = Mid(TheParams, X + Len(BeginQuote))
                TheParams = Left(TheParams, X - 1) & Mid(retval, (Y - X) + Len(EndQuote))
                retval = Left(retval, (Y - X) - 1)
            End If
        End If
    End If
    RemoveQuotedArg = retval
End Function

Public Function CountWord(ByVal Text As String, ByVal Word As String, Optional ByVal Exact As Boolean = True) As Long
    Dim cnt As Long
    cnt = UBound(Split(Text, Word, , IIf(Exact, vbBinaryCompare, vbTextCompare)))
    If cnt > 0 Then CountWord = cnt
End Function

Public Function WordCount(ByVal Text As String, Optional ByVal TheSeperator As String = " ", Optional ByVal Exact As Boolean = True) As Long
    Dim cnt As Long
    cnt = UBound(Split(Text, TheSeperator, , IIf(Exact, vbBinaryCompare, vbTextCompare)))
    If cnt >= 0 Then WordCount = cnt
End Function

Public Function IsAlphaNumeric(ByVal Text As String) As Boolean
    Dim cnt As Integer
    Dim C2 As Integer
    Dim retval As Boolean
    retval = True
    If Not IsNumeric(Text) Then
    If Len(Text) > 0 Then
        For cnt = 1 To Len(Text)
            If (Asc(LCase(Mid(Text, cnt, 1))) = 46) Then
                C2 = C2 + 1
            ElseIf (Not IsNumeric(Mid(Text, cnt, 1))) And (Not (Asc(LCase(Mid(Text, cnt, 1))) >= 97 And Asc(LCase(Mid(Text, cnt, 1))) <= 122)) Then
                retval = False
                Exit For
            End If
        Next
    Else
        retval = False
    End If
    Else
        retval = True
    End If
    IsAlphaNumeric = retval And (C2 <= 1)
End Function

Public Function PathExists(ByVal URL As String, Optional ByVal IsFile As Variant = Empty) As Boolean
    Dim Ret As Boolean
    
    If Left(URL, 2) = "\\" Then GoTo altcheck
    If (Len(URL) = 2) And (Mid(URL, 2, 1) = ":") Then
        URL = URL & "\"
    End If
        
    On Error GoTo altcheck

    URL = Replace(URL, "/", "\")
    If InStr(Mid(URL, 3), ":") > 0 Then
        PathExists = False
    ElseIf Len(URL) > 2 Then
        If Len(URL) <= 3 And Mid(URL, 2, 1) = ":" Then
            If VBA.TypeName(IsFile) = "Empty" Then
                PathExists = (Dir(URL, vbVolume) <> "") Or (Dir(URL & "\*") <> "")
            Else
                PathExists = ((Dir(URL, vbVolume) <> "") Or (Dir(URL & "\*") <> "")) And (Not IsFile)
            End If
        Else
            Do While Right(URL, 1) = "\"
                URL = Left(URL, Len(URL) - 1)
            Loop
            Dim attr As Long
            Dim chk1 As String
            Do
                If VBA.TypeName(IsFile) = "Empty" Then
                    chk1 = Dir(URL, attr)
                    If chk1 <> "" And Not Ret Then
                        If InStr(URL, "*") > 0 Then
                            Ret = True
                        Else
                            If Len(URL) > Len(chk1) Then
                                Ret = LCase(Right(URL, Len(chk1))) = LCase(chk1)
                            Else
                                Ret = LCase(Right(chk1, Len(URL))) = LCase(URL)
                            End If
                        End If
                    End If
                    If Not Ret Then
                        chk1 = Dir(URL, attr + vbDirectory)
                        If chk1 <> "" Then
                            If InStr(URL, "*") > 0 Then
                                Ret = True
                            Else
                                If Len(URL) > Len(chk1) Then
                                    Ret = LCase(Right(URL, Len(chk1))) = LCase(chk1)
                                Else
                                    Ret = LCase(Right(chk1, Len(URL))) = LCase(URL)
                                End If
                                If Ret Then
                                    If Not (GetAttr(URL) And vbDirectory) = vbDirectory Then
                                        Ret = False
                                    End If
                                End If
                            End If
                        End If
                    End If
                Else
                    If Not IsFile Then
                        chk1 = Dir(URL, attr + vbDirectory)
                        If chk1 <> "" And Not Ret Then
                            If InStr(URL, "*") > 0 Then
                                Ret = True
                            Else
                                If Len(URL) > Len(chk1) Then
                                    Ret = LCase(Right(URL, Len(chk1))) = LCase(chk1)
                                Else
                                    Ret = LCase(Right(chk1, Len(URL))) = LCase(URL)
                                End If
                                If Ret Then
                                    If Not (GetAttr(URL) And vbDirectory) = vbDirectory Then
                                        Ret = False
                                    End If
                                End If
                            End If
                        End If
                    Else
                        chk1 = Dir(URL, attr)
                        If chk1 <> "" And Not Ret Then
                            If InStr(URL, "*") > 0 Then
                                Ret = True
                            Else
                                If Len(URL) > Len(chk1) Then
                                    Ret = (LCase(Right(URL, Len(chk1))) = LCase(chk1))
                                Else
                                    Ret = LCase(Right(chk1, Len(URL))) = LCase(URL)
                                End If
                            End If
                        End If
                    End If
                End If
                Select Case attr
                    Case vbNormal
                        attr = vbSystem
                    Case vbSystem
                        attr = vbHidden
                    Case vbHidden
                        attr = vbReadOnly
                    Case vbReadOnly
                        attr = vbHidden + vbReadOnly
                    Case vbHidden + vbReadOnly
                        attr = vbHidden + vbSystem
                    Case vbHidden + vbSystem
                        attr = vbHidden + vbSystem + vbReadOnly
                    Case vbHidden + vbSystem + vbReadOnly
                        attr = vbSystem + vbReadOnly
                    Case vbSystem + vbReadOnly
                        attr = vbNormal
                End Select
            Loop Until Ret Or attr = vbNormal
            PathExists = Ret
        End If
    End If

    Exit Function
altcheck:

    Select Case Err.Number
        Case 55, 58, 70
            PathExists = True
        Case Else '53, 52
            Err.Clear
    End Select
'55 File already open
'58 File already exists
'70 Permission denied
'52 Bad file name or number
'53 File not found

    On Error GoTo fixthis:

    If (URL = vbNullString) Then
        PathExists = False
        Exit Function
    ElseIf (Not IsEmpty(IsFile)) Then
        If ((GetFilePath(URL) = vbNullString) And IsFile And (Not (URL = vbNullString))) Or ((GetFileName(URL) = vbNullString) And (Not IsFile) And (Not (URL = vbNullString))) Then
            PathExists = False
            Exit Function
        End If
    End If
    
    On Error GoTo 0
    On Error GoTo -1
    On Error Resume Next
    
    Dim Alt As Integer
    Alt = GetAttr(URL)
    If Err.Number = 0 Then
        If (IsEmpty(IsFile)) Then
            PathExists = True
        Else
            PathExists = IIf(IsFile, Not CBool(((Alt And vbDirectory) = vbDirectory)), CBool(((Alt And vbDirectory) = vbDirectory)))
        End If
        Exit Function
    End If
    
fixthis:
    If Err.Number <> 0 Then
        Select Case Err.Number
            Case 55, 58, 70
                PathExists = True
            Case Else
                PathExists = False
        End Select
        Err.Clear
    End If
End Function

Public Function ReadFile(ByVal path As String) As String
    Dim num As Long
    Dim Text As String
    Dim timeout As Single
    
    num = FreeFile
    On Error Resume Next
    On Local Error Resume Next
    If PathExists(path, True) Then
        Open path For Append Shared As #num Len = 1 ' LenB(Chr(CByte(0)))
        Close #num
        Select Case Err.Number
            Case 54, 70, 75
                Err.Clear
                On Error GoTo tryagain
                On Local Error GoTo tryagain
                
                Open path For Binary Access Read Lock Write As num Len = 1
                If timeout <> 0 Then
                    Open path For Binary Shared As #num Len = 1
                End If
                Text = String(LOF(num), " ")
                Get #num, 1, Text
                Close #num
            Case Else
                On Error GoTo tryagain
                On Local Error GoTo tryagain
                
                Open path For Binary Access Read As num Len = 1
                If timeout <> 0 Then
                    Open path For Binary Shared As num Len = 1
                End If
                Text = String(LOF(num), " ")
                Get #num, 1, Text
                Close #num
        End Select
        If Err Then GoTo failit
        On Error GoTo 0
        On Local Error GoTo 0
    End If
    ReadFile = Text
    Exit Function
tryagain:
    On Error GoTo tryagain
    On Local Error GoTo tryagain
    If timeout = 0 Then
        timeout = Timer
        Resume Next
    ElseIf Timer - timeout > 10 Then
        GoTo failit
    Else
        On Error GoTo failit
        Resume
    End If
failit:
    On Error GoTo 0
    On Local Error GoTo 0
    Err.Raise 75, "ReadFile"
End Function

Public Function WriteFile(ByVal path As String, ByRef Text As String) As Boolean

    If PathExists(path, True) Then
        If (GetAttr(path) And vbReadOnly) <> 0 Then Exit Function
    End If
    
    Dim timeout As Single
    Dim num As Integer
    
    On Error Resume Next
    On Local Error Resume Next
    
    num = FreeFile
    Open path For Output Shared As #num Len = 1  'Len = LenB(Chr(CByte(0)))
    Close #num
    
    Select Case Err.Number

        Case 54, 70, 75
            Err.Clear
            On Error GoTo tryagain
            On Local Error GoTo tryagain
            
            Open path For Binary Access Write Lock Read As #num Len = 1
            If timeout <> 0 Then
                Open path For Binary Shared As #num Len = 1
            End If
            Put #num, 1, Text
            Close #num
            WriteFile = True
        Case 0
            On Error GoTo tryagain
            On Local Error GoTo tryagain
            
            Open path For Binary Access Write As #num Len = 1
            If timeout <> 0 Then
                Open path For Binary Shared As #num Len = 1
            End If
            Put #num, 1, Text
            Close #num
            WriteFile = True
    End Select

    If Err Then GoTo failit
    On Error GoTo 0
    On Local Error GoTo 0
    
    Exit Function
tryagain:
    On Error GoTo tryagain
    On Local Error GoTo tryagain
    
    If timeout = 0 Then
        timeout = Timer
        Resume Next
    ElseIf timeout = -1 Then
        
    ElseIf Timer - timeout > 10 Then
        GoTo failit
    Else
        Resume
    End If
failit:
    On Error GoTo 0
    On Local Error GoTo 0
    Err.Raise 75, "WriteFile"
End Function

'Public Function ReadFile(ByVal path As String) As String
'    Dim num As Long
'    Dim Text As String
'
'    num = FreeFile
'    On Error Resume Next
'    If PathExists(path, True) Then
'        Open path For Append Shared As #num Len = 1 ' LenB(Chr(CByte(0)))
'        Close #num
'
'    End If
'
'    Select Case Err.Number
'        Case 54, 70, 75
'            Err.Clear
'            On Error GoTo 0
'            num = FreeFile
'            Open path For Binary Access Read As #num Len = 1  'Access Read Lock Write As num Len = 1 'LenB(Chr(CByte(0)))
'                Text = String(LOF(num), " ")
'                Get #num, 1, Text
'            Close #num
'        Case Else
'            On Error GoTo 0
'            num = FreeFile
'            Open path For Binary Shared As #num Len = 1 'Access Read Lock Write As num Len = 1 'LenB(Chr(CByte(0)))
'                Text = String(LOF(num), " ")
'                Get #num, 1, Text
'            Close #num
'    End Select
'
'
'    ReadFile = Text
'End Function
'
'Public Sub WriteFile(ByVal path As String, ByRef Text As String)
'    If (GetAttr(path) And vbReadOnly) = 0 Then
'        Dim num As Integer
'        num = FreeFile
'        Open path For Output Shared As #num Len = 1  'Len = LenB(Chr(CByte(0)))
'        Close #num
'        Open path For Binary Shared As #num Len = 1 'Access Write Lock Read As #num Len = 1  'Len = LenB(Chr(CByte(0)))
'            Put #num, 1, Text
'        Close #num
'    End If
'End Sub

Public Function GetFilePath(ByVal URL As String) As String
    Dim nFolder As String
    If InStr(URL, "/") > 0 Then
        nFolder = Left(URL, InStrRev(URL, "/") - 1)
        If nFolder = "" Then nFolder = "/"
    ElseIf InStr(URL, "\") > 0 Then
        nFolder = Left(URL, InStrRev(URL, "\") - 1)
        If nFolder = "" Then nFolder = "\"
    Else
        nFolder = ""
    End If
    GetFilePath = nFolder
End Function

Public Function GetFileTitle(ByVal URL As String) As String
    URL = GetFileName(URL)
    If InStrRev(URL, ".") > 0 Then
        URL = Left(URL, InStrRev(URL, ".") - 1)
    End If
    GetFileTitle = URL
End Function

Public Function GetFileName(ByVal URL As String) As String
    If InStr(URL, "/") > 0 Then
        GetFileName = Mid(URL, InStrRev(URL, "/") + 1)
    ElseIf InStr(URL, "\") > 0 Then
        GetFileName = Mid(URL, InStrRev(URL, "\") + 1)
    Else
        GetFileName = URL
    End If
End Function

Public Function GetFileExt(ByVal URL As String, Optional ByVal LowerCase As Boolean = True, Optional ByVal RemoveDot As Boolean = False) As String
    If InStrRev(URL, ".") > 0 Then
        If LowerCase Then
            GetFileExt = Trim(LCase(Mid(URL, (InStrRev(URL, ".") + -CInt(RemoveDot)))))
        Else
            GetFileExt = Mid(URL, (InStrRev(URL, ".") + -CInt(RemoveDot)))
        End If
    Else
        GetFileExt = vbNullString
    End If
End Function

Public Function GetFileDate(ByVal URL As String) As Date

    GetFileDate = FileDateTime(URL)
    
End Function
Public Function GetFileSize(ByVal URL As String) As Double
    GetFileSize = FileSize(URL)
End Function

Public Function IsFileNameValid(ByVal FileName As String) As Boolean
    Dim isValid As Boolean
    isValid = True
    If InStr(FileName, "\") > 0 Then isValid = False
    If InStr(FileName, "/") > 0 Then isValid = False
    If InStr(FileName, ":") > 0 Then isValid = False
    If InStr(FileName, "*") > 0 Then isValid = False
    If InStr(FileName, "?") > 0 Then isValid = False
    If InStr(FileName, """") > 0 Then isValid = False
    If InStr(FileName, "<") > 0 Then isValid = False
    If InStr(FileName, ">") > 0 Then isValid = False
    If InStr(FileName, "|") > 0 Then isValid = False
    IsFileNameValid = isValid
End Function

Public Function FormatFileSize(ByVal fSize As String, Optional ByVal Abbrev As Boolean = False) As String

    Dim newSize As String
    Dim cnt As Integer
    Dim numComma As Integer
    Dim trail As Integer
    newSize = fSize
    numComma = ((Len(newSize) - 2) / 3)
    cnt = 1
    Do Until cnt > numComma
        newSize = Left(newSize, Len(newSize) - ((cnt * 3) + (cnt - 1))) & "," & Right(newSize, ((cnt * 3) + (cnt - 1)))
        cnt = cnt + 1
    Loop

    FormatFileSize = Space(16 - Len(newSize)) & newSize & IIf(Abbrev, " B", " Bytes")

End Function

Public Function GetFileNumerical(ByVal FilePath As String, Optional ByVal spacer As String = " ") As String
    Dim cnt As Long
    Dim tmp As String
    
    tmp = GetFileTitle(FilePath)
    If IsNumeric(NextArg(StrReverse(tmp), StrReverse(spacer))) Then
        cnt = CLng(StrReverse(NextArg(StrReverse(tmp), StrReverse(spacer)))) + 1
        tmp = StrReverse(RemoveArg(StrReverse(tmp), StrReverse(spacer)))
    Else
        cnt = 1
    End If
    Do Until Not PathExists(GetFilePath(FilePath) & "\" & tmp & spacer & Trim(CStr(cnt)) & GetFileExt(FilePath, False), True)
        cnt = cnt + 1
    Loop
    GetFileNumerical = GetFilePath(FilePath) & "\" & tmp & spacer & Trim(CStr(cnt)) & GetFileExt(FilePath, False)
End Function

Public Function Padding(ByVal Length As Long, ByVal Value As String, Optional ByVal PadWith As String = " ") As String
    Padding = String(Abs((Length * Len(PadWith)) - (Len(Value) \ Len(PadWith))), PadWith) & Value
End Function

Public Function Toggler(ByVal Value As Long) As Long
    Toggler = (-CInt(CBool(Value)) + -1) + -CInt(Not CBool(-Value + -1))
End Function






