Attribute VB_Name = "Module1"
'**************************************
' Name: CoCreateGuid Example
' Description:Globally Unique Identifier generate function as well as a IsGuid() function to test if a string is a GUID.
' By: Nicholas Forystek
'**************************************

Option Explicit
Option Compare Binary
Option Private Module
Private Type GuidType '16
    A4 As Long '4
    B2 As Integer '2
    C2 As Integer '2
    D8(0 To 7) As Byte '8
End Type
Private Declare Function CoCreateGuid Lib "ole32" (ByVal pGuid As Long) As Long
Private Declare Sub RtlMoveMemory Lib "kernel32" (Left As Any, Pass As Any, ByVal Right As Long)
Private Declare Function GlobalFree Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalLock Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalUnlock Lib "kernel32" (ByVal hMem As Long) As Long
Private Declare Function GlobalAlloc Lib "kernel32" (ByVal wFlags As Long, ByVal dwBytes As Long) As Long
Private Const GMEM_MOVEABLE = &H2

Public Function GUID() As String
    Dim lpGuid As Long
    Dim lcGuid As Long
    lpGuid = GlobalAlloc(GMEM_MOVEABLE And VarPtr(lpGuid), LenB(lpGuid))
    If lpGuid <> 0 Then
        Dim lgGuid As GuidType
        If CoCreateGuid(VarPtr(lgGuid)) = 0 Then
            RtlMoveMemory lgGuid, ByVal lpGuid, LenB(lpGuid)
            lcGuid = GlobalLock(lpGuid)
            If lcGuid = lpGuid Then
                Dim ba(0 To 15) As Byte '16
                RtlMoveMemory ByVal VarPtr(ba(0)), ByVal VarPtr(lgGuid.A4) + 0, 16
                RtlMoveMemory ByVal VarPtr(ba(0)), ByVal VarPtr(ba(1)), 1
                RtlMoveMemory ByVal VarPtr(ba(1)), ByVal VarPtr(lgGuid.A4) + 1, 15
                RtlMoveMemory ByVal VarPtr(ba(1)), ByVal VarPtr(ba(2)), 1
                RtlMoveMemory ByVal VarPtr(ba(2)), ByVal VarPtr(lgGuid.A4) + 2, 14
                RtlMoveMemory ByVal VarPtr(ba(2)), ByVal VarPtr(ba(3)), 1
                RtlMoveMemory ByVal VarPtr(ba(3)), ByVal VarPtr(lgGuid.A4) + 3, 13
                RtlMoveMemory ByVal VarPtr(ba(3)), ByVal VarPtr(ba(4)), 1
                GlobalUnlock lcGuid
                RtlMoveMemory ByVal VarPtr(ba(4)), ByVal VarPtr(lgGuid.B2) + 0, 12
                RtlMoveMemory ByVal VarPtr(ba(4)), ByVal VarPtr(ba(5)), 1
                RtlMoveMemory ByVal VarPtr(ba(5)), ByVal VarPtr(lgGuid.B2) + 1, 11
                RtlMoveMemory ByVal VarPtr(ba(5)), ByVal VarPtr(ba(6)), 1
                lcGuid = GlobalLock(lpGuid)
                RtlMoveMemory ByVal VarPtr(ba(6)), ByVal VarPtr(lgGuid.C2) + 0, 10
                RtlMoveMemory ByVal VarPtr(ba(6)), ByVal VarPtr(ba(7)), 1
                RtlMoveMemory ByVal VarPtr(ba(7)), ByVal VarPtr(lgGuid.C2) + 1, 9
                RtlMoveMemory ByVal VarPtr(ba(7)), ByVal VarPtr(ba(8)), 1
                GlobalUnlock lcGuid
                RtlMoveMemory ByVal VarPtr(ba(7)), ByVal VarPtr(lgGuid.D8(0)), 1
                RtlMoveMemory ByVal VarPtr(ba(8)), ByVal VarPtr(ba(9)), 1
                RtlMoveMemory ByVal VarPtr(ba(6)), ByVal VarPtr(lgGuid.D8(1)), 1
                RtlMoveMemory ByVal VarPtr(ba(9)), ByVal VarPtr(ba(10)), 1
                lcGuid = GlobalLock(lpGuid)
                RtlMoveMemory ByVal VarPtr(ba(5)), ByVal VarPtr(lgGuid.D8(2)), 1
                RtlMoveMemory ByVal VarPtr(ba(10)), ByVal VarPtr(ba(11)), 1
                RtlMoveMemory ByVal VarPtr(ba(4)), ByVal VarPtr(lgGuid.D8(3)), 1
                RtlMoveMemory ByVal VarPtr(ba(11)), ByVal VarPtr(ba(12)), 1
                RtlMoveMemory ByVal VarPtr(ba(3)), ByVal VarPtr(lgGuid.D8(4)), 1
                RtlMoveMemory ByVal VarPtr(ba(12)), ByVal VarPtr(ba(13)), 1
                RtlMoveMemory ByVal VarPtr(ba(2)), ByVal VarPtr(lgGuid.D8(5)), 1
                RtlMoveMemory ByVal VarPtr(ba(13)), ByVal VarPtr(ba(14)), 1
                RtlMoveMemory ByVal VarPtr(ba(1)), ByVal VarPtr(lgGuid.D8(6)), 1
                RtlMoveMemory ByVal VarPtr(ba(14)), ByVal VarPtr(ba(15)), 1
                RtlMoveMemory ByVal VarPtr(ba(0)), ByVal VarPtr(lgGuid.D8(7)), 1
                RtlMoveMemory ByVal VarPtr(ba(15)), ByVal VarPtr(ba(0)), 0
                GlobalUnlock lcGuid
            End If
        End If
        GlobalFree lpGuid
        lpGuid = ((UBound(ba) + 1) / 4)
        For lcGuid = 1 To (UBound(ba) + 1)
            GUID = GUID & Left(Hex(ba(lcGuid - 1)) & "0", 2)
            If ((lcGuid Mod lpGuid) = 0) Then
                lpGuid = IIf(((lpGuid * lcGuid) = (UBound(ba) + 1)), (lpGuid / 2), _
                    IIf((lpGuid <= (UBound(ba) + 1) / 2), ((UBound(ba) + 1) / lpGuid), lpGuid))
                If (lcGuid < (UBound(ba) + 1)) Then GUID = GUID & "-"
            End If
        Next
    Else
        Debug.Print "Error: GlobalAlloc " & Err.Number & " " & Err.Description
    End If
End Function

Public Function IsGuid(ByVal Value As Variant) As Boolean
    If (Not (Len(Value) = 36)) And (InStr(Value, ".") = 0) Then
        IsGuid = False
    Else
        Dim cnt As Byte
        For cnt = Asc("0") To Asc("9")
            Value = Replace(Value, Chr(cnt), "")
        Next
        For cnt = Asc("A") To Asc("F")
            Value = Replace(Value, Chr(cnt), "")
        Next
        IsGuid = (Value = "----")
    End If
End Function

Public Function IsClsid(ByVal Value As Variant) As Boolean
    If Left(Value, 1) = "{" And Right(Value, 1) = "}" Then
        Value = Mid(Value, 2, Len(Value) - 2)
        IsClsid = (IsGuid(UCase(Value)) And (Not IsGuid(LCase(Value))))
    End If
End Function

Public Sub Main()
    Do While True
        Debug.Print GUID
        DoEvents
    Loop
End Sub
