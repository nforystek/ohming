Attribute VB_Name = "modMain"
#Const modMain = -1
Option Explicit

Option Compare Binary

Public Declare Sub Sleep Lib "kernel32" (ByVal ms As Long)


Public Sub Main()
    
    Load frmMain

    
    frmMain.Show
    

End Sub

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
