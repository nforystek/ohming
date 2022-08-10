Attribute VB_Name = "Module1"
#Const Module1 = -1
Option Explicit

Private Const GWL_WNDPROC = -4

Private Declare Function CallWindowProc Lib "user32" Alias "CallWindowProcA" (ByVal lpPrevWndFunc As Long, ByVal hwnd As Long, ByVal Msg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Private Declare Function DefWindowProc Lib "user32" Alias "DefWindowProcA" (ByVal hwnd As Long, ByVal wMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Private Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hwnd As Long, ByVal nIndex As Long) As Long
Private Declare Function DestroyWindow Lib "user32" (ByVal hwnd As Long) As Long

Public Static Function HookObj(ByRef obj)
    'inputs:
    '   as form     hooks and unhooks sequentially when passed a form, so
    '               in the Form_Load and Form_Unload should call Call HookObj(Me)
    '   as hwnd     passing just the hwnd handle, returns the form object
    '               passing hwnd handle as negative, returns the old GWL proc
    'returns:
    '   Form        when hwnd is parameter, the corresponding form is returned,
    '               in such a way, that use with out global declares or use of
    '               the native enviornments object references can be done in
    '               the WndProc, independantly like the WndProc exists on Form
    '   Long        when -hwnd is parameter, the prior WndProc address retruns
    '               this can also be used to check for presence of if subclassed

    Static hc As Collection
    Static ha As Collection
    If IsNumeric(obj) Then
        If hc Is Nothing And obj > 0 Then
            DestroyWindow obj
        Else
            If obj < 0 Then
                HookObj = ha("k" & -obj)
            Else
                Set HookObj = hc("k" & obj)
            End If
        End If
    Else
        If hc Is Nothing Then
            Set hc = New Collection
            Set ha = New Collection
        End If
        Dim cnt As Long
        If hc.Count > 0 Then
            For cnt = 1 To hc.Count
                If hc(cnt).hwnd = obj.hwnd Then
                    SetWindowLong obj.hwnd, _
                    GWL_WNDPROC, ha("k" & obj.hwnd)
                    hc.Remove "k" & obj.hwnd
                    ha.Remove "k" & obj.hwnd
                    GoTo hookok
                End If
            Next
        End If
        hc.Add obj, "k" & obj.hwnd
        ha.Add SetWindowLong(obj.hwnd, GWL_WNDPROC, _
        AddressOf ControlWndProc), "k" & obj.hwnd
    End If
hookok:
    If hc.Count = 0 Then
        Set hc = Nothing
        Set ha = Nothing
    End If
End Function

Private Function ControlWndProc(ByVal hwnd As Long, ByVal uMsg As Long, ByVal wParam As Long, ByVal lParam As Long) As Long
    If (HookObj(-hwnd) <> 0) Then
        Debug.Print TypeName(HookObj(hwnd)) & ", " & hwnd & ", " & uMsg & ", " & wParam & ", " & lParam
        If CallWindowProc(HookObj(-hwnd), hwnd, uMsg, wParam, lParam) = 0 Then
            ControlWndProc = 1
        Else
            ControlWndProc = DefWindowProc(hwnd, uMsg, wParam, lParam)
        End If
    End If
End Function



Public Sub Main()
    NewForm
    NewForm
    NewForm
End Sub

Private Sub NewForm()
    Dim tmp1 As Form1
    Set tmp1 = New Form1
    tmp1.Show
    Set tmp1 = Nothing
End Sub
