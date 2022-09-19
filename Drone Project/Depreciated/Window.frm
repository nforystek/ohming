VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "mscomm32.ocx"
Begin VB.Form Window 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "YAADD"
   ClientHeight    =   5055
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6810
   ClipControls    =   0   'False
   Icon            =   "Window.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5055
   ScaleWidth      =   6810
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command2 
      Caption         =   "&Close"
      Enabled         =   0   'False
      Height          =   315
      Left            =   5145
      TabIndex        =   5
      Top             =   105
      Width           =   780
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&Open"
      Default         =   -1  'True
      Height          =   315
      Left            =   4305
      TabIndex        =   4
      Top             =   105
      Width           =   780
   End
   Begin VB.ComboBox Combo2 
      Height          =   315
      Left            =   2790
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   105
      Width           =   1425
   End
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   975
      Style           =   2  'Dropdown List
      TabIndex        =   2
      Top             =   105
      Width           =   1200
   End
   Begin VB.Timer MainLoop 
      Interval        =   1
      Left            =   1455
      Top             =   1815
   End
   Begin MSCommLib.MSComm Arduino 
      Left            =   4140
      Top             =   930
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      CommPort        =   5
      DTREnable       =   -1  'True
      BaudRate        =   115200
   End
   Begin VB.PictureBox Connecting 
      Height          =   4455
      Left            =   90
      Picture         =   "Window.frx":0442
      ScaleHeight     =   4395
      ScaleWidth      =   6555
      TabIndex        =   9
      Top             =   510
      Width           =   6615
      Begin VB.Label Label3 
         BackStyle       =   0  'Transparent
         Caption         =   "Connecting..."
         BeginProperty Font 
            Name            =   "Lucida Console"
            Size            =   14.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   2145
         TabIndex        =   10
         Top             =   2910
         Width           =   2235
      End
   End
   Begin VB.PictureBox Controller 
      Height          =   4455
      Left            =   60
      MouseIcon       =   "Window.frx":5F59C
      Picture         =   "Window.frx":5F6EE
      ScaleHeight     =   4395
      ScaleWidth      =   6585
      TabIndex        =   11
      Top             =   510
      Width           =   6645
      Begin VB.Image Direction 
         Height          =   390
         Left            =   2925
         MouseIcon       =   "Window.frx":BE848
         Picture         =   "Window.frx":BE99A
         Top             =   1875
         Width           =   450
      End
      Begin VB.Image Altitude 
         Height          =   420
         Left            =   5880
         MouseIcon       =   "Window.frx":BED40
         Picture         =   "Window.frx":BEE92
         Top             =   1875
         Width           =   450
      End
      Begin VB.Image Image2 
         Height          =   420
         Left            =   5880
         MouseIcon       =   "Window.frx":BF228
         Picture         =   "Window.frx":BF37A
         Top             =   1875
         Width           =   450
      End
      Begin VB.Image Image1 
         Height          =   570
         Left            =   2865
         MouseIcon       =   "Window.frx":BF70A
         Picture         =   "Window.frx":BF85C
         Top             =   1800
         Width           =   570
      End
   End
   Begin VB.CommandButton SendButton 
      Caption         =   "&Send"
      Enabled         =   0   'False
      Height          =   345
      Left            =   6015
      TabIndex        =   1
      Top             =   4560
      Visible         =   0   'False
      Width           =   660
   End
   Begin VB.TextBox CommandText 
      Enabled         =   0   'False
      Height          =   360
      Left            =   120
      TabIndex        =   0
      Top             =   4560
      Visible         =   0   'False
      Width           =   5880
   End
   Begin VB.TextBox ConsoleText 
      Enabled         =   0   'False
      Height          =   3990
      Left            =   105
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   6
      Top             =   525
      Visible         =   0   'False
      Width           =   6570
   End
   Begin VB.Label Label2 
      Caption         =   "Baud"
      Height          =   255
      Left            =   2250
      TabIndex        =   8
      Top             =   150
      Width           =   735
   End
   Begin VB.Label Label1 
      Caption         =   "Port"
      Height          =   255
      Left            =   570
      TabIndex        =   7
      Top             =   150
      Width           =   600
   End
End
Attribute VB_Name = "Window"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Const MaxPacketSize = 200
Private Mode As Integer
Private data As String
Private ReSetting As Boolean

Private ALTX As Single
Private ALTY As Single
Private DIRX As Single
Private DIRY As Single

Private Type POINTAPI
        x As Long
        y As Long
End Type

Private Declare Function SetCursorPos Lib "user32" (ByVal x As Long, ByVal y As Long) As Long
Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long

Private Sub EnableForm()
    Combo1.Enabled = (Not Arduino.PortOpen)
    Combo2.Enabled = (Not Arduino.PortOpen)
    Command2.Enabled = Arduino.PortOpen
    Command1.Enabled = (Not Arduino.PortOpen)
    ConsoleText.Enabled = Arduino.PortOpen
    CommandText.Enabled = Arduino.PortOpen
    SendButton.Enabled = Arduino.PortOpen

    Label3.Visible = Arduino.PortOpen
    If (Not Arduino.PortOpen) And (Not ConsoleText.Visible) Then
        Connecting.Visible = True
    End If
    Form_Activate
End Sub


Private Sub Altitude_Click()
    ToggleHandler
End Sub

Private Sub Combo1_Click()
    SaveSetting App.ProductName, "Settings", "CommPort", Combo1.ListIndex
End Sub

Private Sub Combo1_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyHandler KeyCode
End Sub

Private Sub Combo2_Click()
    SaveSetting App.ProductName, "Settings", "BaudRate", Combo2.ListIndex
End Sub

Private Sub Combo2_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyHandler KeyCode
End Sub

Private Sub Command1_Click()
    PortOpen
End Sub

Private Sub Command2_Click()
    PortClose
End Sub

Private Sub CommandText_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyHandler KeyCode
End Sub

Private Sub ToggleConsole()
    If Controller.Visible Then
        Controller.Visible = False
        Connecting.Visible = False
        ConsoleText.Visible = True
        SendButton.Visible = True
        CommandText.Visible = True

    Else
        Controller.Visible = True
        Connecting.Visible = Not (Mode = -1)
        ConsoleText.Visible = False
        SendButton.Visible = False
        CommandText.Visible = False

    End If
    Form_Activate
End Sub

Private Sub ToggleHandler()
    Dim inMode As Boolean
    
    If Controller.Visible Then
        If Screen.MousePointer = 99 Then
            inMode = True
        End If
        inMode = Not inMode
    End If
    
    If inMode Then
        Screen.MousePointer = 99
        MoveMouse
    Else

        Screen.MousePointer = 0
    End If
    
    Form_Activate
End Sub
Private Sub Connecting_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyHandler KeyCode
End Sub

Private Sub ConsoleText_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyHandler KeyCode
End Sub

Private Sub Controller_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyHandler KeyCode
End Sub

Private Sub KeyHandler(ByVal KeyCode As Integer)
    If KeyCode = 192 Then
        KeyCode = 0
        ToggleConsole
    ElseIf KeyCode = 27 Then
        ToggleHandler
    End If
End Sub


Private Sub MoveMouse()
    Static cancel As Boolean
    If Not cancel Then
    
        If Screen.MousePointer = 99 Then
    
            Dim pt As POINTAPI
            GetCursorPos pt
            
            Dim x As Single
            Dim y As Single
            x = (Me.Left + Controller.Left + (Controller.Width / 2)) / Screen.TwipsPerPixelX
            y = (Me.Top + Controller.Top + (Controller.Height / 2)) / Screen.TwipsPerPixelY
            
            Direction.Left = Direction.Left - (x - pt.x)
            Direction.Top = Direction.Top - (y - pt.y)
            
            cancel = True
            SetCursorPos x, y
            cancel = False
            
        End If
    End If
End Sub


Private Sub Controller_Click()
    ToggleHandler
End Sub


Private Sub Direction_Click()
    ToggleHandler
End Sub

Private Sub Form_Activate()
    If Connecting.Visible Then
        Connecting.SetFocus
    ElseIf CommandText.Visible And CommandText.Enabled Then
        CommandText.SetFocus
    ElseIf Controller.Visible Then
        Controller.SetFocus
    End If
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    KeyHandler KeyCode
End Sub


Private Sub Image1_Click()
    ToggleHandler
End Sub

Private Sub Image2_Click()
    ToggleHandler
End Sub

Private Sub SendButton_Click()

    DataSend CommandText.Text

    CommandText.Text = ""
    CommandText.SetFocus
End Sub

Private Sub DebugText(ByVal data As String)
    ConsoleText.Text = ConsoleText.Text & data
    ConsoleText_Change
End Sub

Private Sub Form_Unload(cancel As Integer)
    HookObj Controller
    'End
End Sub

Private Sub ConsoleText_Change()
    ConsoleText.SelStart = Len(ConsoleText.Text)
End Sub

Private Sub CommandText_KeyPress(KeyAscii As Integer)
    If KeyAscii = 13 Then
        SendButton_Click
    End If
End Sub


'####################################################
'############### FASTCODE VERSION ###################
'####### uses extra variable J and calls less #######

'Public Function Numerics(ByVal num As Variant) As Variant
'    Dim i As Long
'    Dim mul As Long
'    If TypeName(num) = "String" Then
'        Numerics = CLng(0)
'        For i = 1 To Len(num)
'            mul = 10 ^ (Len(num) - i)
'            Select Case Mid(num, i, 1)
'                Case "1"
'                    Numerics = Numerics + (mul * 1)
'                Case "2"
'                    Numerics = Numerics + (mul * 2)
'                Case "3"
'                    Numerics = Numerics + (mul * 3)
'                Case "4"
'                    Numerics = Numerics + (mul * 4)
'                Case "5"
'                    Numerics = Numerics + (mul * 5)
'                Case "6"
'                    Numerics = Numerics + (mul * 6)
'                Case "7"
'                    Numerics = Numerics + (mul * 7)
'                Case "8"
'                    Numerics = Numerics + (mul * 8)
'                Case "9"
'                    Numerics = Numerics + (mul * 9)
'            End Select
'        Next
'        Numerics = Numerics
'    Else
'        Numerics = ""
'        mul = 1
'        Do Until ((num / mul) - (num \ mul)) = (num / mul)
'            mul = mul * 10
'            i = i + 1
'        Loop
'        Dim j As Long
'        j = i
'        Do
'            mul = mul \ 10
'            i = 9
'            Do
'                mul = ((10 ^ j) * i)
'                i = i - 1
'            Loop Until ((num \ mul) = 1) Or (i = 0)
'            If (num \ mul) = 1 Then
'                Select Case (mul \ (10 ^ j))
'                    Case 1
'                        Numerics = Numerics & "1"
'                    Case 2
'                        Numerics = Numerics & "2"
'                    Case 3
'                        Numerics = Numerics & "3"
'                    Case 4
'                        Numerics = Numerics & "4"
'                    Case 5
'                        Numerics = Numerics & "5"
'                    Case 6
'                        Numerics = Numerics & "6"
'                    Case 7
'                        Numerics = Numerics & "7"
'                    Case 8
'                        Numerics = Numerics & "8"
'                    Case 9
'                        Numerics = Numerics & "9"
'                End Select
'                num = num - mul
'                If j > 0 >= 1 Then
'                    mul = mul / (mul \ j)
'                End If
'            Else
'                Numerics = Numerics & "0"
'            End If
'            j = j - 1
'        Loop Until j = -1
'        Numerics = Numerics
'    End If
'End Function

'####################################################
'################ SMALL CODE VERSION ################
'### No extra j variable but extra len/teim calls ###

Public Function Numerics(ByVal num As Variant) As Variant
    Dim i As Long
    Dim mul As Long
    If TypeName(num) = "String" Then
        Numerics = CLng(0)
        For i = 1 To Len(num)
            mul = 10 ^ (Len(num) - i)
            Select Case Mid(num, i, 1)
                Case "1"
                    Numerics = Numerics + (mul * 1)
                Case "2"
                    Numerics = Numerics + (mul * 2)
                Case "3"
                    Numerics = Numerics + (mul * 3)
                Case "4"
                    Numerics = Numerics + (mul * 4)
                Case "5"
                    Numerics = Numerics + (mul * 5)
                Case "6"
                    Numerics = Numerics + (mul * 6)
                Case "7"
                    Numerics = Numerics + (mul * 7)
                Case "8"
                    Numerics = Numerics + (mul * 8)
                Case "9"
                    Numerics = Numerics + (mul * 9)
            End Select
        Next
        Numerics = Numerics
    Else
        Numerics = ""
        mul = 1
        Do Until ((num / mul) - (num \ mul)) = (num / mul)
            mul = mul * 10
            i = i + 1
        Loop
        Numerics = String(i, " ")
        Do
            mul = mul \ 10
            i = 9
            Do
                mul = ((10 ^ (((Len(Numerics) - 1) - (Len(Trim(Numerics)) + 1)) + 1)) * i)
                i = i - 1
            Loop Until ((num \ mul) = 1) Or (i = 0)
            If (num \ mul) = 1 Then
                Select Case (mul \ (10 ^ (((Len(Numerics) - 1) - (Len(Trim(Numerics)) + 1)) + 1)))
                    Case 1
                        Numerics = Mid(Numerics, 2) & "1"
                    Case 2
                        Numerics = Mid(Numerics, 2) & "2"
                    Case 3
                        Numerics = Mid(Numerics, 2) & "3"
                    Case 4
                        Numerics = Mid(Numerics, 2) & "4"
                    Case 5
                        Numerics = Mid(Numerics, 2) & "5"
                    Case 6
                        Numerics = Mid(Numerics, 2) & "6"
                    Case 7
                        Numerics = Mid(Numerics, 2) & "7"
                    Case 8
                        Numerics = Mid(Numerics, 2) & "8"
                    Case 9
                        Numerics = Mid(Numerics, 2) & "9"
                End Select
                num = num - mul
                If (10 ^ (((Len(Numerics) - 1) - (Len(Trim(Numerics)) + 1)) + 1)) > 0 >= 1 Then
                    mul = mul / (mul \ (((Len(Numerics) - 1) - (Len(Trim(Numerics)) + 1)) + 1))
                End If
            Else
                Numerics = Mid(Numerics, 2) & "0"
            End If
        Loop Until InStr(Numerics, " ") = 0
        Numerics = Numerics
    End If
End Function



Private Sub Form_Load()
    ALTX = Altitude.Left
    ALTY = Altitude.Top
    DIRX = Direction.Left
    DIRY = Direction.Top
    
    EnableForm
    
    Combo1.AddItem "COMM1"
    Combo1.AddItem "COMM2"
    Combo1.AddItem "COMM3"
    Combo1.AddItem "COMM4"
    Combo1.AddItem "COMM5"
    Combo1.AddItem "COMM6"
    Combo1.AddItem "COMM7"
    Combo1.AddItem "COMM8"
    Combo1.AddItem "COMM9"
    Combo1.AddItem "COMM10"
    Combo1.AddItem "COMM11"
    Combo1.AddItem "COMM12"
    Combo1.AddItem "COMM13"
    Combo1.AddItem "COMM14"
    Combo1.AddItem "COMM15"
    
    Combo2.AddItem "75"
    Combo2.AddItem "110"
    Combo2.AddItem "134"
    Combo2.AddItem "150"
    Combo2.AddItem "300"
    Combo2.AddItem "600"
    Combo2.AddItem "1200"
    Combo2.AddItem "1800"
    Combo2.AddItem "2000"
    Combo2.AddItem "4800"
    Combo2.AddItem "7200"
    Combo2.AddItem "9600"
    Combo2.AddItem "14400"
    Combo2.AddItem "19200"
    Combo2.AddItem "38400"
    Combo2.AddItem "57600"
    Combo2.AddItem "115200"
    Combo2.AddItem "128000"

    Combo1.ListIndex = GetSetting(App.ProductName, "Settings", "CommPort", 0)
    Combo2.ListIndex = GetSetting(App.ProductName, "Settings", "BaudRate", 11)
    
    HookObj Controller
        
    MainLoop.Tag = 0
    Set Screen.MouseIcon = Controller.MouseIcon
    
    Me.Show
    Form_Activate
    
   ' If GetSetting(App.ProductName, "Settings", "OpenOnStart", False) Then PortOpen
End Sub

Private Sub Form_QueryUnload(cancel As Integer, UnloadMode As Integer)

    SaveSetting App.ProductName, "Settings", "OpenOnStart", Arduino.PortOpen
    PortClose
End Sub

Private Function PortOpen() As Boolean
    On Error GoTo errorcatch

    PortClose
    Do Until ATCmd = ""
    Loop
    Mode = 0
    ReSetting = False
    Label3.Caption = "Connecting."
    MainLoop.Tag = 0
    If (Not CommandText.Visible) Then Connecting.Visible = True
    If Not Arduino.PortOpen Then
        Arduino.CommPort = (Combo1.ListIndex + 1)
        Arduino.Settings = Combo2.List(Combo2.ListIndex) & ",N,8,1"
        Arduino.InputMode = comInputModeBinary
        Arduino.InputLen = 64
        Arduino.InBufferSize = 64
        Arduino.OutBufferSize = 64

        DataSend "AT"

        Arduino.PortOpen = True
    End If
    
    PortOpen = Arduino.PortOpen
        
    EnableForm
    Exit Function
errorcatch:
    PortOpen = False

    DebugText "Error: " & Err.Description
    Label3.Caption = "Port error."

    Err.Clear
    
End Function


Private Sub PortClose()
    If Arduino.PortOpen Then
        Arduino.PortOpen = False
    End If
    EnableForm
End Sub

Private Function DataSend(Optional ByVal Text As String = "") As String

    Static out As String
    If Text <> "" Then

        Do While Text <> ""
            If Len(Text) > MaxPacketSize Then
                out = out & Chr(MaxPacketSize) & Left(Text, MaxPacketSize)
                Text = Mid(Text, (MaxPacketSize + 1))
            Else
                out = out & Chr(Len(Text)) & Text
                Text = ""
            End If
        Loop

    ElseIf out <> "" Then
        
        If Len(out) > MaxPacketSize Then
            DataSend = Left(out, MaxPacketSize)
            out = Mid(out, MaxPacketSize + 1)
        Else
            DataSend = out
            out = ""
        End If
    Else
        DataSend = ""
    End If

End Function



Private Function DataMode(ByVal Text As String) As Boolean
    Text = Replace(Text, vbCrLf, "")
    DebugText Now & " " & Text & vbCrLf
    
    Select Case LCase(Text)

        Case "ip"
            If Mode = 0 Then
                ReSetting = False
                Mode = -1
            End If

            Connecting.Visible = False
        Case "ok", "ready", "wifi got ip", "at", ">"
gotready:
            Select Case Mode
                Case 0
                    ReSetting = True
                    ATCmd "AT+RST"
                Case 2
                    ATCmd "AT+CWMODE=1"
                Case 3
                    ATCmd "AT+CWJAP=""YAADD"",""10A6BC65DA"""
                Case 5
                    ATCmd "AT+CIPSTART=""TCP"",""192.168.4.1"",333"
                Case 6
                    ATCmd "AT+CIPMODE=1"
                Case 7
                    ATCmd "AT+CIPSEND"
                Case 9
                    Mode = -1
                    ReSetting = False
                    DataSend "AT"
                Case -1
                    Mode = Mode - 1
                    DataMode = True
            End Select
            Mode = Mode + 1
        Case "fail"
            PortClose
            PortOpen
            
        Case "error"
            Mode = Mode - 1
            GoTo gotready
        Case Else
            If InStr(LCase(Text), "ready") > 0 And Mode = 1 Then
                GoTo gotready
            Else
                
            
            End If
    End Select
    MainLoop.Tag = 0
End Function
Private Function ATCmd(Optional ByVal Text As String = "") As String
    Static out As String
    If Text <> "" Then
        If Right(Text, 2) <> vbCrLf Then Text = Text & vbCrLf
        out = out & Text
        Exit Function
    ElseIf out = "" Then
        out = DataSend("")
    End If
    If out <> "" Then
        If Len(out) > MaxPacketSize Then
            ATCmd = Left(out, MaxPacketSize)
            out = Mid(out, MaxPacketSize + 1)
        Else
            ATCmd = out
            out = ""
        End If
    End If
End Function

Private Function ATProcess(Optional ByVal Movements As String = "") As Boolean
    
    If Arduino.PortOpen Then
           
        Dim recv As String
        
        data = data & modCommon.Convert(Arduino.Input)
        If data <> "" Then
            If (Not ReSetting) Then
                If Asc(Left(data, 1)) <= (Len(data) - 1) Then
                    DataMode Mid(Left(data, Asc(Left(data, 1)) + 1), 2) & vbCrLf
                    data = Mid(data, Asc(Left(data, 1)) + 2)
                End If
            Else
                Do While data <> ""
                    DataMode RemoveNextArg(data, vbCrLf)
                Loop

            End If
        End If
        
        If Arduino.InBufferCount = 0 And Arduino.PortOpen Then
        
            Static outdata As String
            outdata = ATCmd
            If outdata = "" Then outdata = Movements
                
            If outdata <> "" Then

                ATProcess = True
                Do Until outdata = ""
                    Arduino.Output = modCommon.Convert(Left(outdata, Arduino.OutBufferSize))
                    outdata = Mid(outdata, Arduino.OutBufferSize + 1)
                    Do Until Arduino.OutBufferCount = 0
                        DoEvents
                    Loop
                Loop


            End If

        End If

    End If
End Function

Private Function Overlap(ByRef Obj1 As Control, ByRef Obj2 As Control) As Boolean
    Overlap = ((Obj1.Left >= Obj2.Left And Obj1.Left <= Obj2.Left + Obj2.Width) Or (Obj1.Left + Obj1.Width >= Obj2.Left And Obj1.Left + Obj1.Width <= Obj2.Left + Obj2.Width)) And _
                ((Obj1.Top >= Obj2.Top And Obj1.Top <= Obj2.Top + Obj2.Height) Or (Obj1.Top + Obj1.Height >= Obj2.Top And Obj1.Top + Obj1.Height <= Obj2.Top + Obj2.Height))
End Function


Private Sub MainLoop_Timer()

    Dim movement As String
    
    If Connecting.Visible Then

        If MainLoop.Tag >= 30 Then
            Label3.Caption = "Timed out."
        Else
            Static elapse As Single
            If elapse = 0 Or (Timer - elapse) > 1 Then
                elapse = Timer
                Select Case Label3.Caption
                    Case "Connecting..."
                        Label3.Caption = "Connecting."
                    Case "Connecting."
                        Label3.Caption = "Connecting.."
                    Case "Connecting.."
                        Label3.Caption = "Connecting..."
                End Select
                MainLoop.Tag = MainLoop.Tag + 1
            End If
        End If
    ElseIf (Controller.Visible And (Not ConsoleText.Visible)) Then
        
'        If Overlap(Altitude, Image2) Then
'            If Altitude.Top <> ALTY Then
'                If Altitude.Top > ALTY Then
'                    Altitude.Top = Altitude.Top - Screen.TwipsPerPixelY
'                Else
'                    Altitude.Top = Altitude.Top + Screen.TwipsPerPixelY
'                End If
'            End If
'
'            If Altitude.Left <> ALTX Then
'                If Altitude.Left > ALTX Then
'                    Altitude.Left = Altitude.Left - Screen.TwipsPerPixelX
'                Else
'                    Altitude.Left = Altitude.Left + Screen.TwipsPerPixelX
'                End If
'            End If
'        End If
'        If Abs(Altitude.Top - ALTY) < Screen.TwipsPerPixelY Then
'            Altitude.Top = ALTY
'        End If
'        If Abs(Altitude.Left - ALTX) < Screen.TwipsPerPixelX Then
'            Altitude.Left = ALTX
'        End If
                
        If Overlap(Direction, Image1) Then
            If Direction.Top <> DIRY Then
                If Direction.Top > DIRY Then
                    Direction.Top = Direction.Top - Screen.TwipsPerPixelY
                Else
                    Direction.Top = Direction.Top + Screen.TwipsPerPixelY
                End If
                
            End If
        
            If Direction.Left <> DIRX Then
                If Direction.Left > DIRX Then
                    Direction.Left = Direction.Left - Screen.TwipsPerPixelX
                Else
                    Direction.Left = Direction.Left + Screen.TwipsPerPixelX
                End If
            End If
        End If
        If Abs(Direction.Top - DIRY) < Screen.TwipsPerPixelY Then
            Direction.Top = DIRY
        End If
        If Abs(Direction.Left - DIRX) < Screen.TwipsPerPixelX Then
            Direction.Left = DIRX
        End If
        
        If Direction.Left < 0 Then Direction.Left = 0
        If Direction.Left + Direction.Width > Controller.Width Then Direction.Left = Controller.Width - Direction.Width
    
        If Direction.Top < 0 Then Direction.Top = 0
        If Direction.Top + Direction.Height > Controller.Height Then Direction.Top = Controller.Height - Direction.Height
        
        
        If Altitude.Left < 0 Then Altitude.Left = 0
        If Altitude.Left + Altitude.Width > Controller.Width Then Altitude.Left = Controller.Width - Altitude.Width

        If Altitude.Top < 0 Then Altitude.Top = 0
        If Altitude.Top + Altitude.Height > Controller.Height Then Altitude.Top = Controller.Height - Altitude.Height
        
        MoveMouse
        
        Dim cX1 As Single
        Dim cY1 As Single
        Dim cX2 As Single
        Dim cY2 As Single
        
        cX1 = (Direction.Left + (Direction.Width / 2))
        cY1 = (Direction.Top + (Direction.Height / 2))
    
        cX2 = (Image1.Left + (Image1.Width / 2))
        cY2 = (Image1.Top + (Image1.Height / 2))
        
        
        movement = "[" & CStr(Angle(cX1, cY1, cX2, cY2)) & "," & CStr(Accelerate(Distance(cX1, cY1, cX2, cY2), (Controller.Height / 2)))
    
        cX1 = Altitude.Left + (Altitude.Width / 2)
        cY1 = Altitude.Top + (Altitude.Height / 2)
    
        cX2 = (Image2.Left + (Image2.Width / 2))
        cY2 = (Image2.Top + (Image2.Height / 2))
        
        movement = movement & "," & CStr(Accelerate(Distance(cX1, cY1, cX2, cY2), (Controller.Height / 3))) & "]"
        
        'Debug.Print movement
    
    End If

    ATProcess movement
        
End Sub
Private Function Accelerate(ByVal Dist As Single, ByVal MaxUnit As Single) As Single
    Accelerate = Dist * (100 / MaxUnit)
    If Accelerate > 100 Then
        Accelerate = 100
    ElseIf Accelerate < 1 Then
        Accelerate = 0
    Else
        Accelerate = Round(Accelerate, 0)
    End If
End Function
Private Function Distance(ByVal cX1 As Single, ByVal cY1 As Single, ByVal cX2 As Single, ByVal cY2 As Single) As Single
    Distance = Sqr(((cX2 - cX1) ^ 2) + ((cY2 - cY1) ^ 2))
End Function
Private Function Large(ByVal val1 As Single, ByVal val2 As Single) As Single
    If val1 > val2 Then
        Large = val1
    Else
        Large = val2
    End If
End Function
Private Function Least(ByVal val1 As Single, ByVal val2 As Single) As Single
    If val1 < val2 Then
        Least = val1
    Else
        Least = val2
    End If
End Function
Private Function Angle(ByVal cX1 As Single, ByVal cY1 As Single, ByVal cX2 As Single, ByVal cY2 As Single) As Single

    ' Calc angle.
    Select Case cY2
        Case Is < cY1   ' Calc angle above base line.
            Angle = 90 + Atn((cX2 - cX1) / (cY2 - cY1)) * (180 / (4 * Atn(1)))
        Case Is > cY1    ' Calc angle below base line.
            Angle = 270 + Atn((cX2 - cX1) / (cY2 - cY1)) * (180 / (4 * Atn(1)))
        Case cY1         ' Calc angle on base line.
            If cX2 < cX1 Then Angle = 180 Else Angle = 0
    End Select

    Angle = Round(Angle, 0)

End Function
Public Sub MouseWheel(ByVal UpOrDown As Boolean)
    If UpOrDown Then

        Altitude.Top = Altitude.Top - (Screen.TwipsPerPixelY * 5)
    
    Else

        Altitude.Top = Altitude.Top + (Screen.TwipsPerPixelY * 5)
    
    End If

End Sub
