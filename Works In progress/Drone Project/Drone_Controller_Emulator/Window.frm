VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Begin VB.Form Window 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Drone Controller"
   ClientHeight    =   1920
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   3750
   ClipControls    =   0   'False
   Icon            =   "Window.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   128
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   250
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton ConnectButton 
      Caption         =   "Open"
      Height          =   345
      Left            =   2010
      TabIndex        =   1
      Top             =   105
      Width           =   705
   End
   Begin VB.ComboBox CommPort 
      Height          =   315
      ItemData        =   "Window.frx":0442
      Left            =   900
      List            =   "Window.frx":0476
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   120
      Width           =   1050
   End
   Begin VB.Timer MainLoop 
      Interval        =   1
      Left            =   3150
      Top             =   60
   End
   Begin MSCommLib.MSComm Arduino 
      Left            =   3045
      Top             =   -15
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      CommPort        =   6
      DTREnable       =   -1  'True
      NullDiscard     =   -1  'True
      BaudRate        =   115200
      InputMode       =   1
   End
   Begin VB.Shape Shape6 
      Height          =   1215
      Left            =   120
      Top             =   600
      Width           =   3510
   End
   Begin VB.Label Label5 
      BackStyle       =   0  'Transparent
      Height          =   405
      Left            =   1650
      TabIndex        =   5
      Top             =   990
      Width           =   465
   End
   Begin VB.Label Label4 
      BackStyle       =   0  'Transparent
      Height          =   1230
      Left            =   150
      TabIndex        =   4
      Top             =   600
      Width           =   1395
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   1140
      Left            =   2325
      TabIndex        =   2
      Top             =   615
      Width           =   1185
   End
   Begin VB.Shape Shape5 
      Height          =   405
      Left            =   2670
      Shape           =   3  'Circle
      Top             =   990
      Width           =   510
   End
   Begin VB.Shape Shape4 
      Height          =   1215
      Left            =   2370
      Shape           =   3  'Circle
      Top             =   600
      Width           =   1110
   End
   Begin VB.Shape Shape3 
      Height          =   390
      Left            =   1740
      Shape           =   3  'Circle
      Top             =   990
      Width           =   225
   End
   Begin VB.Shape Shape2 
      Height          =   360
      Left            =   1680
      Shape           =   1  'Square
      Top             =   1005
      Width           =   390
   End
   Begin VB.Line Line1 
      X1              =   56
      X2              =   56
      Y1              =   64
      Y2              =   80
   End
   Begin VB.Shape Shape1 
      Height          =   510
      Left            =   495
      Shape           =   3  'Circle
      Top             =   945
      Width           =   720
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   405
      Left            =   2700
      TabIndex        =   3
      Top             =   990
      Width           =   465
   End
End
Attribute VB_Name = "Window"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Const PI = 3.14159
Private Const packetSize As Integer = 20

Private RotaryLength As Single
Private Angle As Single

Private centerX As Single
Private centerY As Single
Private moving As Boolean

Private Declare Sub RtlMoveMemory Lib "kernel32" (Dest As Any, ByVal Source As Long, ByVal Size As Long)

Public Function Large(ByVal V1 As Variant, ByVal V2 As Variant, Optional ByVal V3 As Variant, Optional ByVal V4 As Variant) As Variant
    If IsMissing(V3) Then
        If (V1 >= V2) Then
            Large = V1
        Else
            Large = V2
        End If
    ElseIf IsMissing(V4) Then
        If ((V2 >= V3) And (V2 >= V1)) Then
            Large = V2
        ElseIf ((V1 >= V3) And (V1 >= V2)) Then
            Large = V1
        Else
            Large = V3
        End If
    Else
        If ((V2 >= V3) And (V2 >= V1) And (V2 >= V4)) Then
            Large = V2
        ElseIf ((V1 >= V3) And (V1 >= V2) And (V1 >= V4)) Then
            Large = V1
        ElseIf ((V3 >= V1) And (V3 >= V2) And (V3 >= V4)) Then
            Large = V3
        Else
            Large = V4
        End If
    End If
End Function

Public Function Least(ByVal V1 As Variant, ByVal V2 As Variant, Optional ByVal V3 As Variant, Optional ByVal V4 As Variant) As Variant
    If IsMissing(V3) Then
        If (V1 <= V2) Then
            Least = V1
        Else
            Least = V2
        End If
    ElseIf IsMissing(V4) Then
        If ((V2 <= V3) And (V2 <= V1)) Then
            Least = V2
        ElseIf ((V1 <= V3) And (V1 <= V2)) Then
            Least = V1
        Else
            Least = V3
        End If
    Else
        If ((V2 <= V3) And (V2 <= V1) And (V2 <= V4)) Then
            Least = V2
        ElseIf ((V1 <= V3) And (V1 <= V2) And (V1 <= V4)) Then
            Least = V1
        ElseIf ((V3 <= V1) And (V3 <= V2) And (V3 <= V4)) Then
            Least = V3
        Else
            Least = V4
        End If
    End If
End Function

Public Function Distance(ByVal p1x As Single, ByVal p1y As Single, ByVal p2x As Single, ByVal p2y As Single) As Single
    Distance = ((((p1x - p2x) ^ 2) + ((p1y - p2y) ^ 2)) ^ (1 / 2))
End Function

Public Function DistanceSet(ByVal p1x As Single, ByVal p1y As Single, ByVal p2x As Single, ByVal p2y As Single, ByRef X As Single, ByRef Y As Single, ByVal n As Single)
    Dim dist As Single
    dist = Distance(p1x, p1y, p2x, p2y)

    If Not (dist = n) Then
        If ((dist > 0) And (n > 0)) Then
            X = Large(p1x, p2x) - Least(p1x, p2x)
            Y = Large(p1y, p2y) - Least(p1y, p2y)
            X = (Least(p1x, p2x) + (n * (X / dist)))
            Y = (Least(p1y, p2y) + (n * (Y / dist)))
        ElseIf (n = 0) Then
            X = p1x
            Y = p1y
        ElseIf (dist = 0) Then
            X = p2x
            Y = p2y

        End If
    End If

End Function

Private Sub Form_Load()
    Shape3.Tag = -1
    Shape5.Tag = -1
    
    RotaryLength = Distance(Line1.x1, Line1.y1, Line1.X2, Line1.Y2)
    Angle = 20
    
    centerX = Label3.Left
    centerY = Label3.Top
    
End Sub


Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 0 Then
        moving = False
    End If
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    PortClose
End Sub

Private Sub Form_Unload(Cancel As Integer)
    End
End Sub

Private Sub ConnectButton_Click()
    If Arduino.PortOpen Then
        PortClose
    Else
        PortOpen
    End If
End Sub


Private Function PortOpen() As Boolean
    On Error GoTo errorcatch

    PortClose

    If Arduino.PortOpen = True Then
        Arduino.PortOpen = False
    End If
    If Not Arduino.PortOpen Then
        Arduino.CommPort = (CommPort.ListIndex + 1)
        Arduino.Settings = "256000,N,8,1"
        Arduino.InputLen = packetSize
        Arduino.InBufferSize = packetSize
        Arduino.PortOpen = True
    End If
    
    PortOpen = Arduino.PortOpen
    Exit Function
errorcatch:
    PortOpen = False

    'MsgBox "Error: " & Err.Description

    Err.Clear
    
End Function

Private Sub PortClose()
    If Arduino.PortOpen Then
        Arduino.PortOpen = False
    End If
End Sub

Private Sub Label2_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        moving = True
        
    ElseIf Button = 2 Then
        Depress Shape5

    Else
    
        moving = False

    End If

End Sub

Private Sub Label2_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 0 Then
        moving = False
    End If
    If moving Then

        Dim newx As Single
        Dim newy As Single
        newy = ((Y / Screen.TwipsPerPixelY) - (Label3.Height / 2)) + Label2.Top
        newx = ((X / Screen.TwipsPerPixelX) - (Label3.Width / 2)) + Label2.Left
        
        If newx < Label2.Left Then newx = Label2.Left
        If newy < Label2.Top Then newy = Label2.Top
        If newx + Label3.Width > Label2.Left + Label2.Width Then newx = (Label2.Left + Label2.Width) - Label3.Width
        If newy + Label3.Height > Label2.Top + Label2.Height Then newy = (Label2.Top + Label2.Height) - Label3.Height
        
        Label3.Top = newy
        Label3.Left = newx
        
        Shape5.Top = Label3.Top
        Shape5.Left = Label3.Left
            
    End If
End Sub

Private Sub Label2_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 0 Then
        moving = False
    End If
End Sub

Private Sub Label4_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        Static startx As Single
        Static starty As Single
        If startx = 0 Then startx = X
        If starty = 0 Then starty = Y
        
        Angle = (((((X - startx) / 360) * ((Label4.Height / 2) / 100))) * 360)

        If Angle >= 340 Then Angle = 340
        If Angle < 20 Then Angle = 20
        
        

    Else
        startx = 0
        starty = 0
        
    End If
End Sub


Private Sub Depress(ByRef Shape As Shape)
    If Shape.Tag = -1 Then
        Shape.Left = Shape.Left + 1
        Shape.Top = Shape.Top + 1
        Shape.Width = Shape.Width - 2
        Shape.Height = Shape.Height - 2
        Shape.Tag = 10
    End If
End Sub

Private Sub Undepress(ByRef Shape As Shape)
    If Shape.Tag > 0 Then
        Shape.Tag = Shape.Tag - 1
    End If
    If Shape.Tag = 0 Then
        Shape.Left = Shape.Left - 1
        Shape.Top = Shape.Top - 1
        Shape.Width = Shape.Width + 2
        Shape.Height = Shape.Height + 2
        Shape.Tag = -1
    End If
End Sub

Private Sub Label5_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        Depress Shape3
    End If
End Sub

Private Sub MainLoop_Timer()

'    Angle = Angle + 1
'    If Angle >= 360 Then Angle = Angle - 360
'    If Angle < 0 Then Angle = Angle + 360
    
    If Not moving Then
        If centerX <> Label3.Left And centerY <> Label3.Top Then
            Dim dist As Single
            dist = Distance(centerX, centerY, Label3.Left, Label3.Top)
            If (dist \ 2) > 0 Then
                Dim X As Single
                Dim Y As Single
                DistanceSet centerX, centerY, Label3.Left, Label3.Top, X, Y, (dist / 2)
                Label3.Top = Y
                Label3.Left = X
            Else
                Label3.Top = centerY
                Label3.Left = centerX
            End If
            Shape5.Top = Label3.Top
            Shape5.Left = Label3.Left
            
        End If
    
    End If
    
    Undepress Shape3
    Undepress Shape5
    
    UpdateGUI
End Sub
Private Function Padding(ByVal NumZeros As Integer, ByVal txt As String) As String
    If NumZeros > Len(txt) Then
        Padding = String(NumZeros - Len(txt), "0") & txt
    Else
        Padding = txt
    End If
End Function
Private Sub UpdateGUI()
    ConnectButton.Caption = IIf(Arduino.PortOpen, "Close", "Open")
    CommPort.Enabled = Not Arduino.PortOpen
    Line1.x1 = Line1.X2 + (RotaryLength * Sin((Angle + 180) * (PI / 180)))
    Line1.y1 = Line1.Y2 - (RotaryLength * Cos((Angle + 180) * (PI / 180)))
       
    Dim txt As String
    txt = "s" & -CInt(CBool(Shape3.Tag > 0))
    
    txt = txt & " x" & Padding(4, Round((((Label3.Left - Label2.Left)) * (1024 / (Label2.Width - Label3.Width))), 0))
    
    txt = txt & " y" & Padding(4, Round((((Label3.Top - Label2.Top)) * (1024 / (Label2.Height - Label3.Height))), 0))
            
    txt = txt & " z" & Padding(4, Round(((1024 / 320) * (Angle - 20)), 0))

    txt = txt & " d" & -CInt(CBool(Shape5.Tag > 0))
        
    If PortOpen Then
        Arduino.Output = Replace(txt, " ", "") & vbLf
    Else
        Debug.Print txt
        
    End If
    
End Sub

