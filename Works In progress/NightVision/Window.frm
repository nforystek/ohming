VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Begin VB.Form Window 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Arduino Console"
   ClientHeight    =   9015
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   14310
   ClipControls    =   0   'False
   Icon            =   "Window.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   601
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   954
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture1 
      BeginProperty Font 
         Name            =   "Lucida Console"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   7770
      Left            =   150
      ScaleHeight     =   514
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   925
      TabIndex        =   2
      Top             =   630
      Width           =   13935
   End
   Begin VB.CommandButton ConnectButton 
      Caption         =   "Open"
      Height          =   345
      Left            =   1350
      TabIndex        =   1
      Top             =   105
      Width           =   705
   End
   Begin VB.ComboBox CommPort 
      Height          =   315
      ItemData        =   "Window.frx":0442
      Left            =   240
      List            =   "Window.frx":0476
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   120
      Width           =   1050
   End
   Begin VB.Timer MainLoop 
      Interval        =   1
      Left            =   4515
      Top             =   75
   End
   Begin MSCommLib.MSComm Arduino 
      Left            =   5550
      Top             =   45
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      CommPort        =   6
      DTREnable       =   -1  'True
      NullDiscard     =   -1  'True
      BaudRate        =   115200
      InputMode       =   1
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Height          =   255
      Left            =   2085
      TabIndex        =   3
      Top             =   165
      Width           =   1875
   End
End
Attribute VB_Name = "Window"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Const packetSize As Long = (8 * 16)
Private Const pixelHeight = 20
Private Const pixelWidth = 20

Private Elapsed As Single
Private BytesRead As Single

Private Declare Sub RtlMoveMemory Lib "kernel32" (Dest As Any, ByVal Source As Long, ByVal Size As Long)



Private Sub Form_Load()
    CommPort.ListIndex = 12
    ConnectButton_Click
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

    MsgBox "Error: " & Err.Description

    Err.Clear
    
End Function

Private Sub PortClose()
    If Arduino.PortOpen Then
        Arduino.PortOpen = False
    End If
End Sub

Public Sub ProcessSerial()
    If Arduino.PortOpen Then
        If Arduino.InBufferCount >= packetSize Then

            Dim inc() As Byte
            
            Dim tmp As Variant
            tmp = Arduino.Input
            inc = tmp
            BytesRead = BytesRead + packetSize
            
            Dim x As Integer
            Dim y As Integer
            Dim x1 As Long
            Dim y1 As Long
            
            
            
            Dim i As Integer
            Dim debugtxt As String
            
            For i = LBound(inc) To UBound(inc)

                debugtxt = debugtxt & IIf(Len(CStr(inc(i))) < 3, String(3 - Len(CStr(inc(i))), " ") & inc(i), inc(i)) & " "
                If (i + 1) Mod 16 = 0 Then debugtxt = debugtxt & vbCrLf
            Next
            Picture1.Cls
            
            Picture1.Print debugtxt
            
            
'            For i = LBound(inc) To UBound(inc)
'
'                x1 = x * (Picture1.Width / pixelHeight)
'                y1 = y * (Picture1.Height / pixelHeight)
'
'                Picture1.Line (x1, y1)-(x1 + (Picture1.Width / pixelHeight), y1 + (Picture1.Height / pixelHeight)), RGB(inc(i), inc(i), inc(i)), BF
'
'                x = x + 1
'                If x = pixelWidth Then
'                    x = 0
'                    y = y + 1
'                    If y = pixelHeight Then
'                        y = 0
'                    End If
'                End If
'
'            Next
            
            If Elapsed = 0 Or (Timer - Elapsed) >= 1 Then
                Elapsed = Timer
                Label1.Caption = BytesRead & " bytes/sec"
                BytesRead = 0
            End If
            
            Arduino.InBufferCount = 0
        End If
    End If
End Sub

Private Sub MainLoop_Timer()
    ProcessSerial
    UpdateGUI
End Sub

Private Sub UpdateGUI()
    ConnectButton.Caption = IIf(Arduino.PortOpen, "Close", "Open")
    CommPort.Enabled = Not Arduino.PortOpen

End Sub

