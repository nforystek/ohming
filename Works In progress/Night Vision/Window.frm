VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Begin VB.Form Window 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Arduino Console"
   ClientHeight    =   5130
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   8160
   ClipControls    =   0   'False
   Icon            =   "Window.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5130
   ScaleWidth      =   8160
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture1 
      Height          =   3480
      Left            =   2985
      ScaleHeight     =   3420
      ScaleWidth      =   4905
      TabIndex        =   18
      Top             =   1410
      Width           =   4965
   End
   Begin VB.CommandButton ConnectButton 
      Caption         =   "Open"
      Height          =   345
      Left            =   1215
      TabIndex        =   1
      Top             =   990
      Width           =   705
   End
   Begin VB.ComboBox CommPort 
      Height          =   315
      ItemData        =   "Window.frx":0442
      Left            =   105
      List            =   "Window.frx":0476
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   1005
      Width           =   1050
   End
   Begin VB.Timer MainLoop 
      Interval        =   1
      Left            =   1830
      Top             =   870
   End
   Begin MSCommLib.MSComm Arduino 
      Left            =   2295
      Top             =   840
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
      Caption         =   "Label1"
      Height          =   240
      Index           =   15
      Left            =   0
      TabIndex        =   17
      Top             =   720
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   14
      Left            =   735
      TabIndex        =   16
      Top             =   720
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   13
      Left            =   1470
      TabIndex        =   15
      Top             =   720
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   12
      Left            =   2205
      TabIndex        =   14
      Top             =   720
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   11
      Left            =   0
      TabIndex        =   13
      Top             =   480
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   10
      Left            =   735
      TabIndex        =   12
      Top             =   480
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   9
      Left            =   1470
      TabIndex        =   11
      Top             =   480
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   8
      Left            =   2205
      TabIndex        =   10
      Top             =   480
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   7
      Left            =   0
      TabIndex        =   9
      Top             =   240
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   6
      Left            =   735
      TabIndex        =   8
      Top             =   240
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   5
      Left            =   1470
      TabIndex        =   7
      Top             =   240
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   4
      Left            =   2205
      TabIndex        =   6
      Top             =   240
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   3
      Left            =   2205
      TabIndex        =   5
      Top             =   0
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   2
      Left            =   1470
      TabIndex        =   4
      Top             =   0
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   1
      Left            =   735
      TabIndex        =   3
      Top             =   0
      Width           =   735
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Label1"
      Height          =   240
      Index           =   0
      Left            =   0
      TabIndex        =   2
      Top             =   0
      Width           =   735
   End
End
Attribute VB_Name = "Window"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
    CommPort.ListIndex = 5
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

Private Sub ConsoleText_Change()
    ConsoleText.SelStart = Len(ConsoleText.Text)
End Sub

Private Sub CommandText_KeyPress(KeyAscii As Integer)
    If KeyAscii = 13 Then
        SendButton_Click
    End If
End Sub

Private Function PortOpen() As Boolean
    On Error GoTo errorcatch

    PortClose

    If Arduino.PortOpen = True Then
        Arduino.PortOpen = False
    End If
    If Not Arduino.PortOpen Then
        Arduino.CommPort = 6
        Arduino.Settings = "115200,N,8,1"
        Arduino.InputLen = 16
        Arduino.InBufferSize = 16
        Arduino.OutBufferSize = 16
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
        If Arduino.InBufferCount >= 16 Then
            Dim inc() As Byte
            Dim tmp As Variant
            tmp = Arduino.Input
            inc = tmp
            Dim i As Long
            For i = LBound(inc) To UBound(inc)
                Label1(i).Caption = inc(i)
            Next
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

