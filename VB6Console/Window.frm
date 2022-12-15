VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Begin VB.Form Window 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Arduino Console"
   ClientHeight    =   4575
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   10380
   ClipControls    =   0   'False
   Icon            =   "Window.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   4575
   ScaleWidth      =   10380
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer MainLoop 
      Interval        =   1
      Left            =   1440
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
   Begin VB.CommandButton SendButton 
      Caption         =   "&Send"
      Default         =   -1  'True
      Height          =   345
      Left            =   4845
      TabIndex        =   1
      Top             =   4140
      Width           =   660
   End
   Begin VB.TextBox CommandText 
      Height          =   360
      Left            =   120
      TabIndex        =   0
      Text            =   "hello"
      Top             =   4125
      Width           =   4620
   End
   Begin VB.TextBox ConsoleText 
      Height          =   3975
      Left            =   105
      Locked          =   -1  'True
      MultiLine       =   -1  'True
      ScrollBars      =   2  'Vertical
      TabIndex        =   2
      Top             =   60
      Width           =   5370
   End
End
Attribute VB_Name = "Window"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Const MaxPacketSize = 200

Private Sub SendButton_Click()
    DataSend CommandText.Text
    CommandText.Text = ""
    CommandText.SetFocus
End Sub

Private Sub DebugText(ByVal data As String)
    ConsoleText.Text = ConsoleText.Text & data
    ConsoleText_Change
End Sub

Private Sub Form_Unload(Cancel As Integer)
    End
End Sub

Private Sub ConsoleText_Change()
    ConsoleText.SelStart = Len(ConsoleText.Text)
End Sub

Private Sub CommandText_KeyPress(KeyAscii As Integer)
    If KeyAscii = 13 Then
        SendButton_Click
    End If
End Sub

Private Sub Form_Load()
    PortOpen
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)
    PortClose
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
        Arduino.InBufferSize = 1
        Arduino.OutBufferSize = 1
        Arduino.PortOpen = True
    End If
    
    PortOpen = Arduino.PortOpen
    Exit Function
errorcatch:
    PortOpen = False

    DebugText "Error: " & Err.Description

    Err.Clear
    
End Function

Private Sub PortClose()
    If Arduino.PortOpen Then
        Arduino.PortOpen = False
    End If
End Sub

Private Function DataSend(Optional ByVal Text As String = "") As String
    Static out As String
    If Text <> "" Then 'supplied text then build up out
    'buffer as byte headed records, size of following data
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
        'no parameter, so requesting any next byte to send
        DataSend = Left(out, 1)
        out = Mid(out, 2)
    Else 'no text, and no out data, assuming request for info,
    'sock.send will immediate return for nullstring, no send
        DataSend = ""
    End If
End Function

Public Sub ProcessSerial()
    Static data As String
    If Arduino.PortOpen Then
        Dim bl As Byte
        data = data & Arduino.Input
        Select Case Asc(IIf(data = "", Chr(0), Left(data, 1)))
            Case 0
                Arduino.Output = DataSend
            Case Else
                bl = Asc(Left(data, 1))
                If (Len(Left(data, bl + 1)) >= (bl + 1)) And (bl > 0) Then
                    'buffer contains amount of data for record, or more
                    data = Mid(data, 2)
                    DebugText Left(data, bl)
                    data = Mid(data, bl + 1)
                Else 'buffer is under the amount of data for record
                    data = data & Arduino.Input
                End If
        End Select
    End If
End Sub

Private Sub MainLoop_Timer()
    ProcessSerial
End Sub

