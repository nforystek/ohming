VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Begin VB.Form Window 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Arduino Console"
   ClientHeight    =   3240
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5595
   ClipControls    =   0   'False
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3240
   ScaleWidth      =   5595
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
      Left            =   4785
      TabIndex        =   1
      Top             =   2790
      Width           =   750
   End
   Begin VB.TextBox CommandText 
      Height          =   360
      Left            =   120
      TabIndex        =   0
      Text            =   "hello"
      Top             =   2775
      Width           =   4620
   End
   Begin VB.TextBox ConsoleText 
      Height          =   2640
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

Private Handshake As Integer
Private Header As Boolean

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
        If SendButton.Enabled Then SendButton_Click
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
        SendButton.Enabled = False
        Handshake = 0
    End If
    If Not Arduino.PortOpen Then
        Arduino.CommPort = 5
        Arduino.Settings = "115200,N,8,1"
        Arduino.InBufferSize = 1
        Arduino.OutBufferSize = 1
        Handshake = 1
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
            If Len(Text) > 255 Then
                out = out & Chr(255) & Left(Text, 255)
                Text = Mid(Text, 256)
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

Public Sub ProcessPackets()
    Static data As String
    Dim bl As Byte
    data = data & Arduino.Input
    Select Case Asc(IIf(data = "", Chr(0), Left(data, 1)))
        Case 1
            Handshake = 1 'the routine wont
            'handle single byte packets now
            'with handshake, they must be 2+
        Case 0
            Arduino.Output = DataSend
        Case Else
            bl = Asc(Left(data, 1))
            If (Len(Left(data, bl + 1)) >= (bl + 1)) And (bl > 0) Then
                'buffer contains amount of data for record, or more
                data = Mid(data, 2)
                DebugText Left(data, bl)
                data = Mid(data, bl + 1)
            ElseIf Arduino.InBufferCount > 0 Then
                'buffer is under the amount of data for recrd
                data = data & Arduino.Input
            Else
                'TODO: timeout waiting and reset handhsake
                'to tell the remote serial to repeat packet
            End If
    End Select
End Sub

Public Function Toggler(ByVal Value As Long) As Long
    Toggler = (-CInt(CBool(Value)) + -1) + -CInt(Not CBool(-Value + -1))
End Function

Public Sub ProcessSerial()
    '
    'the Handshake helps stop initial garbage sends
    'and transactions packets to thoroughly complete
    'or repeat handshake for any packet that is loss
    '
    'it starts at 1 repeating send until the remote
    'responds with the same, and then has to do so
    'with 0 to know the remote is aware and not still
    'sending failed packets to unhandled information
    '
    'each poacket after a handshake toggles the bit
    '0 or 1, or char in this case, and a repeat of
    'the 0 or 1 in a packet row is a request resend
    'which then needs to handshake again, so in data
    'or connection loss the handshake must be redone
    '
    'this implementation so far occurs handshake
    'and lets the packets through from then on, no
    'header or stop bit, assuming the arduino will
    'preform from then on, and that will be tragic
    '
    
    Dim inc As String
    If Arduino.PortOpen Then
        Select Case Handshake
            Case 0, 1
                '
                'a carrier and receiver is defined by who
                'inititates connection to who in calling
                'that is the difference between this short
                'piece of handshake code and the one uploaded
                'to the arduino, since it never connects out
                'then we don't need to make each preform both
                'the arduino of course, reads first in 1 loop
                'only if exists, sending secondly, this part
                'sends first, loops, then reads, as carrier
                '
                If Arduino.InBufferCount = 0 Then
                    Arduino.Output = CStr(Abs(Handshake))
                    Debug.Print ">" & CStr(Abs(Handshake))
                Else
                    inc = Arduino.Input
                    Debug.Print "<" & inc
                    Handshake = Toggler(Handshake)
                End If
            Case -1
                ProcessPackets
        End Select
    End If
End Sub

Private Sub MainLoop_Timer()
    ProcessSerial
    SendButton.Enabled = (Handshake = -1) And Arduino.PortOpen
End Sub
