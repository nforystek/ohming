VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "MSCOMM32.OCX"
Begin VB.Form Window 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Arduino Console"
   ClientHeight    =   6945
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   6210
   ClipControls    =   0   'False
   BeginProperty Font 
      Name            =   "Terminal"
      Size            =   6
      Charset         =   255
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "Window.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   463
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   414
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      BeginProperty Font 
         Name            =   "Lucida Console"
         Size            =   9
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   4980
      Left            =   195
      ScaleHeight     =   328
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   383
      TabIndex        =   2
      Top             =   585
      Width           =   5805
   End
   Begin VB.CommandButton ConnectButton 
      Caption         =   "Open"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   345
      Left            =   1350
      TabIndex        =   1
      Top             =   105
      Width           =   705
   End
   Begin VB.ComboBox CommPort 
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
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
      Left            =   3855
      Top             =   90
   End
   Begin MSCommLib.MSComm Arduino 
      Left            =   4125
      Top             =   90
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
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   2085
      TabIndex        =   3
      Top             =   150
      Width           =   3870
   End
End
Attribute VB_Name = "Window"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private Const packetSize As Long = (4 * 2)
Private Const pixelHeight = 100
Private Const pixelWidth = 100
Private xWidth As Single
Private yHeight As Single
            
Private Elapsed As Single
Private BytesRead As Single

Private Declare Sub RtlMoveMemory Lib "kernel32" (Dest As Any, ByVal Source As Long, ByVal Size As Long)


Private Sub Form_Load()
    CommPort.ListIndex = 2
    ConnectButton_Click
    xWidth = (Picture1.Width / pixelWidth)
    yHeight = (Picture1.Height / pixelHeight)
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

    Debug.Print "Error: " & Err.Description

    Err.Clear
    
End Function

Private Sub PortClose()
    If Arduino.PortOpen Then
        Arduino.PortOpen = False
    End If
End Sub

Public Sub ProcessSerial()
On Error GoTo stopcommport

    If Arduino.PortOpen Then
        If Arduino.InBufferCount >= packetSize Then

            Dim inc() As Byte
            Dim ata() As Single
            
            Dim tmp As Variant
            tmp = Arduino.Input
            inc = tmp
            BytesRead = BytesRead + packetSize
            
            Dim x As Integer
            Dim y As Integer
            Dim x1 As Single
            Dim y1 As Single

            
            
            Dim i As Integer
            ReDim ata(LBound(inc) To UBound(inc)) As Single
            
            For i = LBound(inc) To UBound(inc)
                ata(i) = inc(i)
            Next
            
            
            Me.CurrentX = Picture1.Left
            Me.CurrentY = Picture1.Top + Picture1.Height + Picture1.Left
            
            
            Dim debugtxt As String
            debugtxt = "   "
            For i = LBound(inc) To UBound(inc)
                debugtxt = debugtxt & IIf(Len(CStr(inc(i))) < 3, String(3 - Len(CStr(inc(i))), " ") & inc(i), inc(i)) & " "
                If (i + 1) Mod 16 = 0 Then debugtxt = debugtxt & vbCrLf & "   "
            Next
            Me.Cls
            
            Me.Print vbCrLf & vbCrLf & vbCrLf & vbCrLf & _
            vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & _
            vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & _
            vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & _
                    vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & _
                    vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & debugtxt
            



            'Picture1.Cls
            If UBound(inc) > -1 Then
            

                Dim shade As Single
                Dim shade1 As Single
                Dim shade2 As Single
                Dim shade3 As Single
                Dim shade4 As Single

                
                
                x = 1
                y = 1
                On Error GoTo 0
                
                Do Until y = -1
    
                    x1 = (x - 1) * xWidth
                    y1 = (y - 1) * yHeight
    
                    'combine four corner directions for one rgb black and white blend
                    
                    'for each N, S, E and W there is a set of two transmitters
                    'during their states 16 reads occur for each N, S, E, W too
                    'wooven into the reads, is a state of the first clockwise transmitter on alone for one read,
                    'then both are one for two reads then the second clockwise transmitter is on alone for one read,
                    'making a total of four read that are as well occuring clockwise in four pairs of two readers
                    'so
                    '16 reads in all, that break down into 4 sets of 4 transmission states between 2 transmitters
                    'and all are going clockwise N, S, E and W directionally at different micro distances arranged
                    '

                    shade1 = MakePixel(x, pixelWidth, inc(0), inc(1))
                    shade2 = MakePixel(y, pixelHeight, inc(2), inc(3))
                    shade3 = MakePixel(x, pixelWidth, inc(4), inc(5))
                    shade4 = MakePixel(y, pixelHeight, inc(6), inc(7))
                    
                    
                    shade = ((((shade1 + shade3) / 2) + ((shade2 + shade4) / 2)) / 2)
    

                    
                    If shade >= 0 And shade <= 255 Then
                        Picture1.Line (x1, y1)-(x1 + xWidth, y1 + yHeight), RGB(shade, shade, shade), BF
                    End If
                    
                    
                    
                    x = x + 1
                    If x > pixelWidth Then
                        x = 1
                        y = y + 1
                        If y > pixelHeight Then
                            y = -1
                        End If
                    End If
                    

    
                Loop
            End If
            
            If Elapsed = 0 Or (Timer - Elapsed) >= 1 Then
                Elapsed = Timer
                Label1.Caption = BytesRead & " bytes/sec  rec-size " & (Abs(LBound(inc) + -1) + UBound(inc))
                BytesRead = 0
            End If
            
            Arduino.InBufferCount = 0
        End If
    Else
        PortOpen
    End If
    
    Exit Sub
stopcommport:
    Err.Clear
    PortClose
    
End Sub

Private Function MakePixel(ByVal coord As Single, ByVal bounds As Single, ByVal read1 As Single, ByVal read2 As Single) As Single
    Dim difference As Single
    Dim influence As Single
    Dim amplitude As Single
    amplitude = 1
    read1 = read1 * amplitude
    read2 = read2 * amplitude
    
    If (read1 > read2) Then
        difference = (read1 - read2)
    Else
        difference = (read2 - read1)
    End If
    difference = (difference * (coord / bounds))

    MakePixel = (read1 * difference)
End Function


Private Sub MainLoop_Timer()
    ProcessSerial
    UpdateGUI
End Sub

Private Sub UpdateGUI()
    ConnectButton.Caption = IIf(Arduino.PortOpen, "Close", "Open")
    CommPort.Enabled = Not Arduino.PortOpen

End Sub

