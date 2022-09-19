VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "mscomm32.ocx"
Begin VB.Form frmMain 
   AutoRedraw      =   -1  'True
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Scientific Equipment"
   ClientHeight    =   5085
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7650
   ClipControls    =   0   'False
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   ScaleHeight     =   5085
   ScaleWidth      =   7650
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox Picture1 
      Height          =   4815
      Left            =   7560
      ScaleHeight     =   4755
      ScaleWidth      =   5475
      TabIndex        =   33
      Top             =   120
      Visible         =   0   'False
      Width           =   5535
   End
   Begin VB.Timer Timer2 
      Enabled         =   0   'False
      Interval        =   1
      Left            =   4020
      Top             =   300
   End
   Begin VB.CommandButton Command20 
      Caption         =   "Meters"
      Height          =   375
      Left            =   6360
      TabIndex        =   29
      Top             =   4560
      Width           =   1155
   End
   Begin VB.Timer Timer1 
      Interval        =   1
      Left            =   4680
      Top             =   240
   End
   Begin VB.Frame Frame3 
      Caption         =   "Tumbler Controls"
      Height          =   915
      Left            =   4380
      TabIndex        =   23
      Top             =   4080
      Width           =   1875
      Begin VB.CommandButton Command19 
         Caption         =   "Off"
         Height          =   315
         Left            =   960
         TabIndex        =   25
         Top             =   360
         Width           =   735
      End
      Begin VB.CommandButton Command18 
         Caption         =   "On"
         Height          =   315
         Left            =   180
         TabIndex        =   24
         Top             =   360
         Width           =   735
      End
   End
   Begin VB.Frame Frame2 
      Caption         =   "Microscope Controls"
      Height          =   3555
      Left            =   60
      TabIndex        =   6
      Top             =   360
      Width           =   6195
      Begin VB.CommandButton Command16 
         Caption         =   "X"
         Height          =   615
         Left            =   3540
         TabIndex        =   30
         ToolTipText     =   "Stop all possible microscope sctions."
         Top             =   720
         Width           =   615
      End
      Begin VB.CommandButton Command17 
         Caption         =   "I"
         Height          =   615
         Left            =   420
         TabIndex        =   22
         ToolTipText     =   "Change the button operations from continuous to incremental; requiring repeated clicking to continuous movement."
         Top             =   720
         Width           =   615
      End
      Begin VB.CommandButton Command5 
         Caption         =   "X+"
         Height          =   615
         Left            =   2760
         TabIndex        =   7
         ToolTipText     =   "Tray's X axis adjustment, in viewport pan left or right, of the tray moving left or right."
         Top             =   1980
         Width           =   615
      End
      Begin VB.OptionButton Option4 
         Caption         =   "100/1.25"
         Height          =   195
         Left            =   4740
         TabIndex        =   21
         Top             =   2880
         Width           =   1035
      End
      Begin VB.OptionButton Option3 
         Caption         =   "40/.65"
         Height          =   195
         Left            =   4740
         TabIndex        =   20
         Top             =   2460
         Width           =   855
      End
      Begin VB.OptionButton Option2 
         Caption         =   "10/.25"
         Height          =   195
         Left            =   4740
         TabIndex        =   19
         Top             =   2040
         Width           =   855
      End
      Begin VB.OptionButton Option1 
         Caption         =   "4/.10"
         Height          =   195
         Left            =   4740
         TabIndex        =   18
         Top             =   1620
         Value           =   -1  'True
         Width           =   795
      End
      Begin VB.CommandButton Command3 
         Caption         =   "Y-"
         Height          =   615
         Left            =   1980
         TabIndex        =   9
         ToolTipText     =   "Tray's Y axis adjustment, in viewport pan up or down, of the tray moving forward or backward."
         Top             =   2340
         Width           =   615
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Y+"
         Height          =   615
         Left            =   1980
         TabIndex        =   10
         ToolTipText     =   "Tray's Y axis adjustment, in viewport pan up or down, of the tray moving forward or backward."
         Top             =   1620
         Width           =   615
      End
      Begin VB.CommandButton Command4 
         Caption         =   "X-"
         Height          =   615
         Left            =   1200
         TabIndex        =   8
         ToolTipText     =   "Tray's X axis adjustment, in viewport pan left or right, of the tray moving left or right."
         Top             =   1980
         Width           =   615
      End
      Begin VB.CommandButton Command13 
         Caption         =   "D-"
         Height          =   615
         Left            =   420
         TabIndex        =   15
         ToolTipText     =   "Height/distance of the dialated lense under the tray."
         Top             =   2340
         Width           =   615
      End
      Begin VB.CommandButton Command12 
         Caption         =   "D+"
         Height          =   615
         Left            =   420
         TabIndex        =   14
         ToolTipText     =   "Height/distance of the dialated lense under the tray."
         Top             =   1620
         Width           =   615
      End
      Begin VB.CommandButton Command15 
         Caption         =   "L-"
         Height          =   615
         Left            =   2340
         TabIndex        =   17
         ToolTipText     =   "Luminance level of lighting under lense."
         Top             =   720
         Width           =   615
      End
      Begin VB.CommandButton Command14 
         Caption         =   "L+"
         Height          =   615
         Left            =   1620
         TabIndex        =   16
         ToolTipText     =   "Luminance level of lighting under lense."
         Top             =   720
         Width           =   615
      End
      Begin VB.CommandButton Command10 
         Caption         =   "Z-"
         Height          =   615
         Left            =   3540
         TabIndex        =   12
         ToolTipText     =   "Tray's Z axis adjustment, in viewport nearer or farther, of the tray moving up or down."
         Top             =   2340
         Width           =   615
      End
      Begin VB.CommandButton Command11 
         Caption         =   "Z+"
         Height          =   615
         Left            =   3540
         TabIndex        =   13
         ToolTipText     =   "Tray's Z axis adjustment, in viewport nearer or farther, of the tray moving up or down."
         Top             =   1620
         Width           =   615
      End
      Begin MSCommLib.MSComm MSComm1 
         Left            =   660
         Top             =   0
         _ExtentX        =   1005
         _ExtentY        =   1005
         _Version        =   393216
         CommPort        =   6
         DTREnable       =   -1  'True
         BaudRate        =   115200
      End
      Begin VB.Label Label1 
         Caption         =   "Motor handling ratio precisions:"
         Height          =   555
         Left            =   4740
         TabIndex        =   28
         Top             =   840
         Width           =   1155
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Centrifuge Controls"
      Height          =   915
      Left            =   60
      TabIndex        =   1
      Top             =   4080
      Width           =   4215
      Begin VB.CommandButton Command9 
         Caption         =   "Full"
         Height          =   315
         Left            =   2520
         TabIndex        =   11
         Top             =   360
         Width           =   735
      End
      Begin VB.CommandButton Command8 
         Caption         =   "Off"
         Height          =   315
         Left            =   3300
         TabIndex        =   5
         Top             =   360
         Width           =   735
      End
      Begin VB.CommandButton Command6 
         Caption         =   "Slower"
         Height          =   315
         Left            =   180
         TabIndex        =   4
         Top             =   360
         Width           =   735
      End
      Begin VB.CommandButton Command2 
         Caption         =   "Faster"
         Height          =   315
         Left            =   960
         TabIndex        =   3
         Top             =   360
         Width           =   735
      End
      Begin VB.CommandButton Command7 
         Caption         =   "Auto"
         Height          =   315
         Left            =   1740
         TabIndex        =   2
         Top             =   360
         Width           =   735
      End
   End
   Begin VB.Label Label6 
      BackStyle       =   0  'Transparent
      Height          =   4455
      Left            =   6360
      TabIndex        =   32
      Top             =   60
      Width           =   555
   End
   Begin VB.Label Label5 
      BackStyle       =   0  'Transparent
      Height          =   4455
      Left            =   6960
      TabIndex        =   31
      Top             =   60
      Width           =   555
   End
   Begin VB.Label Label4 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Off"
      Height          =   195
      Left            =   6420
      TabIndex        =   26
      Tag             =   "°F"
      Top             =   1620
      Width           =   435
   End
   Begin VB.Shape Shape1 
      BackColor       =   &H00800000&
      BorderColor     =   &H00FFFFFF&
      Height          =   4455
      Left            =   6960
      Top             =   60
      Width           =   555
   End
   Begin VB.Line Line1 
      Visible         =   0   'False
      X1              =   6420
      X2              =   6840
      Y1              =   3180
      Y2              =   3180
   End
   Begin VB.Shape Shape2 
      Height          =   4455
      Left            =   6360
      Top             =   60
      Width           =   555
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      BackColor       =   &H00000080&
      Caption         =   "Closed"
      ForeColor       =   &H00FFFFFF&
      Height          =   195
      Left            =   120
      TabIndex        =   0
      Top             =   60
      Width           =   6135
   End
   Begin VB.Image Image1 
      Height          =   4425
      Left            =   6360
      Picture         =   "frmMain.frx":0442
      Stretch         =   -1  'True
      Top             =   60
      Width           =   540
   End
   Begin VB.Line Line3 
      BorderColor     =   &H00FFFFFF&
      Visible         =   0   'False
      X1              =   6960
      X2              =   7500
      Y1              =   1800
      Y2              =   1800
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      Visible         =   0   'False
      X1              =   6960
      X2              =   7500
      Y1              =   3360
      Y2              =   3360
   End
   Begin VB.Label Label3 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      ForeColor       =   &H00C0C0C0&
      Height          =   255
      Left            =   6960
      TabIndex        =   27
      Top             =   1320
      Width           =   555
   End
   Begin VB.Shape Shape3 
      BackColor       =   &H00C00000&
      BackStyle       =   1  'Opaque
      Height          =   4455
      Left            =   6960
      Top             =   60
      Width           =   555
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

'Private cam As New NTImaging10.Camera

#If WINE = 0 Then
Public WithEvents svce As NTAdvFTP61.Socket
Attribute svce.VB_VarHelpID = -1
#End If

Private Rerun As Boolean
Private SonicRate As Single

Private SonicMax As Single

Private atrate As Single
Private adjust As Single


Private Sub TermistorState(ByVal OnorOff As Boolean)
    If Not OnorOff Then


        If Label4.Top <> 1620 Then Label4.Top = 1620
        If Label4.Caption <> "Off" Then Label4.Caption = "Off"
        If Line1.Visible Then Line1.Visible = False
        
    End If
End Sub
Private Sub Send(ByVal data)
#If WINE = 0 Then
     svce.Send data
#Else
    MSComm1.Output = data
#End If
End Sub
Private Function PortOpen() As Boolean
#If WINE = 0 Then
    PortOpen = svce.Connected
#Else
    PortOpen = MSComm1.PortOpen
#End If
End Function
Private Sub Command16_Click()
     Send "x"
End Sub

Private Sub Command17_Click()
    If Command17.Caption = "I" Then
        Command17.Caption = "C"
    Else
        Command17.Caption = "I"
    End If
    Send "q"
End Sub


Private Sub Command19_Click()
     Send "p"

End Sub

Private Sub Command20_Click()
    frmSetup.Show 1
End Sub



Private Sub Form_Unload(Cancel As Integer)
    Unload frmSetup
End Sub

Private Sub Image1_Click()
    If PortOpen Then
        Send "t"
    End If
    TermistorState False
End Sub



Private Sub Label2_Click()
    If PortOpen Then
        ClosePort
    Else
        OpenPort
    End If

End Sub
Private Sub EnableForm()
    Dim ctl As Control
    For Each ctl In Me.Controls
        Select Case TypeName(ctl)
            Case "CommandButton", "OptionButton", "TextBox"
                ctl.Enabled = PortOpen
        End Select
    Next
    If PortOpen Then
        Label2.Caption = "Open"
        Label2.BackColor = &H8000&

    Else
        Label2.Caption = "Closed (click here to open)"
        Label2.BackColor = &H80&
        
        Line1.Visible = False
        
        Line2.Visible = False
        Line3.Visible = False
        
        Label3.Visible = False
        TermistorState False
        
                        
    End If
        #If WINE = -1 Then
            Timer2.Enabled = PortOpen
        #End If

End Sub
Private Sub Command18_Click()
    Send "v"
End Sub

Private Sub Label5_Click()
    Send "u"

End Sub



Private Sub Label6_Click()
    If PortOpen Then
        Send "t"
        TermistorState True
    Else
        TermistorState False
    End If
End Sub

Private Sub Option1_Click()
     Send "o"
End Sub

Private Sub Option2_Click()
     Send "j"
End Sub

Private Sub Option3_Click()
     Send "l"
End Sub

Private Sub Option4_Click()
     Send "m"
End Sub

Private Sub Command10_Click()
     Send "b"
'Z+
End Sub

Private Sub Command11_Click()
     Send "c"
'Z-
End Sub

Private Sub Command12_Click()
     Send "k"
End Sub

Private Sub Command13_Click()
     Send "r"

End Sub

Private Sub Command14_Click()
     Send "g"
'L+
End Sub

Private Sub Command15_Click()
     Send "h"
'L-
End Sub

Private Sub Command2_Click()

     Send "i"

End Sub

Private Sub Command6_Click()

     Send "d"

End Sub


Private Sub Command1_Click()

     Send "n"

End Sub

Private Sub Command3_Click()

     Send "s"

End Sub

Private Sub Command4_Click()

     Send "e"

End Sub

Private Sub Command5_Click()

     Send "w"

End Sub

Private Sub Command7_Click()

     Send "a"

End Sub

Private Sub Command8_Click()

     Send "p"


End Sub

Private Sub Command9_Click()
     Send "f"

End Sub

Private Sub Form_Load()
    Load frmSetup
    
    Timer1.Tag = 1
    
'    If ProcessRunning("securities.exe") Then
'        Rerun = True
'        RunProcess SysPath & "net.exe", "stop Security", vbHidden, True
'    End If
#If WINE = 0 Then
    Set svce = New NTAdvFTP61.Socket
#End If
    OpenPort

    

    
'   Dim inData As String
'   inData = ReadFile("C:\Development\Projects\Securities\Securities.ini")
'   Dim inLine As String
'   Do Until inData = ""
'    inLine = RemoveNextArg(inData, vbCrLf)
'        Select Case LCase(RemoveNextArg(inLine, "="))
'
'            Case "alarm"
'                If IsNumeric(Replace(Replace(inLine, " ", ""), "-", "")) Then
'                    Text1.Text = NextArg(inLine, "-")
'                    Text2.Text = RemoveArg(inLine, "-")
'                End If
'
'            Case "range"
'                If IsNumeric(Replace(Replace(inLine, " ", ""), "-", "")) Then
'                    Text3.Text = NextArg(inLine, "-")
'                    Text4.Text = RemoveArg(inLine, "-")
'                End If
'        End Select
'    Loop
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)

    #If WINE = 0 Then
    Set svce = Nothing
    #Else
    If PortOpen Then
         Send "x"
         Send "p"
    End If
    ClosePort
    #End If
'    If Rerun Then
'        If ProcessRunning("securities.exe") = 0 Then
'            RunProcess SysPath & "net.exe", "start Security", vbHidden, True
'        End If
'    End If
End Sub


Private Function OpenPort() As Boolean


    ClosePort

    If Not PortOpen Then
    On Error GoTo failit
        #If WINE = 0 Then
            svce.Connect "192.168.1.100", 9389
        #Else
            MSComm1.CommPort = 5
            MSComm1.Settings = "115200,N,8,1"
    
            MSComm1.InBufferSize = 1
    
            MSComm1.OutBufferSize = 1
    
            MSComm1.PortOpen = True
            If PortOpen Then
                svce.Send "u"
                svce.Send "o"
                svce.Send "z"
                    
            End If
        #End If
        
    End If
    EnableForm
    Exit Function
failit:
    MsgBox "Unable to connect to the host."
    Err.Clear


End Function

Private Sub ClosePort()

    If PortOpen Then
        #If WINE = 0 Then
        svce.Disconnect
        #Else
        MSComm1.PortOpen = False
        #End If
    End If

    EnableForm
End Sub

        #If WINE = 0 Then
        
Private Sub svce_Connected()

    
    svce.Send "z"
    
        EnableForm
End Sub
#End If


#If WINE = 0 Then
Private Sub svce_DataArriving()
    Indata
End Sub
#Else
Private Sub Timer2_Timer()
    Indata
End Sub
#End If

Private Sub Indata()
    If PortOpen Then
        Static data As String

        #If WINE = 0 Then
            data = data & svce.Read
        #Else
            data = data & MSComm1.Input
        #End If

        Dim nextIn As String
        Dim inLine As String
         
        Do While InStr(data, vbCrLf) > 0
            nextIn = RemoveNextArg(data, vbCrLf)
            Select Case Left(nextIn, 1)
                Case "q"
                    Command17.Caption = "C"
                Case "y"
                    
                    nextIn = Mid(nextIn, 2)
                    
                    Do Until nextIn = ""
                     inLine = RemoveNextArg(RemoveNextArg(nextIn, ";"), vbCrLf)
                         Select Case LCase(RemoveNextArg(inLine, "="))
                             Case "sound"
                                 If PathExists(inLine, True) Then
                                     frmSetup.Text6.Text = inLine
                                 End If
                             Case "alarm"
                                 If IsNumeric(Replace(Replace(inLine, " ", ""), "-", "")) Then
                                     frmSetup.Text1.Text = NextArg(inLine, "-")
                                     frmSetup.Text2.Text = RemoveArg(inLine, "-")
                                 End If
                                 
                             Case "range"
                                 If IsNumeric(Replace(Replace(inLine, " ", ""), "-", "")) Then
                                     frmSetup.Text3.Text = NextArg(inLine, "-")
                                     frmSetup.Text4.Text = RemoveArg(inLine, "-")
                                 End If
                            Case "email"
                                If InStr(inLine, "@") > 1 And InStr(inLine, "@") < Len(inLine) Then
                                    frmSetup.Text5.Text = inLine
                                End If
                         End Select
                     Loop
                
                Case "S"
                    nextIn = Mid(nextIn, 2)
                    If IsNumeric(nextIn) Then

                        If CLng(nextIn) > SonicMax Then
                            SonicMax = CLng(nextIn)
                        End If
                        If SonicMax = 0 Then Exit Sub
                        
                        Line3.X1 = Shape1.Left
                        Line3.X2 = Shape1.Left + Shape1.Width
                        Line3.Y1 = Shape1.Top + (nextIn * ((Shape1.Height - (Screen.TwipsPerPixelY * 4)) / SonicMax))
                        Line3.Y2 = Line3.Y1

                        If Line3.Y1 - Shape1.Top <= Label3.Height Then
                            Label3.Top = Line3.Y1
                        Else
                            Label3.Top = Line3.Y1 - Label3.Height
                        End If

                        Line3.BorderColor = vbWhite
                        Label3.ForeColor = vbWhite
                        If Not Line3.Visible Then Line3.Visible = True
                        If Not Label3.Visible Then Label3.Visible = True
                        Label3.Caption = nextIn
                        
                        
                        SonicRate = (Timer - SonicRate)
                
                        Line2.Y1 = Line3.Y1
                        Line2.Y2 = Line3.Y1
                        atrate = ((Shape1.Height / SonicMax) * CLng(nextIn)) * (1000 / SonicRate)
                        
                        If Not Line2.Visible Then Line2.Visible = True
                        
                    End If
                                     
                    
                Case "T"
                
                    
                    nextIn = Mid(nextIn, 2)
                    If IsNumeric(nextIn) Then
                        TermistorState True
                        Line1.X1 = Shape2.Left
                        Line1.X2 = Shape2.Left + Shape2.Width
                        Line1.Y1 = Shape2.Top + Shape2.Height - (nextIn * (Shape2.Height / 150))
                        Line1.Y2 = Line1.Y1
                        
                        If Line1.Y1 - Shape2.Top <= Label4.Height Then
                            Label4.Top = Line1.Y1
                        Else
                            Label4.Top = Line1.Y1 - Label4.Height
                        End If
                        
                        Label4.Caption = RemoveNextArg(nextIn, ".") & Label4.Tag
                        
                        If Not Line1.Visible Then Line1.Visible = True
                        If Not Label4.Visible Then Label4.Visible = True
                    Else
                        TermistorState False
                    End If
                    
                    
            End Select
             
    
        Loop
        
    End If

End Sub
#If WINE = 0 Then
Private Sub svce_Disconnected()

    EnableForm
End Sub
#End If
Private Sub Timer1_Timer()
    On Error Resume Next
    
    Static drawing As Boolean

    Static passed As Boolean
    If SonicRate = 0 Or Not IsNumeric(Label3.Caption) Then

        Exit Sub

    End If


    Line2.Y1 = Line2.Y1 - atrate
    Line2.Y2 = Line2.Y1


    If Line3.Tag = "2" Then

        Select Case Line3.BorderColor
            Case vbWhite
                Line3.BorderColor = &HFFC0C0
                Label3.ForeColor = &HFFC0C0

            Case &HFFC0C0
                Line3.BorderColor = &HFF8080
                Label3.ForeColor = &HFF8080

            Case &HFF8080
                Line3.BorderColor = &HFF0000
                Label3.ForeColor = &HFF0000

            Case &HFF0000
                Line3.BorderColor = &HC00000
                Label3.ForeColor = &HC00000

            Case &HC00000
                Line3.BorderColor = &H800000
                Label3.ForeColor = &H800000

            Case &H800000
                If Line3.Visible Then Line3.Visible = False
                If Label3.Visible Then Label3.Visible = False

        End Select
        Line3.Tag = "0"

    ElseIf Line3.Tag = "0" Then
        Line3.Tag = "1"
    ElseIf Line3.Tag = "1" Then
        Line3.Tag = "2"
    Else
        Line3.Tag = "2"
    End If
    
    If Err Then Err.Clear
End Sub

