VERSION 5.00
Object = "{648A5603-2C6E-101B-82B6-000000000014}#1.1#0"; "mscomm32.ocx"
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   3630
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   9165
   LinkTopic       =   "Form1"
   ScaleHeight     =   3630
   ScaleWidth      =   9165
   StartUpPosition =   2  'CenterScreen
   Begin Project1.UserControl2 UserControl22 
      Height          =   255
      Left            =   3090
      TabIndex        =   7
      Top             =   1470
      Width           =   2535
      _ExtentX        =   4471
      _ExtentY        =   450
   End
   Begin Project1.UserControl2 UserControl23 
      Height          =   255
      Left            =   3090
      TabIndex        =   8
      Top             =   1740
      Width           =   2535
      _ExtentX        =   4471
      _ExtentY        =   450
   End
   Begin Project1.UserControl1 UserControl16 
      Height          =   225
      Left            =   5640
      TabIndex        =   5
      Top             =   1470
      Width           =   450
      _ExtentX        =   794
      _ExtentY        =   397
   End
   Begin Project1.UserControl1 UserControl15 
      Height          =   225
      Left            =   5640
      TabIndex        =   4
      Top             =   1740
      Width           =   450
      _ExtentX        =   794
      _ExtentY        =   397
   End
   Begin Project1.UserControl1 UserControl14 
      Height          =   225
      Left            =   2640
      TabIndex        =   3
      Top             =   2010
      Width           =   450
      _ExtentX        =   794
      _ExtentY        =   397
   End
   Begin Project1.UserControl1 UserControl13 
      Height          =   225
      Left            =   2640
      TabIndex        =   2
      Top             =   1740
      Width           =   450
      _ExtentX        =   794
      _ExtentY        =   397
   End
   Begin Project1.UserControl1 UserControl12 
      Height          =   225
      Left            =   2640
      TabIndex        =   1
      Top             =   1470
      Width           =   450
      _ExtentX        =   794
      _ExtentY        =   397
   End
   Begin Project1.UserControl1 UserControl11 
      Height          =   225
      Left            =   2640
      TabIndex        =   0
      Top             =   1200
      Width           =   450
      _ExtentX        =   794
      _ExtentY        =   397
   End
   Begin VB.Timer Timer1 
      Enabled         =   0   'False
      Interval        =   200
      Left            =   1650
      Top             =   135
   End
   Begin MSCommLib.MSComm MSComm1 
      Left            =   1005
      Top             =   15
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
      DTREnable       =   -1  'True
   End
   Begin Project1.UserControl2 UserControl21 
      Height          =   255
      Left            =   3090
      TabIndex        =   6
      Top             =   2010
      Width           =   2535
      _ExtentX        =   4471
      _ExtentY        =   450
   End
   Begin VB.Label Label6 
      Caption         =   "Red Laser Light"
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   6135
      TabIndex        =   14
      Top             =   1770
      Width           =   1020
   End
   Begin VB.Label Label5 
      Caption         =   "Invisible Red Receiver"
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   6135
      TabIndex        =   13
      Top             =   1500
      Width           =   1515
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      Caption         =   "Infractive Reflective Receiver"
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   660
      TabIndex        =   12
      Top             =   2040
      Width           =   1905
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Caption         =   "Invisible Red Receiver"
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   960
      TabIndex        =   11
      Top             =   1770
      Width           =   1590
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      Caption         =   "Invisible Red Emitter"
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   960
      TabIndex        =   10
      Top             =   1500
      Width           =   1605
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Caption         =   "Infractive Reflective Emitter"
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   195
      Left            =   810
      TabIndex        =   9
      Top             =   1245
      Width           =   1770
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Function OpenPort() As Boolean
    On Error GoTo errorcatch

            
        If MSComm1.PortOpen = True Then
            MSComm1.PortOpen = False
        End If
        If Not MSComm1.PortOpen Then
            MSComm1.CommPort = 6
            MSComm1.Settings = "256000,N,8,1"
            MSComm1.InputLen = 0
            MSComm1.InBufferSize = 1
            MSComm1.OutBufferSize = 1
            MSComm1.PortOpen = True
            Debug.Print "Opened"
        End If

      
        OpenPort = True

    Exit Function
errorcatch:
    OpenPort = False

    Err.Clear
    
End Function

Private Sub ClosePort()
    On Error Resume Next
    If MSComm1.PortOpen Then

        MSComm1.PortOpen = False
                Debug.Print "Closed"
    End If
    Err.Clear
End Sub


Private Sub Form_Load()

    UserControl22.LeftToRight = True
   ' UserControl23.LeftToRight = True

    
    Timer1.Enabled = True
    UserControl11.LeftToRight = True
    UserControl12.LeftToRight = True
    UserControl13.LeftToRight = True
    UserControl14.LeftToRight = True
    
    UserControl11.Color = &HFFFFFF
    UserControl12.Color = &H800000
    UserControl13.Color = &HFFFFFF
    UserControl14.Color = &H4000&

    UserControl15.Color = &HC0&
    UserControl16.Color = &HFFFFFF
    

        
        
    
End Sub

Private Sub Form_Unload(Cancel As Integer)
    Timer1.Enabled = False
    ClosePort
End Sub

Private Sub Timer1_Timer()
    Static data As String

    If MSComm1.PortOpen Then
        data = data & MSComm1.Input

        Dim inline As String

        Do While (InStr(data, Chr(13)) > 0)
            inline = RemoveNextArg(data, Chr(13))
            
            If InStr(inline, "[") > 0 And InStr(inline, "]") > 0 Then
                inline = RemoveQuotedArg(inline, "[", "]")
             '   Debug.Print inline
                On Error Resume Next
                
               
                UserControl12.Enabled = (RemoveNextArg(inline, " ") = "1")
                UserControl23.Value = RemoveNextArg(inline, " ")
    
                UserControl15.Enabled = (RemoveNextArg(inline, " ") = "1")
                UserControl22.Value = RemoveNextArg(inline, " ")
    
                UserControl11.Enabled = (RemoveNextArg(inline, " ") = "1")
                UserControl21.Value = RemoveNextArg(inline, " ")
            End If
            
        Loop
    
    
    Else
        OpenPort
    End If
End Sub

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
Public Function NextQuotedArg(ByVal TheParams As String, Optional ByVal BeginQuote As String = """", Optional ByVal EndQuote As String = """", Optional ByVal Embeded As Boolean = False, Optional ByVal Compare As VbCompareMethod = vbBinaryCompare) As String
    NextQuotedArg = RemoveQuotedArg(TheParams, BeginQuote, EndQuote, Embeded, Compare)
End Function
Public Function RemoveQuotedArg(ByRef TheParams As String, Optional ByVal BeginQuote As String = """", Optional ByVal EndQuote As String = """", Optional ByVal Embeded As Boolean = False, Optional ByVal Compare As VbCompareMethod = vbBinaryCompare) As String
    Dim retval As String
    Dim X As Long
    X = InStr(1, TheParams, BeginQuote, Compare)
    If (X > 0) And (X < Len(TheParams)) Then
        If (InStr(X + Len(BeginQuote), TheParams, EndQuote, Compare) > 0) Then
            If (Not Embeded) Or (EndQuote = BeginQuote) Then
                retval = Mid(TheParams, X + Len(BeginQuote))
                TheParams = Left(TheParams, X - 1) & Mid(retval, InStr(1, retval, EndQuote, Compare) + Len(EndQuote))
                retval = Left(retval, InStr(1, retval, EndQuote, Compare) - 1)
            Else
                Dim L As Long
                Dim Y As Long
                L = 1
                Y = X
                Do Until L = 0
                    If (InStr(Y + Len(BeginQuote), TheParams, BeginQuote, Compare) > 0) And (InStr(Y + Len(BeginQuote), TheParams, BeginQuote, Compare) < InStr(Y + Len(BeginQuote), TheParams, EndQuote, Compare)) Then
                        L = L + 1
                        Y = InStr(Y + Len(BeginQuote), TheParams, BeginQuote, Compare)
                    ElseIf (InStr(Y + Len(BeginQuote), TheParams, EndQuote, Compare) > 0) Then
                        L = L - 1
                        Y = InStr(Y + Len(EndQuote), TheParams, EndQuote, Compare)
                    Else
                        Y = Len(TheParams)
                        L = 0
                    End If
                Loop
                retval = Mid(TheParams, X + Len(BeginQuote))
                TheParams = Left(TheParams, X - 1) & Mid(retval, (Y - X) + Len(EndQuote))
                retval = Left(retval, (Y - X) - 1)
            End If
        End If
    End If
    RemoveQuotedArg = retval
End Function

Private Sub UserControl11_Change()
    MSComm1.Output = "3"
End Sub

Private Sub UserControl12_Change()
    MSComm1.Output = "1"
End Sub

Private Sub UserControl15_Change()
    MSComm1.Output = "2"
End Sub

