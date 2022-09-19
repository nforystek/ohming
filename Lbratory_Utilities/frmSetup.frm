VERSION 5.00
Begin VB.Form frmSetup 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Temperature and Sonic Cherp Settings"
   ClientHeight    =   2250
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   7050
   Icon            =   "frmSetup.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2250
   ScaleWidth      =   7050
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Visible         =   0   'False
   Begin VB.CommandButton Command2 
      Caption         =   "&Cancel"
      Height          =   375
      Left            =   5880
      TabIndex        =   12
      Top             =   1800
      Width           =   1095
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&OK"
      Height          =   375
      Left            =   4680
      TabIndex        =   11
      Top             =   1800
      Width           =   1095
   End
   Begin VB.Frame Frame4 
      Caption         =   "Settings"
      Height          =   1755
      Left            =   60
      TabIndex        =   0
      Top             =   0
      Width           =   6915
      Begin VB.TextBox Text4 
         Alignment       =   2  'Center
         Height          =   285
         Left            =   6060
         TabIndex        =   6
         Text            =   "3000"
         Top             =   960
         Width           =   675
      End
      Begin VB.TextBox Text3 
         Alignment       =   2  'Center
         Height          =   285
         Left            =   4560
         TabIndex        =   5
         Text            =   "20"
         Top             =   960
         Width           =   675
      End
      Begin VB.TextBox Text2 
         Alignment       =   2  'Center
         Height          =   285
         Left            =   6120
         TabIndex        =   4
         Text            =   "83"
         Top             =   240
         Width           =   615
      End
      Begin VB.TextBox Text1 
         Alignment       =   2  'Center
         Height          =   285
         Left            =   3360
         TabIndex        =   3
         Text            =   "78"
         Top             =   240
         Width           =   555
      End
      Begin VB.TextBox Text5 
         Height          =   285
         Left            =   3060
         TabIndex        =   2
         Text            =   "9524579224@tmomail.net"
         Top             =   1320
         Width           =   3675
      End
      Begin VB.TextBox Text6 
         Height          =   285
         Left            =   3060
         TabIndex        =   1
         Text            =   "C:\Development\Projects\Securities\alarm.wav"
         Top             =   600
         Width           =   3675
      End
      Begin VB.Label Label6 
         Caption         =   "Send text messages to my e-mail if the sonic range falls below                   or above "
         Height          =   195
         Left            =   120
         TabIndex        =   10
         Top             =   1020
         Width           =   6735
      End
      Begin VB.Label Label5 
         Caption         =   "Sound an alarm if the temperature falls below                after it has reached a high of"
         Height          =   195
         Left            =   120
         TabIndex        =   9
         Top             =   300
         Width           =   6675
      End
      Begin VB.Label Label7 
         Caption         =   "Location of sound file to use for alarm:"
         Height          =   195
         Left            =   120
         TabIndex        =   8
         Top             =   660
         Width           =   2775
      End
      Begin VB.Label Label8 
         Caption         =   "E-mail address to send the message to:"
         Height          =   255
         Left            =   120
         TabIndex        =   7
         Top             =   1380
         Width           =   2775
      End
   End
End
Attribute VB_Name = "frmSetup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Command1_Click()
    If (IsNumeric(Text1.Text) And IsNumeric(Text2.Text)) And (IsNumeric(Text3.Text) And IsNumeric(Text4.Text)) Then
        If PathExists(Text6.Text, True) Then
            If (InStr(Text5.Text, "@") > 1 And InStr(Text5.Text, "@") < Len(Text5.Text)) Then
            
                frmMain.svce.Send "yalarm=" & Text1.Text & "-" & Text2.Text & ";range=" & Text3.Text & "-" & Text4.Text & ";email=" & Text5.Text & ";sound=" & Text6.Text & ";" & vbCrLf
                Me.Hide
            Else
                MsgBox "Invalid email, please provide a valid address."
            End If
        Else
            MsgBox "Invalid sound file, please specify a valid path."
        End If
    Else
        MsgBox "Invalid range, youmust specify numerical values only."
    End If
    

End Sub

Private Sub Command2_Click()
Me.Hide
End Sub
