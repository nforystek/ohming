VERSION 5.00
Begin VB.Form frmMain 
   AutoRedraw      =   -1  'True
   Caption         =   "Wave View"
   ClientHeight    =   5775
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   15930
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   OLEDropMode     =   1  'Manual
   ScaleHeight     =   5775
   ScaleWidth      =   15930
   StartUpPosition =   2  'CenterScreen
   Begin Audioscopic.WaveView WaveView1 
      Height          =   2355
      Left            =   1140
      TabIndex        =   0
      TabStop         =   0   'False
      Top             =   240
      Width           =   4875
      _ExtentX        =   8599
      _ExtentY        =   4154
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Form_Resize()
    On Error Resume Next
    
    WaveView1.Left = 0
    WaveView1.Top = 0
    
    WaveView1.Width = Me.ScaleWidth
    WaveView1.Height = Me.ScaleHeight

    If err Then err.Clear
End Sub

