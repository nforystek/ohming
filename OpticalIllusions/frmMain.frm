VERSION 5.00
Begin VB.Form frmMain 
   AutoRedraw      =   -1  'True
   Caption         =   "Optical Illusions"
   ClientHeight    =   5895
   ClientLeft      =   45
   ClientTop       =   405
   ClientWidth     =   10905
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   ScaleHeight     =   5895
   ScaleWidth      =   10905
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox Picture1 
      AutoRedraw      =   -1  'True
      Height          =   5010
      Left            =   240
      ScaleHeight     =   330
      ScaleMode       =   3  'Pixel
      ScaleWidth      =   330
      TabIndex        =   3
      Top             =   660
      Width           =   5010
      Begin VB.Label Label6 
         BackStyle       =   0  'Transparent
         Caption         =   "4"""
         Height          =   195
         Left            =   2160
         TabIndex        =   6
         Top             =   3540
         Visible         =   0   'False
         Width           =   195
      End
      Begin VB.Label Label4 
         BackStyle       =   0  'Transparent
         Caption         =   "9"""
         Height          =   195
         Left            =   540
         TabIndex        =   5
         Top             =   1920
         Visible         =   0   'False
         Width           =   195
      End
      Begin VB.Label Label5 
         BackStyle       =   0  'Transparent
         Caption         =   "9"""
         Height          =   195
         Left            =   2880
         TabIndex        =   4
         Top             =   2100
         Visible         =   0   'False
         Width           =   195
      End
   End
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   5220
      Top             =   5040
   End
   Begin VB.PictureBox Picture2 
      Height          =   5010
      Left            =   5640
      ScaleHeight     =   4950
      ScaleWidth      =   4950
      TabIndex        =   2
      Top             =   660
      Width           =   5010
      Begin VB.PictureBox Picture3 
         BorderStyle     =   0  'None
         Height          =   915
         Left            =   60
         ScaleHeight     =   915
         ScaleWidth      =   1215
         TabIndex        =   7
         Top             =   4020
         Width           =   1215
         Begin VB.Image Image1 
            Height          =   915
            Left            =   0
            Picture         =   "frmMain.frx":0D4A
            Top             =   0
            Width           =   1215
         End
      End
   End
   Begin VB.Label Label2 
      Caption         =   $"frmMain.frx":116D
      Height          =   660
      Left            =   5610
      TabIndex        =   1
      Top             =   60
      Width           =   5220
   End
   Begin VB.Label Label1 
      Caption         =   $"frmMain.frx":1211
      Height          =   660
      Left            =   210
      TabIndex        =   0
      Top             =   75
      Width           =   5220
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private cnt As Integer

Private Sub Form_Load()
    Set Picture1.Picture = LoadResPicture(99, 0)
    Picture3.Tag = False
    Picture3.Visible = False
    
End Sub

Public Function LargeOf(ByVal V1 As Variant, ByVal V2 As Variant, Optional ByVal V3 As Variant, Optional ByVal V4 As Variant) As Variant
    If IsMissing(V3) Then
        If V1 > V2 Then
            LargeOf = V1
        Else
            LargeOf = V2
        End If
    ElseIf IsMissing(V4) Then
        If V2 > V3 And V2 > V1 Then
            LargeOf = V2
        ElseIf V1 > V3 And V1 > V2 Then
            LargeOf = V1
        Else
            LargeOf = V3
        End If
    Else
        If V2 > V3 And V2 > V1 And V2 > V4 Then
            LargeOf = V2
        ElseIf V1 > V3 And V1 > V2 And V1 > V4 Then
            LargeOf = V1
        ElseIf V3 > V1 And V3 > V2 And V3 > V4 Then
            LargeOf = V3
        Else
            LargeOf = V4
        End If
    End If
End Function

Public Function LeastOf(ByVal V1 As Variant, ByVal V2 As Variant, Optional ByVal V3 As Variant, Optional ByVal V4 As Variant) As Variant
    If IsMissing(V3) Then
        If V1 < V2 Then
            LeastOf = V1
        Else
            LeastOf = V2
        End If
    ElseIf IsMissing(V4) Then
        If V2 < V3 And V2 < V1 Then
            LeastOf = V2
        ElseIf V1 < V3 And V1 < V2 Then
            LeastOf = V1
        Else
            LeastOf = V3
        End If
    Else
        If V2 < V3 And V2 < V1 And V2 < V4 Then
            LeastOf = V2
        ElseIf V1 < V3 And V1 < V2 And V1 < V4 Then
            LeastOf = V1
        ElseIf V3 < V1 And V3 < V2 And V3 < V4 Then
            LeastOf = V3
        Else
            LeastOf = V4
        End If
    End If
End Function


Private Sub Picture1_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)

    If Button = 1 Then

        If Label4.Visible Then

            Label4.Visible = False
            Label5.Visible = False
            Label6.Visible = False
            Set Picture1.Picture = LoadResPicture(99, 0)
            
        Else
        
            Label4.Tag = (((LargeOf(Label4.Left + (Label4.Width / 2), X) - LeastOf(Label4.Left + (Label4.Width / 2), X)) + _
                         (LargeOf(Label4.Top + (Label4.Height / 2), Y) - LeastOf(Label4.Top + (Label4.Height / 2), Y))) / 2)

            Label5.Tag = (((LargeOf(Label5.Left + (Label5.Width / 2), X) - LeastOf(Label5.Left + (Label5.Width / 2), X)) + _
                         (LargeOf(Label5.Top + (Label5.Height / 2), Y) - LeastOf(Label5.Top + (Label5.Height / 2), Y))) / 2)
            
            Label6.Tag = (((LargeOf(Label6.Left + (Label6.Width / 2), X) - LeastOf(Label6.Left + (Label6.Width / 2), X)) + _
                         (LargeOf(Label6.Top + (Label6.Height / 2), Y) - LeastOf(Label6.Top + (Label6.Height / 2), Y))) / 2)
                         

            If ((Label4.Tag <= Label5.Tag) And (Label4.Tag <= Label6.Tag)) Then
                'label4
                Set Picture1.Picture = LoadResPicture(100, 0)
                Label4.Caption = "9"""
                Label5.Caption = "4"""
                Label6.Caption = "9"""
            ElseIf ((Label5.Tag <= Label4.Tag) And (Label5.Tag <= Label6.Tag)) Then
                'label5
                Set Picture1.Picture = LoadResPicture(101, 0)
                Label4.Caption = "9"""
                Label5.Caption = "9"""
                Label6.Caption = "4"""
            ElseIf Label6.Tag <= Label4.Tag And Label6.Tag <= Label5.Tag Then
                'label6
                Set Picture1.Picture = LoadResPicture(102, 0)
                Label4.Caption = "4"""
                Label5.Caption = "9"""
                Label6.Caption = "9"""
            End If
    
            Label4.Visible = True
            Label5.Visible = True
            Label6.Visible = True
        End If
    End If
End Sub

Private Sub Picture2_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        If X > (Picture2.Width / 2) Then
            Picture3.Tag = True
            Picture3.Visible = True
        
        Else
            Picture3.Tag = False
            Picture3.Visible = True
        
        End If
    
    End If
End Sub

Private Sub Timer1_Timer()
    cnt = cnt + 1
    Set Picture2.Picture = LoadResPicture(102 + cnt, 0)
    
    Static dir As Boolean

    If Picture3.Tag = "False" Then
        Picture3.Picture = LoadResPicture(126 + ((-24 - cnt) + (24 * 2)), 0)
    Else
        Picture3.Picture = LoadResPicture(126 + cnt, 0)
    End If
    Randomize

    If cnt = 24 Then cnt = 0

End Sub
