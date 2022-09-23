VERSION 5.00
Begin VB.UserControl UserControl2 
   ClientHeight    =   1740
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   4800
   ScaleHeight     =   1740
   ScaleWidth      =   4800
   Begin VB.Timer Timer1 
      Interval        =   5
      Left            =   3480
      Top             =   735
   End
   Begin VB.Line Line1 
      BorderColor     =   &H00FFFFFF&
      Visible         =   0   'False
      X1              =   0
      X2              =   0
      Y1              =   0
      Y2              =   240
   End
   Begin VB.Line Line2 
      BorderColor     =   &H00FFFFFF&
      Visible         =   0   'False
      X1              =   4635
      X2              =   4635
      Y1              =   -15
      Y2              =   225
   End
   Begin VB.Line Line3 
      BorderColor     =   &H00FFFFFF&
      Visible         =   0   'False
      X1              =   2640
      X2              =   2640
      Y1              =   0
      Y2              =   240
   End
   Begin VB.Shape Shape1 
      BackColor       =   &H00C00000&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00FFFFFF&
      Height          =   240
      Left            =   1080
      Top             =   735
      Width           =   855
   End
End
Attribute VB_Name = "UserControl2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private Min As Long
Private Max As Long
Private Val As Long
Private pLTR As Boolean

'Private Rerun As Boolean
Private Rate As Long

Private atrate As Long
'Private adjust As Single



Public Property Let LeftToRight(ByVal RHS As Boolean)
    pLTR = RHS
    UserControl_Resize
    
End Property
Public Property Get LeftToRight() As Boolean
    LeftToRight = pLTR
End Property
Public Property Let Color(ByVal RHS As Long)
    Shape1.BackColor = RHS
    Shape1.BorderColor = RHS
End Property


Public Property Get Color() As Long
    Color = Shape1.BorderColor
End Property
Public Property Let Value(ByVal RHS As Long)
    Val = RHS
    
    If Val < Min Then Min = Val
    If Val > Max Then Max = Val
    If Max = 0 Then Exit Property


    
    
    Line3.Y1 = Shape1.Top
    Line3.Y2 = Shape1.Top + Shape1.Height
    If pLTR Then
        Line3.X1 = Shape1.Left + (Val * ((Shape1.Width - (Screen.TwipsPerPixelX * 4)) / Max))
    Else
        Line3.X1 = Shape1.Left + (((-Max - Val) + (Max * 2)) * ((Shape1.Width - (Screen.TwipsPerPixelX * 4)) / Max))
    End If
    Line3.X2 = Line3.X1

'                        If Line3.X1 - Shape1.Left <= Label3.Width Then
'                            Label3.Left = Line3.X1
'                        Else
'                            Label3.Left = Line3.X1 - Label3.Width
'                        End If

    Line3.BorderColor = vbWhite

    If Not Line3.Visible Then Line3.Visible = True
'                        If Not Label3.Visible Then Label3.Visible = True
'                        Label3.Caption = Val
    
    
    Rate = (Timer - Rate)
  '  Debug.Print Rate
   If Rate = 0 Then Exit Property

    Line2.X1 = Line3.X1
    Line2.X2 = Line3.X1
    If pLTR Then
        atrate = ((Shape1.Width / Max) * CLng(Val)) * (1000 / Rate)
    Else
        atrate = ((Shape1.Width / Max) * CLng(((-Max - Val) + (Max * 2)))) * (1000 / Rate)
    End If
    
    If Not Line2.Visible Then Line2.Visible = True
                        
                        
'    Line3.Visible = False
'    Shape1.Visible = False
'
'    Dim unit As Single
'    unit = (Max - Min)
'    Dim pos As Single
'    pos = (RHS - Min)
'
'    If unit > 0 And pos > 0 Then
'
'        Dim loc As Single
'        loc = ((UserControl.Width - Screen.TwipsPerPixelX) * (pos / unit))
'        If pLTR Then
'            Line3.X1 = loc
'            Shape1.Left = 0
'            Shape1.Width = (loc + Screen.TwipsPerPixelX)
'
'        Else
'            Line3.X1 = (-(UserControl.Width - Screen.TwipsPerPixelX) - loc) + ((UserControl.Width - Screen.TwipsPerPixelX) * 2)
'            Shape1.Left = Line3.X1
'            Shape1.Width = (UserControl.Width - Line3.X1)
'
'        End If
'        Line3.X2 = Line3.X1
'    Else
'        Line3.X1 = 0
'        Line3.X2 = 0
'    End If
'    Line3.Visible = True
'    Shape1.Visible = True


                        
End Property

Public Property Get Value() As Long
    Value = Val
End Property

Private Sub Timer1_Timer()
    Static drawing As Boolean

    Static passed As Boolean
    If Rate = 0 Then

        Exit Sub

    End If


    Line2.X1 = Line2.X1 - atrate
    Line2.X2 = Line2.X1



    If Line3.Tag = "2" Then

        Select Case Line3.BorderColor
            Case vbWhite
                Line3.BorderColor = &HFFC0C0
            Case &HFFC0C0
                Line3.BorderColor = &HFF8080


            Case &HFF8080
                Line3.BorderColor = &HFF0000


            Case &HFF0000
                Line3.BorderColor = &HC00000
 

            Case &HC00000
                Line3.BorderColor = &H800000
  

            Case &H800000
                If Line3.Visible Then Line3.Visible = False


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

Private Sub UserControl_Initialize()
    
 ' ' Max = -2048
  ' Min = 2048
End Sub

Private Sub UserControl_Resize()

    Shape1.Top = 0
    Shape1.Left = 0
    Shape1.Height = UserControl.Height - Screen.TwipsPerPixelY
    Shape1.Width = UserControl.Width - Screen.TwipsPerPixelX
    
    Line1.Y1 = 0
    Line2.Y1 = 0
    Line3.Y1 = 0
    
    Line1.X1 = 0
    Line1.X2 = 0

    Line2.X1 = (UserControl.Width - Screen.TwipsPerPixelX)
    Line2.X2 = UserControl.Width - Screen.TwipsPerPixelX
    
    Line2.Y2 = UserControl.Height
    Line1.Y2 = UserControl.Height
    Line3.Y2 = UserControl.Height
    
    Value = Val
    
End Sub
