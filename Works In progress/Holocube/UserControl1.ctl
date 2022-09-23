VERSION 5.00
Begin VB.UserControl UserControl1 
   ClientHeight    =   1815
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   3855
   ScaleHeight     =   1815
   ScaleWidth      =   3855
   Begin VB.Shape Shape1 
      BackColor       =   &H008080FF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00000000&
      Height          =   225
      Left            =   0
      Top             =   0
      Width           =   225
   End
   Begin VB.Line Line3 
      BorderColor     =   &H0080FFFF&
      Visible         =   0   'False
      X1              =   1335
      X2              =   1800
      Y1              =   345
      Y2              =   840
   End
   Begin VB.Line Line2 
      BorderColor     =   &H0080FFFF&
      Visible         =   0   'False
      X1              =   765
      X2              =   1410
      Y1              =   480
      Y2              =   840
   End
   Begin VB.Line Line1 
      BorderColor     =   &H0080FFFF&
      Visible         =   0   'False
      X1              =   540
      X2              =   1080
      Y1              =   570
      Y2              =   975
   End
   Begin VB.Shape Shape2 
      BackColor       =   &H008080FF&
      BackStyle       =   1  'Opaque
      BorderColor     =   &H00000000&
      Height          =   225
      Left            =   0
      Shape           =   3  'Circle
      Top             =   0
      Width           =   450
   End
End
Attribute VB_Name = "UserControl1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Private pLTR As Boolean
Public Event Change()

Public Property Let Enabled(ByVal RHS As Boolean)
    Line1.Visible = RHS
    Line2.Visible = RHS
    Line3.Visible = RHS
    
End Property
Public Property Get Enabled() As Boolean
    Enabled = Line1.Visible
End Property
Public Property Let LeftToRight(ByVal RHS As Boolean)
    pLTR = RHS
    UserControl_Resize
    
End Property
Public Property Get LeftToRight() As Boolean
    LeftToRight = pLTR
End Property
Public Property Let Color(ByVal RHS As Long)
    Shape1.BackColor = RHS
    Shape2.BackColor = RHS
End Property

Private Sub UserControl_Click()

    RaiseEvent Change
End Sub



Private Sub UserControl_Initialize()
    UserControl_Resize
End Sub

Private Sub UserControl_Resize()
    Shape1.Top = 0
    Shape2.Top = 0
    If pLTR Then
        Shape1.Left = 0
    Else
        Shape1.Left = 225
    End If
    UserControl.Width = 225 * 2
    
    UserControl.Height = 225
    Shape1.Height = 225
    Shape1.Width = 225
    
    Shape2.Height = 225
    Shape2.Width = 225 * 2
    
    Line1.X1 = 0
    Line1.X2 = UserControl.Width
    Line2.X1 = 0
    Line2.X2 = UserControl.Width
    Line3.X1 = 0
    Line3.X2 = UserControl.Width
    
    
    Line1.Y1 = 0
    Line1.Y2 = UserControl.Height
    

    
    Line2.Y1 = UserControl.Height / 2
    Line2.Y2 = UserControl.Height / 2

    
    Line3.Y1 = UserControl.Height
    Line3.Y2 = 0
End Sub
