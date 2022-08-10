VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Coord"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Private pX As Single
Private pY As Single

Public Property Get X() As Single
    X = pX
End Property
Public Property Let X(ByVal RHS As Single)
    pX = RHS
End Property

Public Property Get Y() As Single
    Y = pY
End Property
Public Property Let Y(ByVal RHS As Single)
    pY = RHS
End Property

Public Function Equals(ByRef Point As Point) As Boolean
    Equals = (Point.X = X And Point.Y = Y)
End Function

Public Property Get Serialize() As String
Attribute Serialize.VB_UserMemId = 0
Attribute Serialize.VB_MemberFlags = "200"
    Serialize = "[" & Round(X, 6) & ", " & Round(Y, 6) & "]"
End Property
Public Property Let Serialize(ByVal RHS As String)
    RHS = Trim(RHS)
    If Left(RHS, 1) = "[" And Right(RHS, 1) = "]" Then
        RHS = Mid(RHS, 2, Len(RHS) - 2)
        If InStr(RHS, ",") > 0 Then
            X = CSng(Left(RHS, InStr(RHS, ",") - 1))
            RHS = Mid(RHS, InStr(RHS, ",") + 1)
        End If
        If InStr(RHS, ",") > 0 Then
            Y = CSng(Left(RHS, InStr(RHS, ",") - 1))
            RHS = Mid(RHS, InStr(RHS, ",") + 1)
        End If
    End If
End Property


