VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Point"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements Coord

Private pZ As Single

Public Property Get X() As Single
    X = Coord.X
End Property
Public Property Let X(ByVal RHS As Single)
    Coord.X = RHS
End Property

Public Property Get Y() As Single
    Y = Coord.Y
End Property
Public Property Let Y(ByVal RHS As Single)
    Coord.Y = RHS
End Property

Public Property Get Z() As Single
    Z = pZ
End Property
Public Property Let Z(ByVal RHS As Single)
    pZ = RHS
End Property

Public Function Equals(ByRef Point As Point) As Boolean
    Equals = (Coord.Equals(Point) And Point.Z = Z)
End Function

Public Property Get Serialize() As String
Attribute Serialize.VB_UserMemId = 0
Attribute Serialize.VB_MemberFlags = "240"
    Serialize = "[" & Round(X, 6) & ", " & Round(Y, 6) & ", " & Round(Z, 6) & "]"
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
        If InStr(RHS, ",") > 0 Then
            Z = CSng(Left(RHS, InStr(RHS, ",") - 1))
            RHS = Mid(RHS, InStr(RHS, ",") + 1)
        End If
    End If
End Property

Private Sub Class_Initialize()
    vbaObjSetAddref Coord, ObjPtr(New Coord)

End Sub

Private Function Coord_Equals(Point As Point) As Boolean
    Coord_Equals = Me.Equals(Point)
End Function

Private Property Let Coord_Serialize(ByVal RHS As String)
    Me.Serialize = RHS
End Property

Private Property Get Coord_Serialize() As String
    Coord_Serialize = Me.Serialize
End Property

Private Property Let Coord_X(ByVal RHS As Single)
    Me.X = RHS
End Property

Private Property Get Coord_X() As Single
    Coord_X = Me.X
End Property

Private Property Let Coord_Y(ByVal RHS As Single)
    Me.Y = RHS
End Property

Private Property Get Coord_Y() As Single
    Coord_X = Me.Y
End Property
