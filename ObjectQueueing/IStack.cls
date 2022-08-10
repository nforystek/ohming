VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IStack"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Implements IUnknown

Public Value As Long

Private List As IStack

Public Property Get NotEmpty() As Boolean
    NotEmpty = Not (List Is Nothing)
End Property

Public Sub Add(ByRef RHS As IStack)
    If List Is Nothing Then
        Set List = RHS
    Else
        List.Add RHS
    End If
End Sub

Public Function Del() As IStack
    If List Is Nothing Then
        Set Del = Nothing
    Else
        Set Del = List.Del
        If Del Is Nothing Then
            Set Del = List
            Set List = Nothing
        End If
    End If
End Function
