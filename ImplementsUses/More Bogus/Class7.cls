VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Class7"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements Class6

Private Sub Class_Initialize()
    Set Class6 = Me
End Sub

Public Sub DoThis()
    Debug.Print "Class7.DoThis() ";
    Class6.DoThis
End Sub

Public Sub DoThat()
    Debug.Print "Class7.DoThat() ";
    Class6.DoThat
End Sub

Private Sub Class6_DoThis()
    Debug.Print "Class7.Class6_DoThis() ";
End Sub

Private Sub Class6_DoThat()
    Debug.Print "Class7.Class6_DoThat() ";
End Sub
