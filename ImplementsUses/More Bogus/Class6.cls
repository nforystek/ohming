VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Class6"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements Class7

Private Sub Class_Initialize()
    Set Class7 = Me
End Sub

Public Sub DoThis()
    Debug.Print "Class6.DoThis() ";
    Class7.DoThis
End Sub

Public Sub DoThat()
    Debug.Print "Class6.DoThat() ";
    Class7.DoThat
End Sub

Private Sub Class7_DoThis()
    Debug.Print "Class6.Class7_DoThis() ";
End Sub

Private Sub Class7_DoThat()
    Debug.Print "Class6.Class7_DoThat() ";
End Sub
