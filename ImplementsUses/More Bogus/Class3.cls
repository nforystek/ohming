VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Class3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements Class1

Private Sub Class_Initialize()
    Set Class1 = New Class1
End Sub

Public Sub DoThis()
    Debug.Print "Class3.DoThis() ";
    Class1.DoThis
End Sub

Public Sub DoThat()
    Debug.Print "Class3.DoThat() ";
    Class1_DoThat
End Sub

Private Sub Class1_DoThis()
    Debug.Print "Class3.Class1_DoThis() ";
End Sub

Private Sub Class1_DoThat()
    Debug.Print "Class3.Class1_DoThat() ";
    Class1.DoThat
End Sub

