VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Class8"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements Class0

Private Sub Class_Initialize()
    Set Class0 = Me
End Sub

Public Property Let DoThis(RHS As Variant)
    Debug.Print "Class8.DoThis ";
    Class0.DoThat = RHS
End Property

Public Property Get DoThat() As Variant
    Debug.Print "Class8.DoThat ";
    Me.DoThis = Class0.DoThis
End Property

Private Property Get Class0_DoThis() As Variant
    Debug.Print "Class8.Class0_DoThis ";
End Property

Private Property Let Class0_DoThat(RHS As Variant)
    Debug.Print "Class8.Class0_DoThat ";
End Property

