VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Class9"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements Class8

Private Sub Class_Initialize()
    Set Class8 = Me
End Sub

Public Property Let DoThis(RHS As Variant)
    Debug.Print "Class9.DoThis ";
    Class8.DoThis = RHS
End Property

Public Property Get DoThat() As Variant
    Debug.Print "Class9.DoThat ";
    Me.DoThis = Class8.DoThat
End Property

Private Property Get Class8_DoThat() As Variant
    Debug.Print "Class9.Class8_DoThat ";
End Property

Private Property Let Class8_DoThis(RHS As Variant)
    Debug.Print "Class9.Class8_DoThis ";
End Property
