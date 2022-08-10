VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Class1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

Implements Class0

Public Sub DoThis()
    Debug.Print "Class1.DoThis() ";
End Sub

Private Sub Class_Initialize()
    'on compiling sometimes errors, so
    'work around for "Set Class0 = Me"
    vbaObjSet Me, ObjPtr(Class0)
End Sub

Private Sub Class0_DoThis()
  Me.DoThis
End Sub

