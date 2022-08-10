VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "IQueue"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Implements IUnknown

Private Declare Function vbaObjSet Lib "msvbvm60.dll" Alias "__vbaObjSet" (dstObject As Any, ByVal srcObjPtr As Long) As Long

Public Value As Long

Private List As IQueue

Public Property Get NotEmpty() As Boolean
    NotEmpty = Not (List Is Nothing)
End Property


Public Function Add(ByRef RHS As IQueue) As IQueue
    If List Is Nothing Then
       ' Set RHS = Me
       Set Add = Me
       
        Set List = Nothing
        
    Else
        Set Add = RHS
        Set List = List.Add(List)
        
      '  Set List = List.Add(RHS)
    End If

End Function

Public Function Del() As IQueue
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

