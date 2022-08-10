Attribute VB_Name = "Module1"
Option Explicit

Public Sub Main()

    Dim pid As New PoolID
    pid.Pacerate = NoTiming
    Do While True
    
    
        Debug.Print pid.Generate
        DoEvents
        
    Loop
End Sub
