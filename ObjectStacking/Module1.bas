Attribute VB_Name = "Module1"
Option Explicit

Public Sub Main()
    'I wrote this for speed in iteration of a collection instead of housing unapplied modifiers externalls,
    'they are stacked as self objects on the one they all will be later applied to in also disposed of, the
    'specific applicationis if the value property below was a rotation, and I apply more but not immediate
    'and need a bottomless enough amount of unapplied rotations, until I combine then into the first rotate
    
    Dim obj As New IStack 'create a first object
    obj.Value = 1 'has some unqiue value
    
    Dim obj2 As New IStack 'create a second object
    obj2.Value = 2 'has some unique value
    obj.StackAdd obj2 'stack on the first object
    
    Set obj2 = New IStack 'create a third object
    obj2.Value = 3 'has some unique value
    obj.StackAdd obj2 'stack on the first object
    
    Set obj2 = New IStack 'create a fourth object
    obj2.Value = 4 'has some unique value
    obj.StackAdd obj2 'stack on the first object
    
    Set obj2 = Nothing 'loose the reference too

    Do While obj.NotEmpty 'check if stack is empty
        'if not, then proceed to get the stack obj
        Set obj2 = obj.StackDel 'this also removes it
        Debug.Print obj2.Value 'display it's value
    Loop
    Debug.Print obj.Value 'display the first's value
    
    Set obj = Nothing 'clean up

End Sub
