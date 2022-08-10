Attribute VB_Name = "Module1"
Option Explicit

Public Sub Main()
    'I wrote this for speed in iteration of a collection instead of housing unapplied modifiers externalls,
    'they are stacked as self objects on the one they all will be later applied to in also disposed of, the
    'specific applicationis if the value property below was a rotation, and I apply more but not immediate
    'and need a bottomless enough amount of unapplied rotations, until I combine then into the first rotate
    
    Debug.Print "Stack Example"
    
    Dim stack As New IStack 'create a first object
    stack.Value = 1 'has some unqiue value
    Debug.Print "Add 1"
    
    Dim stack2 As New IStack 'create a second object
    stack2.Value = 2 'has some unique value
    stack.Add stack2 'stack on the first object
    Debug.Print "Add 2"
    
    Set stack2 = New IStack 'create a third object
    stack2.Value = 3 'has some unique value
    stack.Add stack2 'stack on the first object
    Debug.Print "Add 3"
    
    Set stack2 = New IStack 'create a fourth object
    stack2.Value = 4 'has some unique value
    stack.Add stack2 'stack on the first object
    Debug.Print "Add 4"
    
    Set stack2 = Nothing 'loose the reference too

    Do While stack.NotEmpty 'check if stack is empty
        'if not, then proceed to get the stack obj
        Set stack2 = stack.Del 'this also removes it
        Debug.Print "Del " & stack2.Value 'display it's value
    Loop
    Debug.Print "Del " & stack.Value 'display the first's value
    
    Set stack = Nothing 'clean up
    
    
    Debug.Print "Queue Example"
    
    Dim queue As New IQueue 'create a first object
    queue.Value = 1 'has some unqiue value
    Debug.Print "Add 1"
    
    Dim queue2 As New IQueue 'create a second object
    queue2.Value = 2 'has some unique value
    queue.Add queue2 'stack on the first object
    Debug.Print "Add 2"
    
    Set queue2 = New IQueue 'create a third object
    queue2.Value = 3 'has some unique value
    queue.Add queue2 'stack on the first object
    Debug.Print "Add 3"
    
    Set queue2 = New IQueue 'create a fourth object
    queue2.Value = 4 'has some unique value
    queue.Add queue2 'stack on the first object
    Debug.Print "Add 4"
    
    Set queue2 = Nothing 'loose the reference too

    Do While queue.NotEmpty
        Debug.Print "Del " & queue.Value 'display the first's value
        
        'if not, then proceed to get the stack obj
        Set queue = queue.Del 'this also removes it

    Loop
    
    
    Set queue = Nothing 'clean up

End Sub
