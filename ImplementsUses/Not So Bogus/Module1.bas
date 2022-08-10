Attribute VB_Name = "Module1"
Option Explicit

Public Sub Test0(ByRef Class0 As Class0)
    Debug.Print "Passed " & TypeName(Class0) & " As Class0"
    Debug.Print "   Calling Class0.DoThis() -> ";
    Class0.dothis
    Debug.Print
End Sub

Public Sub Test1(ByRef Class1 As Class1)
    Debug.Print "Passed " & TypeName(Class1) & " As Class1"
    Debug.Print "   Calling Class1.DoThis() -> ";
    Class1.dothis
    Debug.Print
End Sub

Public Sub Test2(ByRef Class2 As Class2)
    Debug.Print "Passed " & TypeName(Class2) & " As Class2"
    Debug.Print "   Calling Class2.DoThis() -> ";
    Class2.dothis
    Debug.Print
End Sub

Public Sub Test3(ByRef Class3 As Class3)
    Debug.Print "Passed " & TypeName(Class3) & " As Class3"
    Debug.Print "   Calling Class3.DoThis() -> ";
    Class3.dothis
    Debug.Print
End Sub

Public Sub Main()

    Debug.Print "Call Stack Debug Output"
    Debug.Print
    Debug.Print "Class1 Example (Only uses the implementing class with a base class as it's interface)"
    Dim Class1 As New Class1
    Debug.Print "   Calling Class1.DoThis() -> ";
    Class1.dothis
    Debug.Print
    Test0 Class1
    Test1 Class1
    Set Class1 = Nothing
    Debug.Print

    Debug.Print "Class2 Example (Makes use of both the base class and implementing class with same interface)"
    Dim Class2 As New Class2
    Debug.Print "   Calling Class2.DoThis() -> ";
    Class2.dothis
    Debug.Print
    Test0 Class2
    Test2 Class2
    Set Class2 = Nothing
    Debug.Print

    Debug.Print "Class3 Example (Excludes the implmenting class from base class when the base class is used)"
    Dim Class3 As New Class3
    Debug.Print "   Calling Class3.DoThis() -> ";
    Class3.dothis
    Debug.Print
    Test0 Class3
    Test3 Class3
    Set Class3 = Nothing
    Debug.Print

End Sub
