Attribute VB_Name = "Module1"
Option Explicit

Public Sub Main()

    Debug.Print "Call Stack Debug Output of Implements (which is only similar to inheritance and interface but non equal to either)"
    Debug.Print
    Debug.Print "Class1 Example - No Implements just simple calling functions."
    Dim Class1 As New Class1
    Debug.Print "    Call Class1.DoThis() Triggers [ ";
    Class1.dothis
    Debug.Print "]"
    Debug.Print "    Call Class1.DoThat() Triggers [ ";
    Class1.dothat
    Debug.Print "]"
    Test1 Class1
    Set Class1 = Nothing
    Debug.Print
    

    Debug.Print "Class2 Example - Basic Implements late shadowing calling base class."
    Dim class2 As New class2
    Debug.Print "    Call Class2.DoThis() Triggers [ ";
    class2.dothis
    Debug.Print "]"
    Debug.Print "    Call Class2.DoThat() Triggers [ ";
    class2.dothat
    Debug.Print "]"
    Test1 class2
    Test2 class2
    Set class2 = Nothing
    Debug.Print

    Debug.Print "Class3 Example - Shared Implements on interface with selective late shadow calling."
    Dim class3 As New class3
    Debug.Print "    Call Class3.DoThis() Triggers [ ";
    class3.dothis
    Debug.Print "]"
    Debug.Print "    Call Class3.DoThat() Triggers [ ";
    class3.dothat
    Debug.Print "]"
    Test1 class3
    Test3 class3
    Set class3 = Nothing
    Debug.Print

    Debug.Print "Class4 Example - Shared Implements like class3 with different selection of shadow calling."
    Dim class4 As New class4
    Debug.Print "    Call Class4.DoThis() Triggers [ ";
    class4.dothis
    Debug.Print "]"
    Debug.Print "    Call Class4.DoThat() Triggers [ ";
    class4.dothat
    Debug.Print "]"
    Test1 class4
    Test4 class4
    Set class4 = Nothing
    Debug.Print

    Debug.Print "Class5 Example - Casting Implements of late and early calling of the base class shadowed."
    Dim class5 As New class5
    Debug.Print "    Call Class5.DoThis() Triggers [ ";
    class5.dothis
    Debug.Print "]"
    Debug.Print "    Call Class5.DoThat() Triggers [ ";
    class5.dothat
    Debug.Print "]"
    Test1 class5
    Test5 class5
    Set class5 = Nothing
    Debug.Print

    Debug.Print "Class6 Example - Equality Implements of late shadow calling forward to a backward class."
    Dim Class6 As New Class6
    Debug.Print "    Call Class6.DoThis() Triggers [ ";
    Class6.dothis
    Debug.Print "]"
    Debug.Print "    Call Class6.DoThat() Triggers [ ";
    Class6.dothat
    Debug.Print "]"
    Test6 Class6
    Test7 Class6
    Set Class6 = Nothing
    Debug.Print

    Debug.Print "Class7 Example - Equality Implements of late shadow calling backward to a forward class."
    Dim Class7 As New Class7
    Debug.Print "    Call Class7.DoThis() Triggers [ ";
    Class7.dothis
    Debug.Print "]"
    Debug.Print "    Call Class7.DoThat() Triggers [ ";
    Class7.dothat
    Debug.Print "]"
    Test6 Class7
    Test7 Class7
    Set Class7 = Nothing
    Debug.Print

    Debug.Print "Class8 Example - Wooven Implements using property shadowing that early and late in stack via base class."
    Dim class8 As New class8
    Debug.Print "    Call Class8.DoThat = Class8.DoThis Triggers [ ";
    class8.dothis = class8.dothat
    Debug.Print "]"
    Test0 class8
    Test8 class8
    Set class8 = Nothing
    Debug.Print

    Debug.Print "Class9 Example - Wooven Implements using property shadowing that stacks on a wooven base class."
    Dim class9 As New class9
    Debug.Print "    Call Class9.DoThat = Class9.DoThis Triggers [ ";
    class9.dothis = class9.dothat
    Debug.Print "]"
    Test8 class9
    Test9 class9
    Set class9 = Nothing
    Debug.Print
End Sub


Public Sub Test0(ByRef Class As class0)
    Debug.Print "    Passed " & TypeName(Class) & " As Class0"
    Debug.Print "        Call Class.DoThat = Class.DoThis Triggers [ ";
    Class.dothat = Class.dothis
    Debug.Print "]"
End Sub

Public Sub Test1(ByRef Class As Class1)
    Debug.Print "    Passed " & TypeName(Class) & " As Class1"
    Debug.Print "        Call Class.DoThis() Triggers [ ";
    Class.dothis
    Debug.Print "]"
    Debug.Print "        Call Class.DoThat() Triggers [ ";
    Class.dothat
    Debug.Print "]"
End Sub
Public Sub Test2(ByRef Class As class2)
    Debug.Print "    Passed " & TypeName(Class) & " As Class2"
    Debug.Print "        Call Class.DoThis() Triggers [ ";
    Class.dothis
    Debug.Print "]"
    Debug.Print "        Call Class.DoThat() Triggers [ ";
    Class.dothat
    Debug.Print "]"
End Sub
Public Sub Test3(ByRef Class As class3)
    Debug.Print "    Passed " & TypeName(Class) & " As Class3"
    Debug.Print "        Call Class.DoThis() Triggers [ ";
    Class.dothis
    Debug.Print "]"
    Debug.Print "        Call Class.DoThat() Triggers [ ";
    Class.dothat
    Debug.Print "]"
End Sub
Public Sub Test4(ByRef Class As class4)
    Debug.Print "    Passed " & TypeName(Class) & " As Class4"
    Debug.Print "        Call Class.DoThis() Triggers [ ";
    Class.dothis
    Debug.Print "]"
    Debug.Print "        Call Class.DoThat() Triggers [ ";
    Class.dothat
    Debug.Print "]"
End Sub
Public Sub Test5(ByRef Class As class5)
    Debug.Print "    Passed " & TypeName(Class) & " As Class5"
    Debug.Print "        Call Class.DoThis() Triggers [ ";
    Class.dothis
    Debug.Print "]"
    Debug.Print "        Call Class.DoThat() Triggers [ ";
    Class.dothat
    Debug.Print "]"
End Sub

Public Sub Test6(ByRef Class As Class6)
    Debug.Print "    Passed " & TypeName(Class) & " As Class6"
    Debug.Print "        Call Class.DoThis() Triggers [ ";
    Class.dothis
    Debug.Print "]"
    Debug.Print "        Call Class.DoThat() Triggers [ ";
    Class.dothat
    Debug.Print "]"
End Sub

Public Sub Test7(ByRef Class As Class7)
    Debug.Print "    Passed " & TypeName(Class) & " As Class7"
    Debug.Print "        Call Class.DoThis() Triggers [ ";
    Class.dothis
    Debug.Print "]"
    Debug.Print "        Call Class.DoThat() Triggers [ ";
    Class.dothat
    Debug.Print "]"
End Sub

Public Sub Test8(ByRef Class As class8)
    Debug.Print "    Passed " & TypeName(Class) & " As Class8"
    Debug.Print "        Call Class.DoThat = Class.DoThis Triggers [ ";
    Class.dothis = Class.dothat
    Debug.Print "]"
End Sub

Public Sub Test9(ByRef Class As class9)
    Debug.Print "    Passed " & TypeName(Class) & " As Class9"
    Debug.Print "        Call Class.DoThat = Class.DoThis Triggers [ ";
    Class.dothis = Class.dothat
    Debug.Print "]"
End Sub

