Attribute VB_Name = "modGeometry"
Option Explicit

Option Compare Binary

Public Const PI As Single = 3.14159265358979
Public Const Epsilon As Double = 0.999999999999999
Public Const D90 As Single = PI / 4
Public Const D180 As Single = PI / 2
Public Const D360 As Single = PI
Public Const D720 As Single = PI * 2
Public Const DEGREE As Single = 180 / PI
Public Const RADIAN As Single = PI / 180
Public Const FOOT As Single = 0.1
Public Const MILE As Single = 5280 * FOOT
Public Const FOVY As Single = (FOOT * 8) '4 feet left, and 4 feet right = 0.8
Public Const FAR  As Single = 900000000
Public Const near As Single = 0.05 'one millimeter (308.4 per foor) or greater

Public Function MakeVector(ByVal X As Single, ByVal Y As Single, ByVal Z As Single) As D3DVECTOR
    MakeVector.X = X
    MakeVector.Y = Y
    MakeVector.Z = Z
End Function

Public Function MakePoint(ByVal X As Single, ByVal Y As Single, ByVal Z As Single) As Point
    Set MakePoint = New Point
    MakePoint.X = X
    MakePoint.Y = Y
    MakePoint.Z = Z
End Function

Public Function MakeCoord(ByVal X As Single, ByVal Y As Single) As Coord
    Set MakeCoord = New Coord
    MakeCoord.X = X
    MakeCoord.Y = Y
End Function

Public Function ToCoord(ByRef Vector As D3DVECTOR) As Coord
    Set ToCoord = New Coord
    ToCoord.X = Vector.X
    ToCoord.Y = Vector.Y
End Function

Public Function ToVector(ByRef Point As Point) As D3DVECTOR
    ToVector.X = Point.X
    ToVector.Y = Point.Y
    ToVector.Z = Point.Z
End Function

Public Function ToPoint(ByRef Vector As D3DVECTOR) As Point
    Set ToPoint = New Point
    ToPoint.X = Vector.X
    ToPoint.Y = Vector.Y
    ToPoint.Z = Vector.Z
End Function

Public Function Distance(ByVal p1x As Single, ByVal p1y As Single, ByVal p1z As Single, ByVal p2x As Single, ByVal p2y As Single, ByVal p2z As Single) As Single
    Distance = ((((p2x - p1x) ^ 2) + ((p2y - p1y) ^ 2) + ((p2z - p1z) ^ 2)) ^ (1 / 2))
End Function

Public Function DistanceEx(ByRef p1 As Point, ByRef p2 As Point) As Single
    DistanceEx = ((((p1.X - p2.X) ^ 2) + ((p1.Y - p2.Y) ^ 2) + ((p1.Z - p2.Z) ^ 2)) ^ (1 / 2))
End Function

Public Function DistanceSet(ByRef p1 As Point, ByVal p2 As Point, ByVal n As Single) As Point
    Dim dist As Single
    dist = DistanceEx(p1, p2)
    Set DistanceSet = LineNormalize(p2, p1)
    With DistanceSet
        If Not (dist = n) Then
            If ((dist > 0) And (n > 0)) Then
'            dist = (((n - dist) / (dist / n)) / n)
'            .X = p2.X + ((p2.X - p1.X) * dist)
'            .Y = p2.Y + ((p2.Y - p1.Y) * dist)
'            .Z = p2.Z + ((p2.Z - p1.Z) * dist)
            
'                .X = (p1.X + ((n * (.X / dist)) * 2))
'                .Y = (p1.Y + ((n * (.Y / dist)) * 2))
'                .Z = (p1.Z + ((n * (.Z / dist)) * 2))

                .X = (p1.X + ((n * (.X / dist))))
                .Y = (p1.Y + ((n * (.Y / dist))))
                .Z = (p1.Z + ((n * (.Z / dist))))
                
            ElseIf (n = 0) Then
                .X = p1.X
                .Y = p1.Y
                .Z = p1.Z
            ElseIf (dist = 0) Then
                .X = p2.X
                .Y = p2.Y
                .Z = p2.Z + IIf(p2.Z > p1.Z, n, -n)
            End If
        End If
    End With
End Function

Public Function RandomPositive(ByVal LowerBound As Long, ByVal UpperBound As Long) As Single
    RandomPositive = CSng((UpperBound - LowerBound + 1) * Rnd + LowerBound)
End Function

Public Function PlaneNormal(ByRef v0 As Point, ByRef V1 As Point, ByRef V2 As Point) As Point
    'returns a vector perpendicular to a plane V, at 0,0,0, with out the local coordinates information
    Set PlaneNormal = VectorNormalize(VectorCrossProduct(VectorDeduction(v0, V1), VectorDeduction(V1, V2)))
End Function

Public Function PointNormalize(ByRef v As Point) As Point
    Set PointNormalize = New Point
    With PointNormalize
        .Z = (v.X ^ 2 + v.Y ^ 2 + v.Z ^ 2) ^ (1 / 2)
        If (.Z <> 0) Then
            .X = (v.X / .Z)
            .Y = (v.Y / .Z)
            .Z = (v.Z / .Z)
        End If
    End With
End Function

Public Function NormalizedAxii(ByVal Value As Single) As Single
    'returns -1, 0 or 1 depending on which is passed to it, in cycles them
    '1 returns 0, 0 returns -1, -1 returns 1, anything else returns a zero
    NormalizedAxii = ((-CSng(CBool(Round(Value, 0))) + -1) + -CSng(Not CBool(-Round(Value, 0) + -1)))
End Function

Public Function SphereSurfaceArea(ByVal Radii As Single) As Single
     SphereSurfaceArea = (4 * PI * (Radii ^ 2))
End Function

Public Function SphereVolume(ByVal Radii As Single) As Single
    SphereVolume = ((4 / 3) * PI * (Radii ^ 3))
End Function

Public Function SquareCenter(ByRef v0 As Point, ByRef V1 As Point, ByRef V2 As Point, ByRef V3 As Point) As Point
    Set SquareCenter = New Point
    With SquareCenter
        .X = (Least(v0.X, V1.X, V2.X, V3.X) + ((Large(v0.X, V1.X, V2.X, V3.X) - Least(v0.X, V1.X, V2.X, V3.X)) / 2))
        .Y = (Least(v0.Y, V1.Y, V2.Y, V3.Y) + ((Large(v0.Y, V1.Y, V2.Y, V3.Y) - Least(v0.Y, V1.Y, V2.Y, V3.Y)) / 2))
        .Z = (Least(v0.Z, V1.Z, V2.Z, V3.Z) + ((Large(v0.Z, V1.Z, V2.Z, V3.Z) - Least(v0.Z, V1.Z, V2.Z, V3.Z)) / 2))
    End With
End Function

Public Function CirclePermeter(ByVal Radii As Single) As Single
    CirclePermeter = ((Radii * 2) * PI)
End Function

Public Function CubePerimeter(ByVal Edge As Single) As Single
    CubePerimeter = (Edge * 12)
End Function

Public Function CubeSurfaceArea(ByVal Edge As Single) As Single
    CubeSurfaceArea = (6 * (Edge ^ 2))
End Function

Public Function CubeVolume(ByVal Edge As Single) As Single
    CubeVolume = (Edge ^ 3)
End Function
Public Function AbsoluteNegativeInverse(ByVal d As Single) As Single
    AbsoluteNegativeInverse = -(1 / d)
End Function

Public Function TrianglePerimeter(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Single
    TrianglePerimeter = (DistanceEx(p1, p2) + DistanceEx(p2, p3) + DistanceEx(p3, p1))
End Function

Public Function TriangleSurfaceArea(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Single
    Dim l1 As Single: l1 = DistanceEx(p1, p2)
    Dim l2 As Single: l2 = DistanceEx(p2, p3)
    Dim l3 As Single: l3 = DistanceEx(p3, p1)
    TriangleSurfaceArea = (((((((l1 + l2) - l3) + ((l2 + l3) - l1) + ((l3 + l1) - l2)) * (l1 * l2 * l3)) / (l1 + l2 + l3)) ^ (1 / 2)))
End Function

Public Function TriangleVolume(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Single
    TriangleVolume = TriangleSurfaceArea(p1, p2, p3)
    TriangleVolume = ((((TriangleVolume ^ (1 / 3)) ^ 2) ^ 3) / 12)
End Function

Public Function TriangleDotProduct(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Single
    TriangleDotProduct = (((VectorDotProduct(p1, VectorSubtraction(p2, p3)) * VectorDotProduct(p2, VectorSubtraction(p1, p3))) ^ (1 / 3)) * 2)
End Function

Public Function TriangleAveraged(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Point
    Set TriangleAveraged = New Point
    With TriangleAveraged
        .X = ((p1.X + p2.X + p3.X) / 3)
        .Y = ((p1.Y + p2.Y + p3.Y) / 3)
        .Z = ((p1.Z + p2.Z + p3.Z) / 3)
    End With
End Function

Public Function TriangleOffset(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Point
    Set TriangleOffset = New Point
    With TriangleOffset
        .X = (Large(p1.X, p2.X, p3.X) - Least(p1.X, p2.X, p3.X))
        .Y = (Large(p1.Y, p2.Y, p3.Y) - Least(p1.Y, p2.Y, p3.Y))
        .Z = (Large(p1.Z, p2.Z, p3.Z) - Least(p1.Z, p2.Z, p3.Z))
    End With
End Function

Public Function TriangleAxii(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Point
    Set TriangleAxii = New Point
    With TriangleAxii
        Dim o As Point
        Set o = TriangleOffset(p1, p2, p3)
        .X = (Least(p1.X, p2.X, p3.X) + (o.X / 2))
        .Y = (Least(p1.Y, p2.Y, p3.Y) + (o.Y / 2))
        .Z = (Least(p1.Z, p2.Z, p3.Z) + (o.Z / 2))
    End With
End Function

Public Function TriangleNormal(ByRef v0 As Point, ByRef V1 As Point, ByRef V2 As Point) As Point
    Set TriangleNormal = New Point
    Dim o As Point
    Dim d As Single
    With TriangleNormal
        Set o = TriangleDisplace(v0, V1, V2)
        d = (o.X + o.Y + o.Z)
        If (d > 0) Then
            .Z = (((o.X + o.Y) - o.Z) / d)
            .X = (((o.Y + o.Z) - o.X) / d)
            .Y = (((o.Z + o.X) - o.Y) / d)
        End If
    End With
End Function

Public Function TriangleAccordance(ByRef v0 As Point, ByRef V1 As Point, ByRef V2 As Point) As Point
    Set TriangleAccordance = New Point
    With TriangleAccordance
        .X = (((v0.X + V1.X) - V2.X) + ((V1.X + V2.X) - v0.X) - ((V2.X + v0.X) - V1.X))
        .Y = (((v0.Y + V1.Y) - V2.Y) + ((V1.Y + V2.Y) - v0.Y) - ((V2.Y + v0.Y) - V1.Y))
        .Z = (((v0.Z + V1.Z) - V2.Z) + ((V1.Z + V2.Z) - v0.Z) - ((V2.Z + v0.Z) - V1.Z))
    End With
End Function

Public Function TriangleDisplace(ByRef v0 As Point, ByRef V1 As Point, ByRef V2 As Point) As Point
    Set TriangleDisplace = New Point
    With TriangleDisplace
        .X = (Abs((Abs(v0.X) + Abs(V1.X)) - Abs(V2.X)) + Abs((Abs(V1.X) + Abs(V2.X)) - Abs(v0.X)) - Abs((Abs(V2.X) + Abs(v0.X)) - Abs(V1.X)))
        .Y = (Abs((Abs(v0.Y) + Abs(V1.Y)) - Abs(V2.Y)) + Abs((Abs(V1.Y) + Abs(V2.Y)) - Abs(v0.Y)) - Abs((Abs(V2.Y) + Abs(v0.Y)) - Abs(V1.Y)))
        .Z = (Abs((Abs(v0.Z) + Abs(V1.Z)) - Abs(V2.Z)) + Abs((Abs(V1.Z) + Abs(V2.Z)) - Abs(v0.Z)) - Abs((Abs(V2.Z) + Abs(v0.Z)) - Abs(V1.Z)))
    End With
End Function

Public Function VectorBalance(ByRef loZero As Point, ByRef hiWhole As Point, ByVal folcrumPercent As Single) As Point
    Set VectorBalance = New Point
    With VectorBalance
        .X = (loZero.X + ((hiWhole.X - loZero.X) * folcrumPercent))
        .Y = (loZero.Y + ((hiWhole.Y - loZero.Y) * folcrumPercent))
        .Z = (loZero.Z + ((hiWhole.Z - loZero.Z) * folcrumPercent))
    End With
'    Set VectorBalance = vectormidpoint(lozer, hiWhole)
'    With VectorBalance
'        If folcrumPercent <= 0.5 Then
'            .X = (.X - ((hiWhole.X - loZero.X) * ((1 - folcrumPercent) - 0.5)))
'            .Y = (.Y - ((hiWhole.Y - loZero.Y) * ((1 - folcrumPercent) - 0.5)))
'            .Z = (.Z - ((hiWhole.Z - loZero.Z) * ((1 - folcrumPercent) - 0.5)))
'        Else
'            .X = (.X + ((hiWhole.X - loZero.X) * (folcrumPercent - 0.5)))
'            .Y = (.Y + ((hiWhole.Y - loZero.Y) * (folcrumPercent - 0.5)))
'            .Z = (.Z + ((hiWhole.Z - loZero.Z) * (folcrumPercent - 0.5)))
'        End If
'    End With
End Function

Public Function TriangleFolcrum(ByRef p1 As Point, ByRef p2 As Point, Optional ByRef p3 As Point = Nothing) As Point
    Set TriangleFolcrum = New Point
    With TriangleFolcrum
        If (Not p3 Is Nothing) Then
            .X = (p3.X ^ 2)
            .Y = (p3.Y ^ 2)
            .Z = (p3.Z ^ 2)
        End If
        .X = (.X + (p1.X ^ 2) + (p2.X ^ 2)) ^ (1 / 2)
        .Y = (.Y + (p1.Y ^ 2) + (p2.Y ^ 2)) ^ (1 / 2)
        .Z = (.Z + (p1.Z ^ 2) + (p2.Z ^ 2)) ^ (1 / 2)
    End With
End Function

Public Function TriangleOpposite(ByRef p1 As Point, ByRef p2 As Point, ByRef p3 As Point) As Single
    Dim hypo As Single
    TriangleOpposite = DistanceEx(p1, p2)
    hypo = DistanceEx(p2, p3)
    If hypo < TriangleOpposite Then
        TriangleOpposite = ((TriangleOpposite ^ 2) - (hypo ^ 2)) ^ (1 / 2)
    Else
        TriangleOpposite = ((hypo ^ 2) - (TriangleOpposite ^ 2)) ^ (1 / 2)
    End If
End Function

Public Function TriangleAdjacent(ByRef p1 As Point, ByRef p2 As Point, Optional ByRef p3 As Point = Nothing) As Single
    TriangleAdjacent = DistanceEx(p1, p2)
    If Not p3 Is Nothing Then
        Dim l1 As Single
        Dim l2 As Single
        l1 = DistanceEx(p2, p3)
        l2 = DistanceEx(p3, p1)
        If TriangleAdjacent < l1 Xor TriangleAdjacent < l2 Then
            If TriangleAdjacent < l1 Then
                If l1 > l2 Then
                    TriangleAdjacent = ((TriangleAdjacent ^ 2) - (l2 ^ 2)) ^ (1 / 2)
                Else
                    TriangleAdjacent = ((TriangleAdjacent ^ 2) - (l1 ^ 2)) ^ (1 / 2)
                End If
            Else
                If l1 > l2 Then
                    If l2 > TriangleAdjacent Then
                        TriangleAdjacent = ((l1 ^ 2) - (TriangleAdjacent ^ 2)) ^ (1 / 2)
                    Else
                        TriangleAdjacent = ((l1 ^ 2) - (l2 ^ 2)) ^ (1 / 2)
                    End If
                Else
                    If l1 > TriangleAdjacent Then
                        TriangleAdjacent = ((l2 ^ 2) - (TriangleAdjacent ^ 2)) ^ (1 / 2)
                    Else
                        TriangleAdjacent = ((l2 ^ 2) - (l1 ^ 2)) ^ (1 / 2)
                    End If
                End If
            End If
        End If
    Else
        TriangleAdjacent = (((TriangleAdjacent ^ 2) / 2) ^ (1 / 2))
    End If
End Function

Public Function TriangleHypotenuse(ByRef p1 As Point, ByRef p2 As Point, Optional ByRef p3 As Point = Nothing) As Single
    TriangleHypotenuse = DistanceEx(p1, p2)
    If p3 Is Nothing Then
        TriangleHypotenuse = ((TriangleHypotenuse ^ 2) + (TriangleHypotenuse ^ 2)) ^ (1 / 2)
    Else
        TriangleHypotenuse = ((TriangleHypotenuse ^ 2) + (DistanceEx(p2, p3) ^ 2)) ^ (1 / 2)
    End If
End Function

Public Function VectorRotateAxis(ByRef p1 As Point, ByRef Angles As Point) As Point
    Set VectorRotateAxis = VectorRotateX(p1, Angles.X)
    Set VectorRotateAxis = VectorRotateY(VectorRotateAxis, Angles.Y)
    Set VectorRotateAxis = VectorRotateZ(VectorRotateAxis, Angles.Z)
End Function

Public Function VectorRotateX(ByRef p1 As Point, ByVal angle As Single) As Point
    Set VectorRotateX = New Point
    With VectorRotateX
        .X = p1.X
        .Y = Cos(angle) * p1.Y - Sin(angle) * p1.Z
        .Z = Sin(angle) * p1.Y + Cos(angle) * p1.Z
    End With
End Function

Public Function VectorRotateY(ByRef p1 As Point, ByVal angle As Single) As Point
    Set VectorRotateY = New Point
    With VectorRotateY
        .X = Sin(angle) * p1.Z + Cos(angle) * p1.X
        .Y = p1.Y
        .Z = Cos(angle) * p1.Z - Sin(angle) * p1.X
    End With
End Function

Public Function VectorRotateZ(ByRef p1 As Point, ByVal angle As Single) As Point
    Set VectorRotateZ = New Point
    With VectorRotateZ
        .X = Cos(angle) * p1.X - Sin(angle) * p1.Y
        .Y = Sin(angle) * p1.X + Cos(angle) * p1.Y
        .Z = p1.Z
    End With
End Function

Public Sub AngleAxisRestrict(ByRef p As Point)
    p.X = AngleRestrict(p.X)
    p.Y = AngleRestrict(p.Y)
    p.Z = AngleRestrict(p.Z)
End Sub
Public Function AngleRestrict(ByRef A As Single) As Single
    Do While A > PI * 2
        A = A - (PI * 2)
    Loop
    Do While A < -PI * 2
        A = A + (PI * 2)
    Loop
    AngleRestrict = A
End Function

Public Function ATan2(ByVal opp As Single, ByVal adj As Single) As Single
    If Abs(adj) < 0.0001 Then
        ATan2 = PI / 2
    Else
        ATan2 = Abs(Atn(opp / adj))
    End If
    If adj < 0 Then ATan2 = PI - ATan2
    If opp < 0 Then ATan2 = -ATan2
End Function

Public Function VectorAxisAngles(ByRef p As Point, Optional ByVal Combined As Boolean = True) As Point
    Set VectorAxisAngles = New Point
    With VectorAxisAngles
        If Combined Then
            Dim magnitude As Single
            Dim heading As Single
            Dim pitch As Single
            Dim slope As Single
            slope = VectorSlope(MakePoint(0, 0, 0), p)
            magnitude = Sqr(p.X * p.X + p.Y * p.Y + p.Z * p.Z)
            heading = ATan2(p.Z, p.X)
            pitch = ATan2(p.Y, Sqr(p.X * p.X + p.Z * p.Z))
            .X = (((heading / magnitude) - pitch) * (slope / magnitude))
            .Z = ((PI / 2) + (-pitch + (heading / magnitude))) * (1 - (slope / magnitude))
            .Y = ((-heading + (pitch / magnitude)) * (1 - (slope / magnitude)))
            .Y = -(.Y + ((.X * (slope / magnitude)) / 2) - (.Y * 2) - ((.Z * (slope / magnitude)) / 2))
            .X = (PI * 2) - (.X - ((PI / 2) * (slope / magnitude)))
            .Z = (PI * 2) - (.Z - ((PI / 2) * (slope / magnitude)))
        Else
            .X = AngleOfCoord(MakePoint(p.Z, p.Y, 0))
            .Y = AngleOfCoord(MakePoint(p.X, p.Z, 0))
            .Z = AngleOfCoord(MakePoint(p.Y, p.X, 0))
        End If
    End With
End Function

Public Function AngleOfCoord(ByRef p As Point) As Single
    If p.X = 0 Then
        If p.Y > 0 Then
            AngleOfCoord = (90 * RADIAN)
        ElseIf p.Y < 0 Then
            AngleOfCoord = (270 * RADIAN)
        End If
    ElseIf p.Y = 0 Then
        If p.X > 0 Then
            AngleOfCoord = (360 * RADIAN)
        ElseIf p.X < 0 Then
            AngleOfCoord = (180 * RADIAN)
        End If
    Else
        Dim dist As Single
        dist = Distance(0, 0, 0, p.X, p.Y, p.Z)
        If p.X > 0 And p.Y > 0 Then
            If Abs(p.Y) > Abs(p.X) Then
                AngleOfCoord = ((0 + ((Abs(p.X) / dist) * DEGREE)) * RADIAN)
            ElseIf Abs(p.Y) < Abs(p.X) Then
                AngleOfCoord = ((45 + (45 - ((Abs(p.Y) / Abs(p.X)) * DEGREE))) * RADIAN)
            Else
                AngleOfCoord = (45 * RADIAN)
            End If
        ElseIf p.X > 0 And p.Y < 0 Then
            If Abs(p.Y) < Abs(p.X) Then
                AngleOfCoord = ((90 + ((Abs(p.Y) / dist) * DEGREE)) * RADIAN)
            ElseIf Abs(p.Y) > Abs(p.X) Then
                AngleOfCoord = ((135 + (45 - ((Abs(p.X) / Abs(p.Y)) * DEGREE))) * RADIAN)
            Else
                AngleOfCoord = (135 * RADIAN)
            End If
        ElseIf p.X < 0 And p.Y < 0 Then
            If Abs(p.Y) > Abs(p.X) Then
                AngleOfCoord = ((180 + ((Abs(p.X) / dist) * DEGREE)) * RADIAN)
            ElseIf Abs(p.Y) < Abs(p.X) Then
                AngleOfCoord = ((225 + (45 - ((Abs(p.Y) / Abs(p.X)) * DEGREE))) * RADIAN)
            Else
                AngleOfCoord = (225 * RADIAN)
            End If
        ElseIf p.X < 0 And p.Y > 0 Then
            If Abs(p.Y) < Abs(p.X) Then
                AngleOfCoord = ((270 + ((Abs(p.Y) / dist) * DEGREE)) * RADIAN)
            ElseIf Abs(p.Y) > Abs(p.X) Then
                AngleOfCoord = ((315 + (45 - ((Abs(p.X) / Abs(p.Y)) * DEGREE))) * RADIAN)
            Else
                AngleOfCoord = (315 * RADIAN)
            End If
        End If

    End If

    If (Not (Abs(p.Y) = Abs(p.X))) And (Not ((p.X = 0) Or (p.Y = 0))) Then
        If AbsoluteDecimal(Round(AngleOfCoord / Round((PI / 2), 6), 6)) = 0 Then AngleOfCoord = 0
    End If

End Function

Public Function AngleInvertRotation(ByVal A As Single) As Single
    If A >= 0 Then
        AngleInvertRotation = A - PI
    ElseIf A < 0 Then
        AngleInvertRotation = A + PI
    End If
End Function
Public Function AngleAxisInvert(ByVal p As Point) As Point
    Set AngleAxisInvert = New Point
    With AngleAxisInvert
        .X = AngleInvertRotation(p.X)
        .Y = AngleInvertRotation(p.Y)
        .Z = AngleInvertRotation(p.Z)
    End With
End Function
Public Function VectorRise(ByRef p1 As Point, Optional ByRef p2 As Point = Nothing) As Single
    VectorRise = (Large(p1.Y, p2.Y) - Least(p1.Y, p2.Y))
End Function

Public Function VectorRun(ByRef p1 As Point, ByRef p2 As Point) As Single
    VectorRun = DistanceEx(MakePoint(p1.X, 0, p1.Z), MakePoint(p2.X, 0, p2.Z))
End Function

Public Function VectorSlope(ByRef p1 As Point, ByRef p2 As Point) As Single
    VectorSlope = VectorRun(p1, p2)
    If (VectorSlope <> 0) Then
        VectorSlope = Round((VectorRise(p1, p2) / VectorSlope), 6)
        If (VectorSlope = 0) Then VectorSlope = -CInt(Not ((p1.X = p2.X) And (p1.Y = p2.Y) And (p1.Z = p2.Z)))
    ElseIf VectorRise(p1, p2) <> 0 Then
        VectorSlope = 1
    End If
End Function

Public Function VectorYIntercept(ByRef p1 As Point, ByRef p2 As Point) As Single
    With VectorMidPoint(p1, p2)
        VectorYIntercept = VectorSlope(p1, p2)
        VectorYIntercept = -((VectorYIntercept * .X) - .Y)
    End With
End Function

Public Function VectorDotProduct(ByRef p1 As Point, ByRef p2 As Point) As Single
    VectorDotProduct = ((p1.X * p2.X) + (p1.Y * p2.Y) + (p1.Z * p2.Z))
End Function

Public Function VectorMultiply(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorMultiply = New Point
    With VectorMultiply
        .X = (p1.X * p2.X)
        .Y = (p1.Y * p2.Y)
        .Z = (p1.Z * p2.Z)
    End With
End Function

Public Function VectorCrossProduct(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorCrossProduct = New Point
    With VectorCrossProduct
        .X = ((p1.Y * p2.Z) - (p1.Z * p2.Y))
        .Y = ((p1.Z * p2.X) - (p1.X * p2.Z))
        .Z = ((p1.X * p2.Y) - (p1.Y * p2.X))
    End With
End Function

Public Function VectorSubtraction(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorSubtraction = New Point
    With VectorSubtraction
        .X = ((p1.X - p2.Z) - (p1.X - p2.Y))
        .Y = ((p1.Y - p2.X) - (p1.Y - p2.Z))
        .Z = ((p1.Z - p2.Y) - (p1.Z - p2.X))
    End With
End Function

Public Function VectorAccordance(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorAccordance = New Point
    With VectorAccordance
        .X = (((p1.X + p1.Y) - p2.Z) + ((p1.Z + p1.X) - p2.Y) - ((p1.Y + p1.Z) - p2.X))
        .Y = (((p1.Y + p1.Z) - p2.X) + ((p1.X + p1.Y) - p2.Z) - ((p1.Z + p1.X) - p2.Y))
        .Z = (((p1.Z + p1.X) - p2.Y) + ((p1.Y + p1.Z) - p2.X) - ((p1.X + p1.Y) - p2.Z))
    End With
End Function

Public Function VectorDisplace(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorDisplace = New Point
    With VectorDisplace
        .X = (Abs((Abs(p1.X) + Abs(p1.Y)) - Abs(p2.Z)) + Abs((Abs(p1.Z) + Abs(p1.X)) - Abs(p2.Y)) - Abs((Abs(p1.Y) + Abs(p1.Z)) - Abs(p2.X)))
        .Y = (Abs((Abs(p1.Y) + Abs(p1.Z)) - Abs(p2.X)) + Abs((Abs(p1.X) + Abs(p1.Y)) - Abs(p2.Z)) - Abs((Abs(p1.Z) + Abs(p1.X)) - Abs(p2.Y)))
        .Z = (Abs((Abs(p1.Z) + Abs(p1.X)) - Abs(p2.Y)) + Abs((Abs(p1.Y) + Abs(p1.Z)) - Abs(p2.X)) - Abs((Abs(p1.X) + Abs(p1.Y)) - Abs(p2.Z)))
    End With
End Function

Public Function VectorOffset(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorOffset = New Point
    With VectorOffset
        .X = (Large(p1.X, p2.X) - Least(p1.X, p2.X))
        .Y = (Large(p1.Y, p2.Y) - Least(p1.Y, p2.Y))
        .Z = (Large(p1.Z, p2.Z) - Least(p1.Z, p2.Z))
    End With
End Function

Public Function VectorDeduction(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorDeduction = New Point
    With VectorDeduction
        .X = (p1.X - p2.X)
        .Y = (p1.Y - p2.Y)
        .Z = (p1.Z - p2.Z)
    End With
End Function

Public Function VectorCrossDeduct(ByRef p1 As Point, ByRef p2 As Point)
    Set VectorCrossDeduct = New Point
    With VectorCrossDeduct
        .X = (p1.X - p2.Z)
        .Y = (p1.Y - p2.X)
        .Z = (p1.Z - p2.Y)
    End With
End Function

Public Function VectorAddition(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorAddition = New Point
    With VectorAddition
        .X = (p1.X + p2.X)
        .Y = (p1.Y + p2.Y)
        .Z = (p1.Z + p2.Z)
    End With
End Function

Public Function VectorMultiplyBy(ByRef p1 As Point, ByVal n As Single) As Point
    Set VectorMultiplyBy = New Point
    With VectorMultiplyBy
        .X = (p1.X * n)
        .Y = (p1.Y * n)
        .Z = (p1.Z * n)
    End With
End Function

Public Function VectorCombination(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorCombination = New Point
    With VectorCombination
        .X = ((p1.X + p2.X) / 2)
        .Y = ((p1.Y + p2.Y) / 2)
        .Z = ((p1.Z + p2.Z) / 2)
    End With
End Function

Public Function VectorNormalize(ByRef p1 As Point) As Point
    Set VectorNormalize = New Point
    With VectorNormalize
        .Z = (Abs(p1.X) + Abs(p1.Y) + Abs(p1.Z))
        If (.Z > 0) Then
            .Z = (1 / .Z)
            .X = (p1.X * .Z)
            .Y = (p1.Y * .Z)
            .Z = (p1.Z * .Z)
        End If
    End With
End Function

Public Function LineNormalize(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set LineNormalize = New Point
    With LineNormalize
        .Z = DistanceEx(p1, p2)
        If (.Z > 0) Then
            .Z = (1 / .Z)
            .X = ((p1.X - p2.X) * .Z)
            .Y = ((p1.Y - p2.Y) * .Z)
            .Z = ((p1.Z - p2.Z) * .Z)
        End If
    End With
End Function

Public Function VectorMidPoint(ByRef p1 As Point, ByRef p2 As Point) As Point
    Set VectorMidPoint = New Point
    With VectorMidPoint
        .X = ((Large(p1.X, p2.X) - Least(p1.X, p2.X)) / 2) + Least(p1.X, p2.X)
        .Y = ((Large(p1.Y, p2.Y) - Least(p1.Y, p2.Y)) / 2) + Least(p1.Y, p2.Y)
        .Z = ((Large(p1.Z, p2.Z) - Least(p1.Z, p2.Z)) / 2) + Least(p1.Z, p2.Z)
    End With
End Function

Public Function VectorNegative(ByRef p1 As Point) As Point
    Set VectorNegative = New Point
    With VectorNegative
        .X = -p1.X
        .Y = -p1.Y
        .Z = -p1.Z
    End With
End Function

Public Function VectorDivision(ByRef p1 As Point, ByVal n As Single) As Point
    Set VectorDivision = New Point
    With VectorDivision
        .X = (p1.X / n)
        .Y = (p1.Y / n)
        .Z = (p1.Z / n)
    End With
End Function

Public Function VectorIsNormal(ByRef p1 As Point) As Boolean
    'returns if a point provided is normalized, to the best of ability
    VectorIsNormal = (Round(Abs(p1.X) + Abs(p1.Y) + Abs(p1.Z), 0) = 1) 'first kind is the absolute of all values equals one
    VectorIsNormal = VectorIsNormal Or (DistanceEx(MakePoint(0, 0, 0), p1) = 1) 'another is the total length of vector is one
    'another is if any value exists non zero as well as adding up in any non specific arrangement cancels to zero, as has one
    VectorIsNormal = VectorIsNormal Or ((p1.X <> 0 Or p1.Y <> 0 Or p1.Z <> 0) And ( _
        ((p1.X + p1.Y + p1.Z) = 0) Or ((p1.Y + p1.Z + p1.X) = 0) Or ((p1.Z + p1.X + p1.Y) = 0) Or _
        ((p1.X + p1.Z + p1.Y) = 0) Or ((p1.Z + p1.Y + p1.X) = 0) Or ((p1.Y + p1.X + p1.Z) = 0))) 'And _
        ((p1.X > -PI And p1.X < PI) And (p1.Y > -PI And p1.Y < PI) And (p1.Z > -PI And p1.Z < PI)))
End Function

Public Function AbsoluteFactor(ByRef n As Single) As Single
    'accepts a number and returns 1 if positive, and -1 if negative
    AbsoluteFactor = ((-(Abs(n - 1) - n) - (-Abs(n + 1) + n)) * 0.5)
End Function

Public Function AbsoluteValue(ByRef n As Single) As Single
    'same as abs(), returns a number as positive quantified
    AbsoluteValue = (-((-(n * -1) * n) ^ (1 / 2) * -1))
End Function

Public Function AbsoluteWhole(ByRef n As Single) As Single
    'returns only the value to the left of a decimal number
    AbsoluteWhole = (n \ 1)
End Function

Public Function AbsoluteDecimal(ByRef n As Single) As Single
    'returns only the value to the right of a decimal number
    AbsoluteDecimal = (n - AbsoluteWhole(n))
End Function

Public Function AngleQuadrant(ByVal angle As Single) As Single
    'returns the axis quadrant a radian angle falls with-in
    angle = angle * DEGREE
    If angle > 0 And angle <= 90 Then
        AngleQuadrant = 1
    ElseIf angle > 90 And angle <= 180 Then
        AngleQuadrant = 2
    ElseIf angle > 180 And angle <= 270 Then
        AngleQuadrant = 3
    ElseIf angle > 270 And angle <= 360 Or angle = 0 Then
        AngleQuadrant = 4
    End If
End Function

Public Function AbsoluteInvert(ByVal Value As Long, Optional ByVal Whole As Long = 100, Optional ByVal Unit As Long = 1)
    'returns the inverted value of a whole conprised of unit measures
    'AbsoluteInvert(0, 16777216) returns the negative of black 0, which is 16777216
    AbsoluteInvert = -(Whole / Unit) + -(Value / Unit) + ((Whole / Unit) * 2)
End Function

Public Function Large(ByVal V1 As Variant, ByVal V2 As Variant, Optional ByVal V3 As Variant, Optional ByVal V4 As Variant) As Variant
    If IsMissing(V3) Then
        If (V1 >= V2) Then
            Large = V1
        Else
            Large = V2
        End If
    ElseIf IsMissing(V4) Then
        If ((V2 >= V3) And (V2 >= V1)) Then
            Large = V2
        ElseIf ((V1 >= V3) And (V1 >= V2)) Then
            Large = V1
        Else
            Large = V3
        End If
    Else
        If ((V2 >= V3) And (V2 >= V1) And (V2 >= V4)) Then
            Large = V2
        ElseIf ((V1 >= V3) And (V1 >= V2) And (V1 >= V4)) Then
            Large = V1
        ElseIf ((V3 >= V1) And (V3 >= V2) And (V3 >= V4)) Then
            Large = V3
        Else
            Large = V4
        End If
    End If
End Function

Public Function Least(ByVal V1 As Variant, ByVal V2 As Variant, Optional ByVal V3 As Variant, Optional ByVal V4 As Variant) As Variant
    If IsMissing(V3) Then
        If (V1 <= V2) Then
            Least = V1
        Else
            Least = V2
        End If
    ElseIf IsMissing(V4) Then
        If ((V2 <= V3) And (V2 <= V1)) Then
            Least = V2
        ElseIf ((V1 <= V3) And (V1 <= V2)) Then
            Least = V1
        Else
            Least = V3
        End If
    Else
        If ((V2 <= V3) And (V2 <= V1) And (V2 <= V4)) Then
            Least = V2
        ElseIf ((V1 <= V3) And (V1 <= V2) And (V1 <= V4)) Then
            Least = V1
        ElseIf ((V3 <= V1) And (V3 <= V2) And (V3 <= V4)) Then
            Least = V3
        Else
            Least = V4
        End If
    End If
End Function

