Attribute VB_Name = "modColor"
Option Explicit


Public Function RGBRed(RGBCol As Long) As Integer
'Return the Red component from an RGB Color
 RGBRed = RGBCol And &HFF
End Function
Public Function RGBGreen(RGBCol As Long) As Integer
'Return the Green component from an RGB Color
 RGBGreen = ((RGBCol And &H100FF00) / &H100)
End Function
Public Function RGBBlue(RGBCol As Long) As Integer
'Return the Blue component from an RGB Color
 RGBBlue = (RGBCol And &HFF0000) / &H10000
End Function

Public Function Blend(RGB1 As Long, RGB2 As Long, _
Percent As Single) As Long
'This one doesn't really use the HSL routines, just the
'RGB Component routines. I threw it in as a bonus ;)
'Takes two colors and blends them according to a
'percentage given as a Single
'For example, .3 will return a color 30% of the way
'between the first color and the second.
'.5, or 50%, will be an even blend (halfway)
Dim R As Integer, R1 As Integer, R2 As Integer, _
G As Integer, G1 As Integer, G2 As Integer, _
B As Integer, B1 As Integer, B2 As Integer
If Percent >= 1 Then
Blend = RGB2
Exit Function
ElseIf Percent <= 0 Then
Blend = RGB1
Exit Function
End If
R1 = RGBRed(RGB1)
R2 = RGBRed(RGB2)
G1 = RGBGreen(RGB1)
G2 = RGBGreen(RGB2)
B1 = RGBBlue(RGB1)
B2 = RGBBlue(RGB2)
R = ((R2 * Percent) + (R1 * (1 - Percent)))
G = ((G2 * Percent) + (G1 * (1 - Percent)))
B = ((B2 * Percent) + (B1 * (1 - Percent)))
Blend = RGB(R, G, B)
End Function
