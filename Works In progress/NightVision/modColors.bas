Attribute VB_Name = "modCOlors"
'**************************************
'Windows API/Global Declarations for :HSL<->RGB and Color Manipulation Routines
'**************************************
Option Explicit
'
' modHSL.bas
' HSL/RGB + Color Manipulation routines
'
'Portions of this code marked with *** are converted from
'C/C++ routines for RGB/HSL conversion found in the
'Microsoft Knowledge Base (PD sample code):
'http://support.microsoft.com/support/kb/articles/Q29/2/40.asp
'In addition to the language conversion, some internal
'calculations have been modified and converted to FP math to
'reduce rounding errors.
'Conversion to VB and original code by
'Dan Redding (bwsoft@revealed.net)
'http://home.revealed.net/bwsoft
'Free to use, please give proper credit
Const HSLMAX As Integer = 240 '***
 'H, S and L values can be 0 - HSLMAX
 '240 matches what is used by MS Win;
 'any number less than 1 byte is OK;
 'works best if it is evenly divisible by 6
Const RGBMAX As Integer = 255 '***
 'R, G, and B value can be 0 - RGBMAX
Const UNDEFINED As Integer = (HSLMAX * 2 / 3) '***
 'Hue is undefined if Saturation = 0 (greyscale)
Public Type HSLCol 'Datatype used to pass HSL Color values
 Hue As Integer
 Sat As Integer
 Lum As Integer
End Type
Public Function rgbRed(RGBCol As Long) As Integer
'Return the Red component from an RGB Color
 rgbRed = RGBCol And &HFF
End Function
Public Function rgbGreen(RGBCol As Long) As Integer
'Return the Green component from an RGB Color
 rgbGreen = ((RGBCol And &H100FF00) / &H100)
End Function
Public Function rgbBlue(RGBCol As Long) As Integer
'Return the Blue component from an RGB Color
 rgbBlue = (RGBCol And &HFF0000) / &H10000
End Function
Private Function iMax(A As Integer, b As Integer) As Integer
'Return the Larger of two values
 iMax = IIf(A > b, A, b)
End Function
Private Function iMin(A As Integer, b As Integer) As Integer
'Return the smaller of two values
 iMin = IIf(A < b, A, b)
End Function
Public Function RGBtoHSL(RGBCol As Long) As HSLCol '***
'Returns an HSLCol datatype containing Hue, Luminescence
'and Saturation; given an RGB Color value
Dim r As Integer, g As Integer, b As Integer
Dim cMax As Integer, cMin As Integer
Dim RDelta As Double, GDelta As Double, _
 BDelta As Double
Dim H As Double, S As Double, l As Double
Dim cMinus As Long, cPlus As Long
 
 r = rgbRed(RGBCol)
 g = rgbGreen(RGBCol)
 b = rgbBlue(RGBCol)
 
 cMax = iMax(iMax(r, g), b) 'Highest and lowest
 cMin = iMin(iMin(r, g), b) 'color values
 
 cMinus = cMax - cMin 'Used to simplify the
 cPlus = cMax + cMin 'calculations somewhat.
 
 'Calculate luminescence (lightness)
 l = ((cPlus * HSLMAX) + RGBMAX) / (2 * RGBMAX)
 
 If cMax = cMin Then 'achromatic (r=g=b, greyscale)
 S = 0 'Saturation 0 for greyscale
 H = UNDEFINED 'Hue undefined for greyscale
 Else
 'Calculate color saturation
 If l <= (HSLMAX / 2) Then
 S = ((cMinus * HSLMAX) + 0.5) / cPlus
 Else
 S = ((cMinus * HSLMAX) + 0.5) / (2 * RGBMAX - cPlus)
 End If
 
 'Calculate hue
 RDelta = (((cMax - r) * (HSLMAX / 6)) + 0.5) / cMinus
 GDelta = (((cMax - g) * (HSLMAX / 6)) + 0.5) / cMinus
 BDelta = (((cMax - b) * (HSLMAX / 6)) + 0.5) / cMinus
 
 Select Case cMax
 Case CLng(r)
 H = BDelta - GDelta
 Case CLng(g)
 H = (HSLMAX / 3) + RDelta - BDelta
 Case CLng(b)
 H = ((2 * HSLMAX) / 3) + GDelta - RDelta
 End Select
 
 If H < 0 Then H = H + HSLMAX
 End If
 
 RGBtoHSL.Hue = CInt(H)
 RGBtoHSL.Lum = CInt(l)
 RGBtoHSL.Sat = CInt(S)
End Function
Public Function HSLtoRGB(HueLumSat As HSLCol) As Long '***
Dim r As Long, g As Long, b As Long
Dim H As Long, l As Long, S As Long
Dim Magic1 As Integer, Magic2 As Integer
H = HueLumSat.Hue
l = HueLumSat.Lum
S = HueLumSat.Sat
If S = 0 Then 'Greyscale
r = (l * RGBMAX) / HSLMAX 'luminescence,
'converted to the proper range
g = r 'All RGB values same in greyscale
b = r
If H <> UNDEFINED Then
'This is technically an error.
'The RGBtoHSL routine will always return
'Hue = UNDEFINED (in this case 160)
'when Sat = 0.
'if you are writing a color mixer and
'letting the user input color values,
'you may want to set Hue = UNDEFINED
'in this case.
End If
Else
'Get the "Magic Numbers"
If l <= HSLMAX / 2 Then
Magic2 = (l * (HSLMAX + S) + _
(HSLMAX / 2)) / HSLMAX
Else
Magic2 = l + S - ((l * S) + _
(HSLMAX / 2)) / HSLMAX
End If
Magic1 = 2 * l - Magic2
'get R, G, B; change units from HSLMAX range
'to RGBMAX range
r = (HuetoRGB(Magic1, Magic2, H + (HSLMAX / 3)) _
* RGBMAX + (HSLMAX / 2)) / HSLMAX
g = (HuetoRGB(Magic1, Magic2, H) _
* RGBMAX + (HSLMAX / 2)) / HSLMAX
b = (HuetoRGB(Magic1, Magic2, H - (HSLMAX / 3)) _
* RGBMAX + (HSLMAX / 2)) / HSLMAX
End If
HSLtoRGB = RGB(CInt(r), CInt(g), CInt(b))
End Function
Private Function HuetoRGB(mag1 As Integer, mag2 As Integer, _
Hue As Long) As Long '***
'Utility function for HSLtoRGB
'Range check
If Hue < 0 Then
Hue = Hue + HSLMAX
ElseIf Hue > HSLMAX Then
Hue = Hue - HSLMAX
End If
'Return r, g, or b value from parameters
Select Case Hue 'Values get progressively larger.
'Only the first true condition will execute
Case Is < (HSLMAX / 6)
HuetoRGB = (mag1 + (((mag2 - mag1) * Hue + _
(HSLMAX / 12)) / (HSLMAX / 6)))
Case Is < (HSLMAX / 2)
HuetoRGB = mag2
Case Is < (HSLMAX * 2 / 3)
HuetoRGB = (mag1 + (((mag2 - mag1) * _
((HSLMAX * 2 / 3) - Hue) + _
(HSLMAX / 12)) / (HSLMAX / 6)))
Case Else
HuetoRGB = mag1
End Select
End Function
'
' The following are individual functions
' that use the HSL/RGB routines
' This is not intended to be a comprehensive library,
' just a sampling to demonstrate how to use the routines
' and what kind of things are possible
'
Public Function ContrastingColor(RGBCol As Long) As Long
'Returns Black or White, whichever will show up better
'on the specified color
'Useful for setting label forecolors with transparent
'backgrounds (send it the form backcolor - RGB value, not
'system value!)
Dim HSL As HSLCol
HSL = RGBtoHSL(RGBCol)
If HSL.Lum > HSLMAX / 2 Then ContrastingColor = 0 _
Else: ContrastingColor = &HFFFFFF
End Function
'Color adjustment routines
'These accept a color and return a modified color.
'Perhaps the most common use might be to apply a process
'to an image pixel by pixel
Public Function Greyscale(RGBColor As Long) As Long
'Returns the achromatic version of a color
Dim HSL As HSLCol
HSL = RGBtoHSL(RGBColor)
HSL.Sat = 0
HSL.Hue = UNDEFINED
Greyscale = HSLtoRGB(HSL)
End Function
Public Function Tint(RGBColor As Long, Hue As Integer)
'Changes the Hue of a color to a specified Hue
'For example, changing all the pixels in a picture to
'a hue of 80 would tint the picture green
Dim HSL As HSLCol
If Hue < 0 Then
Hue = 0
ElseIf Hue > HSLMAX Then
Hue = HSLMAX
End If
HSL = RGBtoHSL(RGBColor)
HSL.Hue = Hue
Tint = HSLtoRGB(HSL)
End Function
Public Function Brighten(RGBColor As Long, Percent As Single)
'Lightens the color by a specifie percent, given as a Single
'(10% = .10)
Dim HSL As HSLCol, l As Long
If Percent <= 0 Then
Brighten = RGBColor
Exit Function
End If
HSL = RGBtoHSL(RGBColor)
l = HSL.Lum + (HSL.Lum * Percent)
If l > HSLMAX Then l = HSLMAX
HSL.Lum = l
Brighten = HSLtoRGB(HSL)
End Function
Public Function Darken(RGBColor As Long, Percent As Single)
'Darkens the color by a specifie percent, given as a Single
Dim HSL As HSLCol, l As Long
If Percent <= 0 Then
Darken = RGBColor
Exit Function
End If
HSL = RGBtoHSL(RGBColor)
l = HSL.Lum - (HSL.Lum * Percent)
If l < 0 Then l = 0
HSL.Lum = l
Darken = HSLtoRGB(HSL)
End Function
Public Function ReverseLight(RGBColor As Long) As Long
'Make dark colors light and vice versa without changing Hue
'or saturation
Dim HSL As HSLCol
HSL = RGBtoHSL(RGBColor)
HSL.Lum = HSLMAX - HSL.Lum
ReverseLight = HSLtoRGB(HSL)
End Function
Public Function ReverseColor(RGBColor As Long) As Long
'Swap colors without changing saturation or luminescence
Dim HSL As HSLCol
HSL = RGBtoHSL(RGBColor)
HSL.Hue = HSLMAX - HSL.Hue
ReverseColor = HSLtoRGB(HSL)
End Function
Public Function CycleColor(RGBColor As Long) As Long
'Cycle colors thru a 12 stage pattern without changing
'saturation or luminescence
Dim HSL As HSLCol, H As Long
HSL = RGBtoHSL(RGBColor)
H = HSL.Hue + (HSLMAX / 12)
If H > HSLMAX Then H = H - HSLMAX
HSL.Hue = H
CycleColor = HSLtoRGB(HSL)
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
Dim r As Integer, r1 As Integer, r2 As Integer, _
g As Integer, g1 As Integer, g2 As Integer, _
b As Integer, b1 As Integer, b2 As Integer
If Percent >= 1 Then
Blend = RGB2
Exit Function
ElseIf Percent <= 0 Then
Blend = RGB1
Exit Function
End If
r1 = rgbRed(RGB1)
r2 = rgbRed(RGB2)
g1 = rgbGreen(RGB1)
g2 = rgbGreen(RGB2)
b1 = rgbBlue(RGB1)
b2 = rgbBlue(RGB2)
r = ((r2 * Percent) + (r1 * (1 - Percent)))
g = ((g2 * Percent) + (g1 * (1 - Percent)))
b = ((b2 * Percent) + (b1 * (1 - Percent)))
Blend = RGB(r, g, b)
End Function


Public Function Blend2(RGB1 As Long, RGB2 As Long) As Long
'This one doesn't really use the HSL routines, just the
'RGB Component routines. I threw it in as a bonus ;)
'Takes two colors and blends them according to a
'percentage given as a Single
'For example, .3 will return a color 30% of the way
'between the first color and the second.
'.5, or 50%, will be an even blend (halfway)
Dim r As Integer, r1 As Integer, r2 As Integer, _
g As Integer, g1 As Integer, g2 As Integer, _
b As Integer, b1 As Integer, b2 As Integer
If RGB1 <> -1 And RGB2 <> -1 Then
r1 = rgbRed(RGB1)
r2 = rgbRed(RGB2)
g1 = rgbGreen(RGB1)
g2 = rgbGreen(RGB2)
b1 = rgbBlue(RGB1)
b2 = rgbBlue(RGB2)
r = ((r1 + r2) \ 2)
g = ((g1 + g2) \ 2)
b = ((b1 + b2) \ 2)
Blend2 = RGB(r, g, b)
ElseIf RGB1 <> -1 Then
Blend2 = RGB1
ElseIf RGB2 <> -1 Then
Blend2 = RGB2
End If
End Function
