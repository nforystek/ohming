VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "PoolID"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Option Compare Binary

Public Enum TimedSums
    NoTiming = 0
    CheckSums = 1
    BoundSums = 2
End Enum

Private Dividen As Single
Private LastGen As Single

Private Const Bit1 As Byte = &H1
Private Const Bit2 As Byte = &H2
Private Const Bit3 As Byte = &H4
Private Const Bit4 As Byte = &H8
Private Const Bit5 As Byte = &H10
Private Const Bit6 As Byte = &H20
Private Const Bit7 As Byte = &H40
Private Const Bit8 As Byte = &H80

Private Declare Sub Sleep Lib "kernel32" (ByVal dwMilliseconds As Long)
Private Declare Function GetTickCount Lib "kernel32" () As Long

Private SumCheck As TimedSums
Private Declare Sub CopyMem Lib "kernel32" Alias "RtlMoveMemory" (Destination As Any, Source As Any, ByVal Length As Long)

Public Static Property Get Pacerate() As TimedSums
    Pacerate = SumCheck
End Property
Public Static Property Let Pacerate(ByRef nval As TimedSums)
    SumCheck = nval
End Property
    
Public Function Generate(Optional ByVal ParaPhrase As String = "*") As String
    Dim tick As Long
    Dim start As Single
    Dim ohmz As String
    Dim dash As Byte
    Static sum As Boolean
    
    If (SumCheck = NoTiming) Then
        ohmz = EncryptString(CStr(Timer \ Dividen), CStr(GetTickCount \ Dividen), False)
        ohmz = EncryptString(ParaPhrase, EncryptString(CStr(Timer), ohmz, False), False)
            
        ohmz = EncryptString( _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)), ohmz, True)
        
        dash = 1 + Int(2 + ((8 - 2) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((9 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((9 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((7 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        If Not (dash Mod 2 = 0) Then ohmz = StrReverse(ohmz)
        
        Generate = ohmz
    ElseIf (SumCheck = CheckSums) Then
        
        start = Timer
        tick = GetTickCount

        ohmz = EncryptString(CStr(Timer \ Dividen), CStr(GetTickCount \ Dividen), False)
        ohmz = EncryptString(ParaPhrase, EncryptString(CStr(Timer), ohmz, False), False)
            
        ohmz = EncryptString( _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)), ohmz, True)
        
        dash = 1 + Int(2 + ((8 - 2) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((9 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((9 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((7 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        If Not (dash Mod 2 = 0) Then ohmz = StrReverse(ohmz)
        
        Generate = ohmz
        
        tick = CSng(GetTickCount - tick)
        start = CSng(Timer - start)
            
        If Not sum Then
            sum = True
                
            Static tsu As Single
            Static eta As Single
            If tsu > 0 Then Sleep tsu
            eta = eta - 1
            If (eta < 0) Then
                tsu = eta
                eta = CheckSum(ParaPhrase)
            End If
            sum = False
        End If

        LastGen = LastGen + Sqr(tick * start)
    ElseIf (SumCheck = BoundSums) Then

        If Not sum Then
            sum = True
            Sleep CheckSum(ParaPhrase)
            sum = False
        End If

        start = Timer
        tick = GetTickCount
        
        ohmz = EncryptString(CStr(Timer \ Dividen), CStr(GetTickCount \ Dividen), False)
        ohmz = EncryptString(ParaPhrase, EncryptString(CStr(Timer), ohmz, False), False)
            
        ohmz = EncryptString( _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)) & Int((9 * Rnd)) & Int((9 * Rnd)) & _
            Int((9 * Rnd)), ohmz, True)
        
        dash = 1 + Int(2 + ((8 - 2) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((9 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((9 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        dash = dash + Int(3 + ((7 - 3) * Rnd))
        ohmz = Left(ohmz, dash - 1) & "-" & Mid(ohmz, dash)
        If Not (dash Mod 2 = 0) Then ohmz = StrReverse(ohmz)
        
        Generate = ohmz

        tick = CSng(GetTickCount - tick)
        start = CSng(Timer - start)

        LastGen = LastGen + Sqr(tick * start)

    End If

End Function

Private Function CheckSum(Optional ByVal ParaPhrase As String = "*") As Single
    Dim ohmz As String
    Dim hert As String
    Dim temp As String
        
    Dim tick As Long
    Dim start As Single
    If (SumCheck = NoTiming) Then

        start = Timer
        tick = GetTickCount

        hert = EncryptString(CStr(start \ Dividen), CStr(GetTickCount \ Dividen), False)
        ohmz = EncryptString(ParaPhrase, EncryptString(CStr(Timer), hert, False), False)
        hert = DecryptString(ohmz, hert, False)
    
        tick = CSng(GetTickCount - tick)
        start = CSng(Timer - start)

        CheckSum = Sqr(tick * start)
        
    ElseIf (SumCheck = CheckSums) Then
    
        start = Timer
        tick = GetTickCount
        
        Do
    
            hert = EncryptString(CStr(start \ Dividen), CStr(GetTickCount \ Dividen), False)
            ohmz = EncryptString(ParaPhrase, EncryptString(CStr(Timer), hert, False), False)
    
            Static toggle As Boolean
            toggle = Not toggle
            If toggle Then hert = StrReverse(hert)
            hert = DecryptString(ohmz, hert, False)
    
        Loop Until hert = ParaPhrase
    
        tick = CSng(GetTickCount - tick)
        start = CSng(Timer - start)
        If toggle Then
            LastGen = 0
            Generate Generate
            CheckSum = LastGen
        Else
            CheckSum = Sqr(tick * start)
        End If
    ElseIf (SumCheck = BoundSums) Then
    
        start = Timer
        tick = GetTickCount
        
        Static tolerance As Single
        If tolerance <= 0 Then tolerance = 0.3543
        Do Until hert = ParaPhrase Or (Timer - start) >= tolerance
    
            hert = EncryptString(CStr(Timer \ Dividen), CStr(GetTickCount \ Dividen), False)
            ohmz = EncryptString(ParaPhrase, EncryptString(CStr(Timer), hert, False), False)
    
            hert = DecryptString(ohmz, hert, False)
    
        Loop
    
        If Not (hert = ParaPhrase) And (Timer - start) >= tolerance Then
            tolerance = 0.0103
        ElseIf tolerance < 0.3543 Then
            tolerance = tolerance + 0.0199
        End If
    
        tick = CSng(GetTickCount - tick)
        start = CSng(Timer - start)
        
        If Sqr(tick * start) - LastGen < 0 Then
            LastGen = 0
            Generate Generate
            CheckSum = Sqr(tick * start)
        ElseIf LastGen > 0 Then
            CheckSum = Sqr(tick * start) - LastGen
            LastGen = (Sqr(tick * start) - LastGen) + Sqr(tick * start)
        End If
    
    End If

End Function

Private Sub Class_Initialize()
    Dividen = GetTickCount / Timer
End Sub

Private Sub Class_Terminate()
    Randomize
End Sub

Private Function EncryptString(ByVal Text As String, ByVal Key As String, Optional ByVal OutputInHex As Boolean = True) As String
    If Len(Text) < 1 Or Len(Key) < 1 Then
        Err.Raise 8, "NTCipher10.NCode", "Both length of Text and Key in characters, must be non zero."
    Else
        EncryptString = StrConv(EnKrimpt(StrConv(Text, vbFromUnicode), StrConv(Key, vbFromUnicode)), vbUnicode)
        If OutputInHex = True Then EncryptString = HexEncodeData(EncryptString)
    End If
End Function

Private Function DecryptString(ByVal Text As String, ByVal Key As String, Optional ByVal IsTextInHex As Boolean = True) As String
    If Len(Text) < 1 Or Len(Key) < 1 Then
        Err.Raise 8, "NTCipher10.NCode", "Both length of Text and Key in characters, must be non zero."
    Else
        If IsTextInHex = True Then Text = HexDecodeData(Text)
        DecryptString = StrConv(DeKrimpt(StrConv(Text, vbFromUnicode), StrConv(Key, vbFromUnicode)), vbUnicode)
    End If
End Function

Private Function HexEncodeData(ByVal d As String) As String
    Dim s As String
    Dim l As Long
    Dim i As Long
    Dim n As String
    l = Len(d)
    If l > 0 Then
        For i = 1 To l
            n = Hex(Asc(Mid(d, i, 1)))
            If Len(n) < 2 Then
                s = s & "0" & n
            Else
                s = s & n
            End If
        Next
    End If
    HexEncodeData = s
End Function

Private Function HexDecodeData(ByVal d As String) As String
    Dim s As String
    Dim l As Long
    Dim i As Long
    l = Len(d)
    If l > 0 Then
        For i = 1 To l Step 2
            s = s & Chr(Val("&H" & Mid(d, i, 2)))
        Next
    End If
    HexDecodeData = s
End Function

Private Function EnKrimpt(Info() As Byte, Seed() As Byte) As Byte()
    
    Dim pin As Byte
    Dim swp As Byte
    Dim cap As Boolean
   
    Dim cnt1 As Long
    Dim cnt2 As Long
    
    Dim lbi As Long
    Dim ubi As Long
    Dim lbs As Long
    Dim ubs As Long
    
    lbi = LBound(Info)
    ubi = UBound(Info)
    lbs = LBound(Seed)
    ubs = UBound(Seed)
    
    BitByte(pin, Bit1) = BitByte(Seed(lbs), Bit6) Or BitByte(Seed(ubs), Bit2)
    BitByte(pin, Bit2) = BitByte(Seed(ubs), Bit4) Or BitByte(Seed(lbs), Bit1)
    BitByte(pin, Bit3) = BitByte(Seed(lbs), Bit8) Or BitByte(Seed(ubs), Bit5)
    BitByte(pin, Bit4) = BitByte(Seed(ubs), Bit7) Or BitByte(Seed(lbs), Bit3)
    
    cap = (BitByte(pin, Bit1) Or BitByte(pin, Bit2)) And (BitByte(pin, Bit3) Or BitByte(pin, Bit4))
    
    For cnt1 = lbs To ubs
    
        For cnt2 = lbi To ubi
        
            Select Case (-BitByte(Seed(cnt1), Bit1)) & (-BitByte(Seed(cnt1), Bit2)) & (-BitByte(Info(cnt2), Bit1)) & (-BitByte(Info(cnt2), Bit2))
                Case "0011"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
                Case "0000"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
                Case "0010"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True
                Case "0001"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
    
                Case "1111"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
                Case "1100"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
                Case "1110"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True
                Case "1101"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
    
                Case "1011"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True
                Case "1000"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
                Case "1010"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
                Case "1001"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
    
                Case "0111"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
                Case "0100"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True
                Case "0110"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
                Case "0101"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
            End Select
            
        Next
    Next
    
    For cnt1 = lbi To ubi

        Select Case (-BitByte(pin, Bit1)) & (-BitByte(pin, Bit2)) & (-BitByte(Info(cnt1), Bit3)) & (-BitByte(Info(cnt1), Bit4))
            Case "0011"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
            Case "0000"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True
            Case "0010"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True
            Case "0001"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False

            Case "1111"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
            Case "1100"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False
            Case "1110"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True
            Case "1101"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True

            Case "1011"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True
            Case "1000"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True
            Case "1010"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
            Case "1001"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False

            Case "0111"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False
            Case "0100"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True
            Case "0110"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True
            Case "0101"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
        End Select
       
    Next
    
    BitByte(swp, Bit1) = BitByte(Info(lbi), IIf(cap, Bit5, Bit6))
    BitByte(swp, Bit2) = BitByte(Info(lbi), IIf(cap, Bit6, Bit7))
    BitByte(swp, Bit3) = BitByte(Info(lbi), IIf(cap, Bit7, Bit8))
    BitByte(swp, Bit4) = BitByte(Info(lbi), IIf(cap, Bit8, Bit5))
    For cnt1 = lbi To ubi - 1
        BitByte(Info(cnt1), IIf(cap, Bit6, Bit5)) = BitByte(Info(cnt1 + 1), IIf(cap, Bit5, Bit6))
        BitByte(Info(cnt1), IIf(cap, Bit7, Bit6)) = BitByte(Info(cnt1 + 1), IIf(cap, Bit6, Bit7))
        BitByte(Info(cnt1), IIf(cap, Bit8, Bit7)) = BitByte(Info(cnt1 + 1), IIf(cap, Bit7, Bit8))
        BitByte(Info(cnt1), IIf(cap, Bit5, Bit8)) = BitByte(Info(cnt1 + 1), IIf(cap, Bit8, Bit5))
    Next
    BitByte(Info(ubi), IIf(cap, Bit6, Bit5)) = BitByte(swp, Bit1)
    BitByte(Info(ubi), IIf(cap, Bit7, Bit6)) = BitByte(swp, Bit2)
    BitByte(Info(ubi), IIf(cap, Bit8, Bit7)) = BitByte(swp, Bit3)
    BitByte(Info(ubi), IIf(cap, Bit5, Bit8)) = BitByte(swp, Bit4)
        
    EnKrimpt = Info
        
End Function

Private Function DeKrimpt(Info() As Byte, Seed() As Byte) As Byte()
    
    Dim pin As Byte
    Dim swp As Byte
    Dim cap As Boolean
    
    Dim cnt1 As Long
    Dim cnt2 As Long
    
    Dim lbi As Long
    Dim ubi As Long
    Dim lbs As Long
    Dim ubs As Long
        
    lbi = LBound(Info)
    ubi = UBound(Info)
    lbs = LBound(Seed)
    ubs = UBound(Seed)
    
    BitByte(pin, Bit1) = BitByte(Seed(lbs), Bit6) Or BitByte(Seed(ubs), Bit2)
    BitByte(pin, Bit2) = BitByte(Seed(ubs), Bit4) Or BitByte(Seed(lbs), Bit1)
    BitByte(pin, Bit3) = BitByte(Seed(lbs), Bit8) Or BitByte(Seed(ubs), Bit5)
    BitByte(pin, Bit4) = BitByte(Seed(ubs), Bit7) Or BitByte(Seed(lbs), Bit3)
    
    cap = (BitByte(pin, Bit1) Or BitByte(pin, Bit2)) And (BitByte(pin, Bit3) Or BitByte(pin, Bit4))

    BitByte(swp, Bit1) = BitByte(Info(ubi), IIf(cap, Bit6, Bit5))
    BitByte(swp, Bit2) = BitByte(Info(ubi), IIf(cap, Bit7, Bit6))
    BitByte(swp, Bit3) = BitByte(Info(ubi), IIf(cap, Bit8, Bit7))
    BitByte(swp, Bit4) = BitByte(Info(ubi), IIf(cap, Bit5, Bit8))
    For cnt1 = ubi To (lbi + 1) Step -1
        
        BitByte(Info(cnt1), IIf(cap, Bit5, Bit6)) = BitByte(Info(cnt1 - 1), IIf(cap, Bit6, Bit5))
        BitByte(Info(cnt1), IIf(cap, Bit6, Bit7)) = BitByte(Info(cnt1 - 1), IIf(cap, Bit7, Bit6))
        BitByte(Info(cnt1), IIf(cap, Bit7, Bit8)) = BitByte(Info(cnt1 - 1), IIf(cap, Bit8, Bit7))
        BitByte(Info(cnt1), IIf(cap, Bit8, Bit5)) = BitByte(Info(cnt1 - 1), IIf(cap, Bit5, Bit8))
    Next
    BitByte(Info(lbi), IIf(cap, Bit5, Bit6)) = BitByte(swp, Bit1)
    BitByte(Info(lbi), IIf(cap, Bit6, Bit7)) = BitByte(swp, Bit2)
    BitByte(Info(lbi), IIf(cap, Bit7, Bit8)) = BitByte(swp, Bit3)
    BitByte(Info(lbi), IIf(cap, Bit8, Bit5)) = BitByte(swp, Bit4)
    
    For cnt1 = lbi To ubi
    
        Select Case (-BitByte(Info(cnt1), Bit3)) & (-BitByte(Info(cnt1), Bit4)) & (-BitByte(pin, Bit1)) & (-BitByte(pin, Bit2))
            Case "0000"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True
            Case "1100"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
            Case "0100"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False
            Case "1000"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True

            Case "0011"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True
            Case "1011"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
            Case "0111"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False
            Case "1111"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True

            Case "0110"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True
            Case "1110"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
            Case "0010"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False
            Case "1010"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True

            Case "1001"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = True
            Case "0101"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = False
            Case "1101"
                BitByte(Info(cnt1), Bit3) = True
                BitByte(Info(cnt1), Bit4) = False
            Case "0001"
                BitByte(Info(cnt1), Bit3) = False
                BitByte(Info(cnt1), Bit4) = True

        End Select
        
    Next

    For cnt1 = ubs To lbs Step -1
        
        For cnt2 = lbi To ubi
            
            Select Case (-BitByte(Info(cnt2), Bit1)) & (-BitByte(Info(cnt2), Bit2)) & (-BitByte(Seed(cnt1), Bit1)) & (-BitByte(Seed(cnt1), Bit2))
                Case "0000"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
                Case "1100"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
                Case "0100"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
                Case "1000"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True

                Case "0011"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
                Case "1011"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
                Case "0111"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
                Case "1111"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True

                Case "0110"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
                Case "1110"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
                Case "0010"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
                Case "1010"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True

                Case "1001"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = True
                Case "0101"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = False
                Case "1101"
                    BitByte(Info(cnt2), Bit1) = True
                    BitByte(Info(cnt2), Bit2) = False
                Case "0001"
                    BitByte(Info(cnt2), Bit1) = False
                    BitByte(Info(cnt2), Bit2) = True
    
            End Select
        
        Next
    
    Next
    
    DeKrimpt = Info

End Function

Private Property Let BitByte(ByRef bThis As Byte, ByRef bBit As Byte, ByRef nValue As Boolean)
    If (bThis And bBit) And (Not nValue) Then
        bThis = bThis - bBit
    ElseIf (Not (bThis And bBit)) And nValue Then
        bThis = bThis Or bBit
    End If
End Property
Private Property Get BitByte(ByRef bThis As Byte, ByRef bBit As Byte) As Boolean
    BitByte = (bThis And bBit)
End Property
