Attribute VB_Name = "modAudio"
Option Explicit
Option Compare Binary
#Const modAudio = -1

Public Type AudioHead
    bPart(0 To 3) As Byte
    lInfo As Long
End Type

Public Type AudioInfo
    wFormatSpecific As Integer
    wNumberOfChannels As Integer
    lSamplesPerSecond As Long
    lBytesPerSecond As Long
    wChannelBandwidth As Integer
    wBitsPerSample As Integer
End Type

Public Type AudioData
    lBytes As Long
    wWave() As Integer
End Type

Public Type AudioFile
    wInfoLen As Integer
    Infos() As AudioInfo
    
    wDataLen As Integer
    Datas() As AudioData
    
    wOtherLen As Integer
    Other() As AudioHead
    bRemain() As Byte
End Type

' API Declarations
Public Declare Sub RtlMoveMemory Lib "kernel32" (ByVal Dest As Long, ByVal Source As Long, ByVal Length As Long)

Public Declare Function sndPlaySound Lib "WINMM.DLL" Alias "sndPlaySoundA" (ByVal lpszSoundName As String, ByVal uFlags As Long) As Long

Public Declare Function PlaySound Lib "WINMM.DLL" Alias "PlaySoundA" (ByRef Sound As Any, ByVal hLib As Long, ByVal lngFlag As Long) As Long      'BOOL

Public Enum sndConst
    SND_ASYNC = &H1 ' play asynchronously
    SND_LOOP = &H8 ' loop the sound until Next sndPlaySound
    SND_MEMORY = &H4 ' lpszSoundName points To a memory file
    SND_NODEFAULT = &H2 ' silence Not default, If sound not found
    SND_NOSTOP = &H10 ' don't stop any currently playing sound
    SND_SYNC = &H0 ' play synchronously (default), halts prog use till done playing
End Enum

Public Sub WavePCM16bitStereo(ByRef aFile As AudioFile)
    With aFile
        Erase aFile.Infos
        Erase aFile.Other
        .wOtherLen = 0
        .wInfoLen = 1
        ReDim aFile.Infos(1 To 1) As AudioInfo
    End With
    With aFile.Infos(1)
        .wFormatSpecific = 1
        .wNumberOfChannels = 2
        .lSamplesPerSecond = 44100
        .lBytesPerSecond = 176400
        .wChannelBandwidth = 4
        .wBitsPerSample = 16
    End With
End Sub
Public Function ArraySize(InArray) As Single
 ArraySize = CSng((UBound(InArray) + -CInt(Not CBool(-LBound(InArray)))))
End Function

Public Static Sub PlayWaveSound(ByRef WaveData() As Byte)
    Static SoundByte() As Byte
    SoundByte = WaveData
    PlaySound SoundByte(0), 0, SND_MEMORY Or SND_NODEFAULT Or SND_ASYNC
End Sub

Private Function WaveFileSize(ByRef aFile As AudioFile) As Long
    Dim i As Long
    If aFile.wInfoLen > 0 Then
        For i = 1 To aFile.wInfoLen
            WaveFileSize = WaveFileSize + LenB(aFile.Infos(i))
            WaveFileSize = WaveFileSize + 8
        Next
    End If
    If aFile.wDataLen > 0 Then
        For i = 1 To aFile.wDataLen
            WaveFileSize = WaveFileSize + (ArraySize(aFile.Datas(i).wWave) * 2)
            WaveFileSize = WaveFileSize + 8
        Next
    End If
    If aFile.wOtherLen > 0 Then
        For i = 1 To aFile.wOtherLen
            WaveFileSize = WaveFileSize + aFile.Other(i).lInfo
            WaveFileSize = WaveFileSize + 8
        Next
    End If
    WaveFileSize = WaveFileSize + 4
End Function

Public Function WaveInfosEqual(ByRef Info1 As AudioInfo, ByRef Info2 As AudioInfo) As Boolean
    WaveInfosEqual = (Info1.lBytesPerSecond = Info2.lBytesPerSecond) And _
                         (Info1.lSamplesPerSecond = Info2.lSamplesPerSecond) And _
                          (Info1.wBitsPerSample = Info2.wBitsPerSample) And _
                           (Info1.wChannelBandwidth = Info2.wChannelBandwidth) And _
                            (Info1.wFormatSpecific = Info2.wFormatSpecific) And _
                             (Info1.wNumberOfChannels = Info2.wNumberOfChannels)
End Function

Public Function WaveSilentAudio(ByRef aFile As AudioFile, ByVal DurationMS As Single) As Byte()
    Dim outBytes() As Byte
    ReDim outBytes(0 To 3) As Byte
    AddDesc outBytes, "RIFF", 0
    AddLong outBytes, WaveFileSize(aFile)
    AddDesc outBytes, "WAVE"
    Dim i As Long
    If aFile.wInfoLen > 0 Then
        For i = 1 To aFile.wInfoLen
            AddDesc outBytes, "fmt "
            AddLong outBytes, LenB(aFile.Infos(i))
            ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + LenB(aFile.Infos(i))) As Byte
            RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (LenB(aFile.Infos(i)) - 1))), ByVal VarPtr(aFile.Infos(i)), LenB(aFile.Infos(i))
            If aFile.wDataLen >= i Then
                AddDesc outBytes, "data"
                DurationMS = (((aFile.Infos(i).lSamplesPerSecond * (DurationMS / 1000)) * aFile.Infos(i).wBitsPerSample) / aFile.Infos(i).wChannelBandwidth)
                AddLong outBytes, DurationMS
                ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + DurationMS) As Byte
            End If
        Next
    End If
    If aFile.wOtherLen > 0 Then
        Dim runTotal As Long
        For i = 1 To aFile.wOtherLen
            AddDesc outBytes, GetDesc(aFile.Other(i))
            AddLong outBytes, aFile.Other(i).lInfo
            ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + aFile.Other(i).lInfo) As Byte
            RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (aFile.Other(i).lInfo - 1))), ByVal VarPtr(aFile.bRemain(LBound(aFile.bRemain) + runTotal)), aFile.Other(i).lInfo
            runTotal = runTotal + aFile.Other(i).lInfo
        Next
    End If
    WaveSilentAudio = outBytes
End Function

Public Sub WaveFilter(ByRef aFile As AudioFile, ByVal StartTimeMS As Single, ByVal DurationMS As Single, ByRef definedFilters As ScriptControl, ByVal filterFunction As String)
    If aFile.wDataLen > 0 And aFile.wInfoLen > 0 Then
        StartTimeMS = Round(aFile.Infos(1).lBytesPerSecond * (StartTimeMS / 1000))
        DurationMS = Round(aFile.Infos(1).lBytesPerSecond * (DurationMS / 1000))
        
        Dim i As Long
        For i = 1 To (DurationMS / 2)
            aFile.Datas(1).wWave((StartTimeMS / 2) + i) = definedFilters.Eval((Replace(filterFunction, "%", CStr(aFile.Datas(1).wWave((StartTimeMS / 2) + i)))))
        Next
    Else
        err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
    End If
    
End Sub

Public Function WaveCombine(ByRef DestWave() As Byte, ByVal DestStartTimeMS As Single, ByRef SourceWave() As Byte, ByVal SourceDurationTimeMS As Single) As Byte()
    
    Dim aFile1 As AudioFile
    Dim aFile2 As AudioFile
    
    aFile1 = WaveBytesAsAudio(DestWave)
    aFile2 = WaveBytesAsAudio(SourceWave)
 
    If aFile1.wDataLen > 0 And aFile1.wInfoLen > 0 And _
        aFile2.wDataLen > 0 And aFile2.wInfoLen > 0 Then
    
        DestStartTimeMS = Round(aFile1.Infos(1).lBytesPerSecond * (DestStartTimeMS / 1000))
        SourceDurationTimeMS = Round(aFile2.Infos(1).lBytesPerSecond * (SourceDurationTimeMS / 1000))
        
        Dim newData() As Integer
        ReDim Preserve newData(1 To ((aFile1.Datas(1).lBytes + SourceDurationTimeMS) \ 2)) As Integer
        
        If DestStartTimeMS > 0 Then
            RtlMoveMemory VarPtr(newData(LBound(newData))), _
                ByVal VarPtr(aFile1.Datas(1).wWave(LBound(aFile1.Datas(1).wWave))), DestStartTimeMS
        End If
        
        RtlMoveMemory VarPtr(newData(LBound(newData) + (DestStartTimeMS / 2))), _
            ByVal VarPtr(aFile2.Datas(1).wWave(LBound(aFile2.Datas(1).wWave))), SourceDurationTimeMS
            
        If (aFile1.Datas(1).lBytes - DestStartTimeMS) > 0 Then
            RtlMoveMemory VarPtr(newData(LBound(newData) + ((DestStartTimeMS + SourceDurationTimeMS) \ 2))), _
                ByVal VarPtr(aFile1.Datas(1).wWave(LBound(aFile1.Datas(1).wWave) + (DestStartTimeMS \ 2))), _
                (aFile1.Datas(1).lBytes - DestStartTimeMS)
        End If
        
        aFile1.Datas(1).lBytes = aFile1.Datas(1).lBytes + SourceDurationTimeMS
        Erase aFile1.Datas(1).wWave
        aFile1.Datas(1).wWave = newData
        
        WaveRecordToBytes aFile1, WaveCombine
        
        Erase newData
        WaveResetRecord aFile1
        WaveResetRecord aFile2
    Else
        err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
    End If
End Function

Public Function WaveAudioPartial(ByRef aFile As AudioFile, ByVal StartTimeMS As Single, ByVal DurationMS As Single) As Byte()
    Dim outBytes() As Byte
    ReDim outBytes(0 To 3) As Byte
    AddDesc outBytes, "RIFF", 0
    AddLong outBytes, WaveFileSize(aFile)
    AddDesc outBytes, "WAVE"
    Dim i As Long
    If aFile.wInfoLen > 0 Then
        For i = 1 To aFile.wInfoLen
            AddDesc outBytes, "fmt "
            AddLong outBytes, LenB(aFile.Infos(i))
            ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + LenB(aFile.Infos(i))) As Byte
            RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (LenB(aFile.Infos(i)) - 1))), ByVal VarPtr(aFile.Infos(i)), LenB(aFile.Infos(i))
            If aFile.wDataLen >= i Then
                AddDesc outBytes, "data"
                StartTimeMS = Round(aFile.Infos(1).lBytesPerSecond * (StartTimeMS / 1000))
                DurationMS = Round(aFile.Infos(1).lBytesPerSecond * (DurationMS / 1000))
                
               ' StartTimeMS = (((aFile.Infos(i).lSamplesPerSecond * (StartTimeMS / 1000)) * aFile.Infos(i).wBitsPerSample) / aFile.Infos(i).wChannelBandwidth)
               ' DurationMS = (((aFile.Infos(i).lSamplesPerSecond * (DurationMS / 1000)) * aFile.Infos(i).wBitsPerSample) / aFile.Infos(i).wChannelBandwidth)
                
                AddLong outBytes, DurationMS
                ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + DurationMS) As Byte
                RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (DurationMS - 1))), _
                    ByVal VarPtr(aFile.Datas(i).wWave(LBound(aFile.Datas(i).wWave) + StartTimeMS)), DurationMS
            End If
        Next
    End If
    If aFile.wOtherLen > 0 Then
        Dim runTotal As Long
        For i = 1 To aFile.wOtherLen
            AddDesc outBytes, GetDesc(aFile.Other(i))
            AddLong outBytes, aFile.Other(i).lInfo
            ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + aFile.Other(i).lInfo) As Byte
            RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (aFile.Other(i).lInfo - 1))), ByVal VarPtr(aFile.bRemain(LBound(aFile.bRemain) + runTotal)), aFile.Other(i).lInfo
            runTotal = runTotal + aFile.Other(i).lInfo
        Next
    End If
    WaveAudioPartial = outBytes

End Function
Public Function WaveBytesAsAudio(ByRef inBytes() As Byte) As AudioFile
    Dim riff As AudioHead
    Dim aFile As AudioFile
    Dim validity As Integer
    Dim fNum As Long
    fNum = LBound(inBytes)
    Dim temp() As Byte
    riff = GetHead(inBytes, fNum)
    If GetDesc(riff) = "RIFF" Then
        Debug.Print "File Size " & riff.lInfo
        Do While fNum < UBound(inBytes)
            riff = GetHead(inBytes, fNum)
            If GetDesc(riff) = "WAVE" Then
                fNum = fNum - 4
                Do While fNum < UBound(inBytes)
                    riff = GetHead(inBytes, fNum)
                    Select Case Trim(GetDesc(riff))
                        Case "DATA"
                            aFile.wDataLen = aFile.wDataLen + 1
                            ReDim Preserve aFile.Datas(1 To aFile.wDataLen) As AudioData
                            aFile.Datas(aFile.wDataLen).lBytes = riff.lInfo

                            If riff.lInfo > 0 Then
                                ReDim aFile.Datas(aFile.wDataLen).wWave(1 To (riff.lInfo / 2)) As Integer
                                RtlMoveMemory VarPtr(aFile.Datas(aFile.wDataLen).wWave(LBound(aFile.Datas(aFile.wDataLen).wWave))), _
                                     ByVal VarPtr(inBytes(fNum)), riff.lInfo
                                fNum = fNum + riff.lInfo
                                If aFile.wInfoLen = aFile.wDataLen - 1 Then
                                    aFile.wInfoLen = aFile.wInfoLen + 1
                                    ReDim Preserve aFile.Infos(1 To aFile.wInfoLen) As AudioInfo
                                    aFile.Infos(aFile.wInfoLen) = aFile.Infos(aFile.wDataLen)
                                End If
                            End If
                            Debug.Print "Data Size " & aFile.Datas(aFile.wDataLen).lBytes & " bytes"
                            Debug.Print "Data Length " & (WaveMilliseconds(aFile) / 1000) & " s"
    
                            'Debug.Print "Data Length " & Round(((riff.lInfo / aFile.Infos(aFile.wInfoLen).lBytesPerSecond) * 1000)) & " ms"
                            validity = validity + 1
                        Case "FMT"
                            aFile.wInfoLen = aFile.wInfoLen + 1
                            ReDim Preserve aFile.Infos(1 To aFile.wInfoLen) As AudioInfo
                            If LenB(aFile.Infos(aFile.wInfoLen)) = riff.lInfo Then
                                RtlMoveMemory VarPtr(aFile.Infos(aFile.wInfoLen)), ByVal VarPtr(inBytes(fNum)), LenB(aFile.Infos(aFile.wInfoLen))
                                fNum = fNum + LenB(aFile.Infos(aFile.wInfoLen))
                                 If aFile.wInfoLen - 2 = aFile.wDataLen Then
                                    aFile.wDataLen = aFile.wDataLen + 1
                                    ReDim Preserve aFile.Datas(1 To aFile.wDataLen) As AudioData
                                    aFile.Datas(aFile.wDataLen) = aFile.Datas(aFile.wDataLen - 1)
                                End If
                            
                                With aFile.Infos(aFile.wInfoLen)
                                    Debug.Print "Format Specific " & .wFormatSpecific
                                    Debug.Print "Number Of Channels " & .wNumberOfChannels
                                    Debug.Print "Samples Per Second " & .lSamplesPerSecond
                                    Debug.Print "Bytes Per Second " & .lBytesPerSecond
                                    Debug.Print "Channel Bandwidth " & .wChannelBandwidth
                                    Debug.Print "Bits Per Sample " & .wBitsPerSample
                                End With
                                If aFile.Infos(aFile.wInfoLen).wBitsPerSample <> 16 Then
                                    err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
                                Else
                                    validity = validity + 1
                                End If
                            Else
                                ReDim Preserve aFile.Infos(1 To aFile.wInfoLen - 1) As AudioInfo
                                aFile.wInfoLen = aFile.wInfoLen - 1
                                fNum = fNum + riff.lInfo
                            End If
                        
                        Case Else
                            aFile.wOtherLen = aFile.wOtherLen + 1
                            ReDim Preserve aFile.Other(1 To aFile.wOtherLen) As AudioHead
                            aFile.Other(aFile.wOtherLen) = riff

                            ReDim temp(1 To aFile.Other(aFile.wOtherLen).lInfo) As Byte
                            RtlMoveMemory VarPtr(temp(LBound(temp))), ByVal VarPtr(inBytes(fNum)), aFile.Other(aFile.wOtherLen).lInfo
                            If aFile.wOtherLen = 1 Then
                                aFile.bRemain = temp
                            Else
                                ReDim Preserve aFile.bRemain(LBound(aFile.bRemain) To UBound(aFile.bRemain) + aFile.Other(aFile.wOtherLen).lInfo) As Byte
                                RtlMoveMemory VarPtr(aFile.bRemain(UBound(aFile.bRemain) - (aFile.Other(aFile.wOtherLen).lInfo - 1))), ByVal VarPtr(temp(LBound(temp))), aFile.Other(aFile.wOtherLen).lInfo
                            End If
                            fNum = fNum + aFile.Other(aFile.wOtherLen).lInfo

                    End Select
                Loop
            Else
                err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
            End If
        Loop
    Else
        err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
    End If
    If validity < 2 Then
        err.Raise 321, , "Invalid file type, expected riff (a few single musical note, tone, key or chord blended as a conbined melodic harmony, continious sound or portion of a song.... a riff)."
    End If
    WaveBytesAsAudio = aFile
End Function

Public Sub WaveRecordToBytes(ByRef aFile As AudioFile, ByRef outBytes() As Byte)
    ReDim outBytes(0 To 3) As Byte
    AddDesc outBytes, "RIFF", 0
    AddLong outBytes, WaveFileSize(aFile)
    AddDesc outBytes, "WAVE"
    Dim i As Long
    If aFile.wInfoLen > 0 Then
        For i = 1 To aFile.wInfoLen
            AddDesc outBytes, "fmt "
            AddLong outBytes, LenB(aFile.Infos(i))
            ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + LenB(aFile.Infos(i))) As Byte
            RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (LenB(aFile.Infos(i)) - 1))), ByVal VarPtr(aFile.Infos(i)), LenB(aFile.Infos(i))
            If aFile.wDataLen >= i Then
                AddDesc outBytes, "data"
                AddLong outBytes, aFile.Datas(aFile.wDataLen).lBytes
                ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + aFile.Datas(aFile.wDataLen).lBytes) As Byte
                RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (aFile.Datas(aFile.wDataLen).lBytes - 1))), _
                    ByVal VarPtr(aFile.Datas(i).wWave(LBound(aFile.Datas(i).wWave))), aFile.Datas(aFile.wDataLen).lBytes
            End If
        Next
    End If
    If aFile.wOtherLen > 0 Then
        Dim runTotal As Long
        For i = 1 To aFile.wOtherLen
            AddDesc outBytes, GetDesc(aFile.Other(i))
            AddLong outBytes, aFile.Other(i).lInfo
            ReDim Preserve outBytes(LBound(outBytes) To UBound(outBytes) + aFile.Other(i).lInfo) As Byte
            RtlMoveMemory VarPtr(outBytes(UBound(outBytes) - (aFile.Other(i).lInfo - 1))), ByVal VarPtr(aFile.bRemain(LBound(aFile.bRemain) + runTotal)), aFile.Other(i).lInfo
            runTotal = runTotal + aFile.Other(i).lInfo
        Next
    End If
End Sub
Public Function WaveMilliseconds(ByRef aFile As AudioFile) As Long
    With aFile.Infos(aFile.wInfoLen)
        Dim factor As Single
        Dim remain As Single
        Dim multiplyer As Single
          
        factor = ((.lSamplesPerSecond * .wNumberOfChannels * .wBitsPerSample) \ (.wNumberOfChannels * .wChannelBandwidth))
        multiplyer = (aFile.Datas(aFile.wDataLen).lBytes \ factor)
        remain = (aFile.Datas(aFile.wDataLen).lBytes Mod factor)
                                
        WaveMilliseconds = ((multiplyer + (((factor / (factor - remain)) / 100))) * 1000)
    End With
End Function
Public Sub WaveLoadFromFile(ByVal FileName As String, ByRef aFile As AudioFile)
    Dim riff As AudioHead
    Dim validity As Integer
    Dim temp() As Byte
    Dim fNum As Long
    fNum = FreeFile
    Open FileName For Binary As #fNum
    Get #fNum, 1, riff
    If GetDesc(riff) = "RIFF" Then
        WaveResetRecord aFile
        Debug.Print "File Size " & riff.lInfo
        Do While Not EOF(fNum)
            Get #fNum, Seek(fNum), riff
            If GetDesc(riff) = "WAVE" Then
                RtlMoveMemory VarPtr(riff), ByVal VarPtr(riff.lInfo), 4
                Get #fNum, Seek(fNum), riff.lInfo
                Do While Not EOF(fNum)
                    Select Case Trim(GetDesc(riff))
                        Case "DATA"
                            aFile.wDataLen = aFile.wDataLen + 1
                            ReDim Preserve aFile.Datas(1 To aFile.wDataLen) As AudioData
                            aFile.Datas(aFile.wDataLen).lBytes = riff.lInfo
                            If riff.lInfo > 0 Then
                            
                                ReDim aFile.Datas(aFile.wDataLen).wWave(1 To (riff.lInfo / 2)) As Integer
                                Get #fNum, Seek(fNum), aFile.Datas(aFile.wDataLen).wWave
                                If aFile.wInfoLen = aFile.wDataLen - 1 Then
                                    aFile.wInfoLen = aFile.wInfoLen + 1
                                    ReDim Preserve aFile.Infos(1 To aFile.wInfoLen) As AudioInfo
                                    aFile.Infos(aFile.wInfoLen) = aFile.Infos(aFile.wDataLen)
                                End If
                            End If

                            Debug.Print "Data Size " & aFile.Datas(aFile.wDataLen).lBytes & " bytes"

                            Debug.Print "Data Length " & (WaveMilliseconds(aFile) / 1000) & " s"
 
                            validity = validity + 1
                                                    
                        Case "FMT"
                            aFile.wInfoLen = aFile.wInfoLen + 1
                            ReDim Preserve aFile.Infos(1 To aFile.wInfoLen) As AudioInfo
                            If LenB(aFile.Infos(aFile.wInfoLen)) = riff.lInfo Then
                                Get #fNum, Seek(fNum), aFile.Infos(aFile.wInfoLen)
                                If aFile.wInfoLen - 2 = aFile.wDataLen Then
                                    aFile.wDataLen = aFile.wDataLen + 1
                                    ReDim Preserve aFile.Datas(1 To aFile.wDataLen) As AudioData
                                    aFile.Datas(aFile.wDataLen) = aFile.Datas(aFile.wDataLen - 1)
                                End If
                            
                                With aFile.Infos(aFile.wInfoLen)
                                    Debug.Print "Format Specific " & .wFormatSpecific
                                    Debug.Print "Number Of Channels " & .wNumberOfChannels
                                    Debug.Print "Samples Per Second " & .lSamplesPerSecond
                                    Debug.Print "Bytes Per Second " & .lBytesPerSecond
                                    Debug.Print "Channel Bandwidth " & .wChannelBandwidth
                                    Debug.Print "Bits Per Sample " & .wBitsPerSample
                                End With
                                If aFile.Infos(aFile.wInfoLen).wBitsPerSample <> 16 Then
                                    err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
                                Else
                                    validity = validity + 1
                                End If
                            Else
                                ReDim Preserve aFile.Infos(1 To aFile.wInfoLen - 1) As AudioInfo
                                aFile.wInfoLen = aFile.wInfoLen - 1
                                Seek #fNum, Seek(fNum) + riff.lInfo
                            End If
                        Case Else
                            aFile.wOtherLen = aFile.wOtherLen + 1
                            ReDim Preserve aFile.Other(1 To aFile.wOtherLen) As AudioHead
                            aFile.Other(aFile.wOtherLen) = riff
                            ReDim temp(1 To aFile.Other(aFile.wOtherLen).lInfo) As Byte
                            Get #fNum, Seek(fNum), temp
                            If aFile.wOtherLen = 1 Then
                                aFile.bRemain = temp
                            Else
                                ReDim Preserve aFile.bRemain(LBound(aFile.bRemain) To UBound(aFile.bRemain) + aFile.Other(aFile.wOtherLen).lInfo) As Byte
                                RtlMoveMemory VarPtr(aFile.bRemain(UBound(aFile.bRemain) - (aFile.Other(aFile.wOtherLen).lInfo - 1))), ByVal VarPtr(temp(LBound(temp))), aFile.Other(aFile.wOtherLen).lInfo
                            End If
                    End Select
                    Get #fNum, Seek(fNum), riff
                Loop
            Else
                err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
            End If
        Loop
    Else
        err.Raise 321, , "The file you have selected is not a RIFF nor WAVE file.  Only 16-bits (Standard PCM) wave format is supported."
    End If
    Close #fNum
    If validity < 2 Then
        err.Raise 321, , "Invalid file type, expected riff (a few single musical note, tone, key or chord blended as a conbined melodic harmony, continious sound or portion of a song.... a riff)."
    End If
End Sub

Public Sub AddDesc(ByRef audio() As Byte, ByVal Desc As String, Optional ByVal StartIndex As Long = -1)
    If StartIndex = -1 Then
        ReDim Preserve audio(LBound(audio) To UBound(audio) + 4) As Byte
        StartIndex = UBound(audio) - 3
    End If
    audio(StartIndex + 0) = Asc(Mid(Desc, 1, 1))
    audio(StartIndex + 1) = Asc(Mid(Desc, 2, 1))
    audio(StartIndex + 2) = Asc(Mid(Desc, 3, 1))
    audio(StartIndex + 3) = Asc(Mid(Desc, 4, 1))
End Sub

Public Sub AddLong(ByRef audio() As Byte, ByVal lValue As Long)
    ReDim Preserve audio(LBound(audio) To UBound(audio) + 4) As Byte
    RtlMoveMemory VarPtr(audio(UBound(audio) - 3)), ByVal VarPtr(lValue) + 0, 4
End Sub

Public Function GetHead(ByRef inBytes() As Byte, ByRef Idx As Long) As AudioHead
    RtlMoveMemory VarPtr(GetHead), ByVal VarPtr(inBytes(Idx)), LenB(GetHead)
    Idx = Idx + LenB(GetHead)
End Function

Public Function GetDesc(ByRef header As AudioHead) As String
    GetDesc = UCase(Chr(header.bPart(0)) & Chr(header.bPart(1)) & Chr(header.bPart(2)) & Chr(header.bPart(3)))
End Function

Public Sub WaveResetRecord(ByRef aFile As AudioFile)
    Dim i As Long
    If aFile.wInfoLen > 0 Then
        For i = LBound(aFile.Infos) To UBound(aFile.Infos)
            aFile.Infos(i).wFormatSpecific = 0
            aFile.Infos(i).wNumberOfChannels = 0
            aFile.Infos(i).lSamplesPerSecond = 0
            aFile.Infos(i).lBytesPerSecond = 0
            aFile.Infos(i).wChannelBandwidth = 0
            aFile.Infos(i).wBitsPerSample = 0
        Next
        Erase aFile.Infos
        aFile.wInfoLen = 0
    End If
    If aFile.wDataLen > 0 Then
        For i = LBound(aFile.Datas) To UBound(aFile.Datas)
            Erase aFile.Datas(i).wWave
        Next
        Erase aFile.Datas
        aFile.wDataLen = 0
    End If
    If aFile.wOtherLen > 0 Then
        Erase aFile.Other
        Erase aFile.bRemain
        aFile.wOtherLen = 0
    End If
End Sub

