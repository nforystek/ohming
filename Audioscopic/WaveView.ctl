VERSION 5.00
Object = "{0E59F1D2-1FBE-11D0-8FF2-00A0D10038BC}#1.0#0"; "msscript.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.UserControl WaveView 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H80000005&
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   3600
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   7710
   ClipControls    =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ScaleHeight     =   3600
   ScaleWidth      =   7710
   Begin MSScriptControlCtl.ScriptControl FilterScripts 
      Left            =   6180
      Top             =   600
      _ExtentX        =   1005
      _ExtentY        =   1005
      AllowUI         =   0   'False
   End
   Begin VB.CommandButton ResetButton 
      Appearance      =   0  'Flat
      DisabledPicture =   "WaveView.ctx":0000
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   4320
      MaskColor       =   &H00FF00FF&
      Picture         =   "WaveView.ctx":04D2
      Style           =   1  'Graphical
      TabIndex        =   7
      Top             =   2280
      UseMaskColor    =   -1  'True
      Width           =   255
   End
   Begin VB.Timer Blinkers 
      Interval        =   20
      Left            =   1020
      Top             =   2280
   End
   Begin VB.PictureBox PicVolscale 
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      FillColor       =   &H8000000F&
      BeginProperty Font 
         Name            =   "Small Fonts"
         Size            =   6.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000FF&
      Height          =   735
      Left            =   4020
      ScaleHeight     =   735
      ScaleWidth      =   315
      TabIndex        =   6
      Top             =   2640
      Width           =   315
   End
   Begin VB.PictureBox PicSlidebar 
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      DrawStyle       =   5  'Transparent
      FillColor       =   &H8000000F&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   375
      Left            =   420
      ScaleHeight     =   375
      ScaleWidth      =   2175
      TabIndex        =   5
      Top             =   3060
      Width           =   2175
      Begin VB.Shape ViewSlider 
         FillColor       =   &H8000000F&
         Height          =   315
         Left            =   120
         Top             =   60
         Width           =   1635
      End
   End
   Begin VB.CommandButton PlayButton 
      DisabledPicture =   "WaveView.ctx":09A4
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3660
      MaskColor       =   &H00FF00FF&
      Picture         =   "WaveView.ctx":0E76
      Style           =   1  'Graphical
      TabIndex        =   4
      Top             =   2520
      UseMaskColor    =   -1  'True
      Width           =   255
   End
   Begin VB.PictureBox PicRender 
      AutoRedraw      =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      ClipControls    =   0   'False
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1395
      Left            =   2340
      ScaleHeight     =   1395
      ScaleWidth      =   2295
      TabIndex        =   3
      Top             =   180
      Visible         =   0   'False
      Width           =   2295
   End
   Begin VB.CommandButton StopButton 
      DisabledPicture =   "WaveView.ctx":1348
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3600
      MaskColor       =   &H00FF00FF&
      Picture         =   "WaveView.ctx":181A
      Style           =   1  'Graphical
      TabIndex        =   2
      Top             =   3000
      UseMaskColor    =   -1  'True
      Width           =   255
   End
   Begin VB.CommandButton OpenButton 
      Appearance      =   0  'Flat
      DisabledPicture =   "WaveView.ctx":1CEC
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3000
      MaskColor       =   &H00FF00FF&
      Picture         =   "WaveView.ctx":21BE
      Style           =   1  'Graphical
      TabIndex        =   1
      Top             =   2700
      UseMaskColor    =   -1  'True
      Width           =   255
   End
   Begin MSComDlg.CommonDialog BrowseForWave 
      Left            =   120
      Top             =   180
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.PictureBox PicDisplay 
      Appearance      =   0  'Flat
      AutoRedraw      =   -1  'True
      AutoSize        =   -1  'True
      BackColor       =   &H80000005&
      BorderStyle     =   0  'None
      DragIcon        =   "WaveView.ctx":2690
      FillColor       =   &H00FFFFFF&
      ForeColor       =   &H80000008&
      Height          =   1455
      Left            =   840
      OLEDropMode     =   1  'Manual
      ScaleHeight     =   1455
      ScaleWidth      =   2775
      TabIndex        =   0
      Top             =   600
      Width           =   2775
      Begin VB.Line PlayCursor 
         BorderColor     =   &H00E0E0E0&
         Visible         =   0   'False
         X1              =   1200
         X2              =   1200
         Y1              =   300
         Y2              =   900
      End
      Begin VB.Line RightMarker 
         BorderColor     =   &H00404040&
         X1              =   360
         X2              =   360
         Y1              =   360
         Y2              =   1020
      End
      Begin VB.Line LeftMarker 
         BorderColor     =   &H00808080&
         Visible         =   0   'False
         X1              =   60
         X2              =   60
         Y1              =   360
         Y2              =   1080
      End
   End
   Begin VB.Image DragIcon 
      Height          =   480
      Left            =   3060
      Picture         =   "WaveView.ctx":2AD2
      Top             =   2220
      Visible         =   0   'False
      Width           =   480
   End
End
Attribute VB_Name = "WaveView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Private Type POINTAPI
        x As Long
        y As Long
End Type
Private Type RECT
        Left As Long
        Top As Long
        Right As Long
        Bottom As Long
End Type


' Declare Color Constants
Private Const cBlue = &HFF0000
Private Const cRed = &HFF
Private Const cGreen = &HFF00
Private Const cYellow = &HFFFF
Private Const cTeal = &H64FF64
Private Const cPurple = &HFF007D
Private Const cGray = &H7D7D7D
Private Const cOrange = &H96FF

'' Public Declarations
''################################
'Private wFileHeader As FileHeader
'Private wFormat As FormatChunk
'Private wChunk As ChunkHeader
'Private I_Data() As Integer
''################################


Private dBitsPerTwip As Single
Private FileLoaded As String
Private Peak As Single, Dip As Single

Private SoundPlay As Boolean
Private CursorAdjust As Integer

Private TotalTime As Single
Private ChannelTap As Single
Private DotsOnly As Boolean
Private MRatio As Single
Private DBZoom As Single

Private AudioFile As AudioFile

Private LastX As Single
Private LastY As Single
Private LastBtn As Integer
Private LaststartMS As Single
Private LaststopMS As Single
        
Private Declare Function WindowFromPoint Lib "user32" (ByVal xPoint As Long, ByVal yPoint As Long) As Long

Private Declare Function GetCursorPos Lib "user32" (lpPoint As POINTAPI) As Long
Private Declare Function GetWindowRect Lib "user32" (ByVal hwnd As Long, lpRect As RECT) As Long

Private Declare Function RegisterClipboardFormat Lib "user32.dll" Alias "RegisterClipboardFormatA" (ByVal lpszFormat$) As Integer
Private MyFormat As Integer

Public Event Click()

Public Property Get WaveMarkerStartMS() As Single
    LineRestrictions
    WaveMarkerStartMS = (TotalTime * ((LeftMarker.X1 - Screen.TwipsPerPixelX) / PicDisplay.Width))
End Property
Public Property Get WaveMarkerStopMS() As Single
    LineRestrictions
    WaveMarkerStopMS = (TotalTime * ((RightMarker.X1 + Screen.TwipsPerPixelX) / PicDisplay.Width))
End Property

Public Property Get WaveMarkerPlayMS() As Single
    LineRestrictions
    WaveMarkerPlayMS = (TotalTime * ((PlayCursor.X1 + Screen.TwipsPerPixelX) / PicDisplay.Width))
End Property
Public Property Get WaveLengthMS() As Single
    WaveLengthMS = TotalTime
End Property

Private Function ScrollOver() As Control
    Dim pt As POINTAPI

    Dim rct As RECT
    
    GetCursorPos pt
    
    GetWindowRect PicSlidebar.hwnd, rct

    If ((pt.x > rct.Left) And (pt.x < rct.Right)) And ((pt.y > rct.Top) And (pt.y < rct.Bottom)) Then
        Set ScrollOver = PicSlidebar
    Else
        GetWindowRect PicVolscale.hwnd, rct
        If (((pt.x > rct.Left) And (pt.x < rct.Right)) And ((pt.y > rct.Top) And (pt.y < rct.Bottom))) Then
            Set ScrollOver = PicVolscale
        Else
            Set ScrollOver = PicDisplay
        End If
    End If

End Function

Friend Sub ScrollUp()

    On Error GoTo catcherr
    
    Dim useScroller As Control
    Set useScroller = ScrollOver

    If Not useScroller Is Nothing Then
        
        Select Case useScroller.Name
                
            Case "PicSlidebar"
                PicSlidebar.Tag = (PicSlidebar.Width / 100)
                MoveViewPort (PicSlidebar.Tag * 2)
                PicSlidebar.Tag = 0
            Case "PicVolscale"
'                If DBZoom < 10 Then
'                    DBZoom = DBZoom + 0.2
'                End If
'                PicDisplay.Height = ((UserControl.Height - PicSlidebar.Height) * DBZoom)
'                PicDisplay.Top = (((UserControl.Height - PicSlidebar.Height) / 2) - (PicDisplay.Height / 2))
'
'                PicDisplay.PaintPicture PicRender.Image, 0, 0, PicDisplay.Width, PicDisplay.Height, 0, 0, PicRender.Width, PicRender.Height
'
'                RenderTimeLine
'                RenderFileInfo vbBlack
'                RenderCenters
'                CursorAdjust = 0
                
            Case Else

                Dim startMS As Single
                Dim stopMS As Single

                startMS = WaveMarkerStartMS
                stopMS = WaveMarkerStopMS
            
                PicDisplay.Width = PicDisplay.Width + (PicDisplay.Width / (Screen.Width / Screen.Height))

                PicDisplay.PaintPicture PicRender.Image, 0, 0, PicDisplay.Width, PicDisplay.Height, 0, 0, PicRender.Width, PicRender.Height
      
                ViewSlider.Width = (PicSlidebar.Width * ((UserControl.Width - PicVolscale.Width) / PicDisplay.Width))

                If PicDisplay.Left < 0 Then
                    ViewSlider.Left = (PicSlidebar.Width * (-PicDisplay.Left / PicDisplay.Width))
                End If

                If TotalTime > 0 Then
                    LeftMarker.X1 = (PicDisplay.Width * (startMS / TotalTime))
                    LeftMarker.X2 = LeftMarker.X1
                    RightMarker.X1 = (PicDisplay.Width * (stopMS / TotalTime))
                    RightMarker.X2 = RightMarker.X1
                End If
                
                RenderTimeLine
                RenderFileInfo vbBlack
                RenderCenters
                
                LineRestrictions
                CursorAdjust = 0
                PicDisplay.SetFocus
        End Select

    End If

    Set useScroller = Nothing
    
    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub
Friend Sub ScrollDown()

    On Error GoTo catcherr
    
    Dim useScroller As Control
    Set useScroller = ScrollOver

    If Not useScroller Is Nothing Then
        
        Select Case useScroller.Name
            Case "PicSlidebar"
                PicSlidebar.Tag = ((PicSlidebar.Width / 100) * 2)
                MoveViewPort (PicSlidebar.Tag / 2)
                PicSlidebar.Tag = 0
            Case "PicVolscale"
'                If DBZoom > 0.2 Then
'                    DBZoom = DBZoom - 0.2
'                End If
'
'                PicDisplay.Height = ((UserControl.Height - PicSlidebar.Height) * DBZoom)
'                PicDisplay.Top = (((UserControl.Height - PicSlidebar.Height) / 2) - (PicDisplay.Height / 2))
'
'                PicDisplay.PaintPicture PicRender.Image, 0, 0, PicDisplay.Width, PicDisplay.Height, 0, 0, PicRender.Width, PicRender.Height
'
'                RenderTimeLine
'                RenderFileInfo vbBlack
'                RenderCenters
'                CursorAdjust = 0
                
            Case Else

                Dim startMS As Single
                Dim stopMS As Single

                startMS = WaveMarkerStartMS
                stopMS = WaveMarkerStopMS
                
                If (PicDisplay.Width - (PicDisplay.Width / (Screen.Width / Screen.Height))) >= (17 * Screen.TwipsPerPixelX) Then
                    PicDisplay.Width = (PicDisplay.Width - (PicDisplay.Width / (Screen.Width / Screen.Height)))
                Else
                    PicDisplay.Width = (17 * Screen.TwipsPerPixelX)
                End If

                PicDisplay.PaintPicture PicRender.Image, 0, 0, PicDisplay.Width, PicDisplay.Height, 0, 0, PicRender.Width, PicRender.Height
 
                ViewSlider.Width = (PicSlidebar.Width * ((UserControl.Width - PicVolscale.Width) / PicDisplay.Width))
                
                If PicDisplay.Width < (UserControl.Width - PicVolscale.Width) Then
                    PicDisplay.Left = 0
                    ViewSlider.Left = 0
                Else
                    If PicDisplay.Left < 0 Then
                        ViewSlider.Left = (PicSlidebar.Width * (-PicDisplay.Left / PicDisplay.Width))
                    End If
                    If (PicDisplay.Width + PicDisplay.Left) < (UserControl.Width - PicVolscale.Width) And ((PicDisplay.Left < 0) And (PicDisplay.Width > (UserControl.Width - PicVolscale.Width))) Then
                        PicDisplay.Left = -(PicDisplay.Width - (UserControl.Width - PicVolscale.Width))
                        ViewSlider.Left = PicSlidebar.Width - ViewSlider.Width
                    End If
                End If

                If TotalTime > 0 Then
                    LeftMarker.X1 = (PicDisplay.Width * (startMS / TotalTime))
                    LeftMarker.X2 = LeftMarker.X1
                    RightMarker.X1 = (PicDisplay.Width * (stopMS / TotalTime))
                    RightMarker.X2 = RightMarker.X1
                End If

                RenderTimeLine
                RenderFileInfo vbBlack
                RenderCenters
                LineRestrictions
                CursorAdjust = 0
                PicDisplay.SetFocus
        End Select

    End If

    Set useScroller = Nothing
    
    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub

Private Sub MoveViewPort(ByVal x As Single)

    RenderFileInfo vbWhite
    If PicSlidebar.Tag > 0 Then
    
        If Not (ViewSlider.Left + (x - PicSlidebar.Tag) >= 0) Then
            PicSlidebar.Tag = -ViewSlider.Left
        ElseIf Not (ViewSlider.Left + (x - PicSlidebar.Tag) + ViewSlider.Width < PicSlidebar.Width) Then
            PicSlidebar.Tag = PicSlidebar.Width - (ViewSlider.Left + ViewSlider.Width)
        Else
            PicSlidebar.Tag = (x - PicSlidebar.Tag)
        End If
        
        PicDisplay.Left = PicDisplay.Left - (PicDisplay.Width * (PicSlidebar.Tag / PicSlidebar.Width))
        ViewSlider.Left = ViewSlider.Left + PicSlidebar.Tag
    
        RenderFileInfo vbBlack
        CursorAdjust = 0
        PicDisplay.SetFocus
    End If
End Sub

Private Sub LineRestrictions()
    
    If LeftMarker.X1 > RightMarker.X1 Then
        RightMarker.X2 = LeftMarker.X1
        LeftMarker.X1 = RightMarker.X1
        RightMarker.X1 = RightMarker.X2
        RightMarker.X2 = RightMarker.X1
        LeftMarker.X2 = LeftMarker.X1
    End If

    If LeftMarker.X1 <= 0 Then
        LeftMarker.X1 = Screen.TwipsPerPixelX
        LeftMarker.X2 = Screen.TwipsPerPixelX
    End If

    If RightMarker.X1 >= PicDisplay.Width Then
        RightMarker.X1 = PicDisplay.Width - Screen.TwipsPerPixelX
        RightMarker.X2 = PicDisplay.Width - Screen.TwipsPerPixelX
    End If

End Sub

Public Property Get WaveData(Optional ByVal StartTimeMS As Single = 0, Optional ByVal DurationTimeMS As Single = -1) As Byte()

    On Error GoTo catcherr
    
    If StartTimeMS > TotalTime Or StartTimeMS < 0 Then
        err.Raise 8, "WaveData", "Invalid or exceeding start time."
    Else
        If DurationTimeMS = -1 Then DurationTimeMS = TotalTime - StartTimeMS
        If StartTimeMS + DurationTimeMS <= TotalTime Then
            WaveData = WaveAudioPartial(AudioFile, StartTimeMS, DurationTimeMS)
        Else
            err.Raise 9, "WaveData", "Invalid or exceeding  duration time."
        End If
    End If
    
    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Property

'Public Sub WaveMixIn(ByRef SourceWaveView As WaveView, Optional ByVal DestStartTimeMS As Single = 0, Optional ByVal SourceStartTimeMS As Single = 0, Optional ByVal SourceDurationTimeMS As Single = -1)
'
'    On Error GoTo catcherr
'
'    If ((DestStartTimeMS > TotalTime) Or (DestStartTimeMS < 0)) Or ((SourceStartTimeMS > SourceWaveView.WaveLengthMS) Or (SourceStartTimeMS < 0)) Then
'        err.Raise 9, "WaveData", "Invalid or exceeding start time."
'    Else
'        If SourceDurationTimeMS = -1 Then SourceDurationTimeMS = SourceWaveView.WaveLengthMS - SourceStartTimeMS
'        If (DestStartTimeMS + SourceDurationTimeMS <= TotalTime) And (SourceStartTimeMS + SourceDurationTimeMS <= SourceWaveView.WaveLengthMS) Then
'            WaveCombine WaveData, DestStartTimeMS, SourceWaveView.WaveData(SourceStartTimeMS, SourceDurationTimeMS), SourceDurationTimeMS
'
'        Else
'            err.Raise 9, "WaveData", "Invalid or exceeding duration time."
'        End If
'    End If
'
'    Exit Sub
'catcherr:
'    MsgBox err.Description, vbCritical, "An error occured"
'    err.Clear
'End Sub

Public Sub WaveStop()

    On Error GoTo catcherr
    
    If SoundPlay Then
        PlayWaveSound WaveSilentAudio(AudioFile, 0)
        SoundPlay = False
        PlayButton.Enabled = True
        StopButton.Enabled = False
    End If

    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub

Public Sub WavePlay()

    On Error GoTo catcherr
    
    If FileLoaded <> "" And SoundPlay = False Then
        
        Dim i As Single
        Dim lTiming As Single
        Dim startMS As Single
        Dim stopMS As Single
        
        Dim x As Single, y As Single, y1 As Single, h As Single, w As Single
        
        
        Dim CurrentTrack As Integer
        
        LineRestrictions

        SoundPlay = True
        PlayButton.Enabled = False
        StopButton.Enabled = True

        PlayCursor.X1 = LeftMarker.X1 - Screen.TwipsPerPixelX
        PlayCursor.X2 = PlayCursor.X1
        
        startMS = WaveMarkerStartMS
        
        Dim sndBytes() As Byte
        
        sndBytes = WaveAudioPartial(AudioFile, startMS, WaveMarkerStopMS - startMS)
        
        startMS = PlayCursor.X1
        stopMS = RightMarker.X1 + Screen.TwipsPerPixelX
        
        If PlayCursor.X1 + PicDisplay.Left >= 0 And PlayCursor.X1 + PicDisplay.Left < (UserControl.Width - PicVolscale.Width) Then
            If ((LeftMarker.X1 + PicDisplay.Left >= 0) And ((LeftMarker.X1 + PicDisplay.Left) < (UserControl.Width - PicVolscale.Width))) And _
                ((RightMarker.X1 + PicDisplay.Left >= 0) And ((RightMarker.X1 + PicDisplay.Left) < (UserControl.Width - PicVolscale.Width))) Then
                CursorAdjust = 0
            Else
                CursorAdjust = 1
            End If
        Else
            CursorAdjust = 0
        End If
        
        PlayWaveSound sndBytes
        lTiming = Timer
        
        PlayCursor.Visible = True

        
        Do
        
            If (Timer - lTiming) > 0 And TotalTime > 0 Then
                PlayCursor.X1 = startMS + (PicDisplay.Width * (((Timer - lTiming) * 1000) / TotalTime))
            End If
        
            If CursorAdjust = 1 Then
                If PlayCursor.X1 + PicDisplay.Left >= ((UserControl.Width - PicVolscale.Width) / 2) - ((17 * Screen.TwipsPerPixelX) / 2) And _
                    PlayCursor.X1 + PicDisplay.Left < ((UserControl.Width - PicVolscale.Width) / 2) + ((17 * Screen.TwipsPerPixelX) / 2) Then
                    
                    If (PicDisplay.Width + (PicDisplay.Left - (PlayCursor.X1 - PlayCursor.X2))) >= (UserControl.Width - PicVolscale.Width) Then
                    
                        PicDisplay.Left = PicDisplay.Left - (PlayCursor.X1 - PlayCursor.X2)
                        ViewSlider.Left = (PicSlidebar.Width * (-PicDisplay.Left / PicDisplay.Width))
                    End If
                    
                End If
            End If
        
            PlayCursor.X2 = PlayCursor.X1

            h = (((Abs(Peak) + Abs(Dip)) / (Abs(-32768) + Abs(32767))) / PicDisplay.Height)
               
            For CurrentTrack = 1 To AudioFile.Infos(1).wNumberOfChannels

                w = (PicSlidebar.Width / AudioFile.Infos(1).wNumberOfChannels)
                    
                y1 = Abs(AudioFile.Datas(1).wWave(Round((AudioFile.Infos(1).lBytesPerSecond * (WaveMarkerPlayMS / 1000)) / AudioFile.Infos(1).wNumberOfChannels) + (CurrentTrack - 1)))
                
                h = (y1 / (Abs(Peak) + Abs(Dip)))
                
                y1 = (h * (Abs(-32768) + Abs(32767)))

                h = ((-100 - Round(h * 100)) + 200)
                
                If h > 75 Then
                    PicVolscale.Line ((w * (CurrentTrack - 1)), PicDisplay.Height)-((w * CurrentTrack), (PicDisplay.Height * (h / 100))), Blend(vbYellow, vbGreen, h), BF
                ElseIf h > 50 Then
                    PicVolscale.Line ((w * (CurrentTrack - 1)), PicDisplay.Height)-((w * CurrentTrack), (PicDisplay.Height * (h / 100))), Blend(&H80C0FF, vbYellow, h), BF
                Else
                    PicVolscale.Line ((w * (CurrentTrack - 1)), PicDisplay.Height)-((w * CurrentTrack), (PicDisplay.Height * (h / 100))), Blend(vbRed, &H80C0FF, h), BF
                End If
                
                PicVolscale.Line ((w * (CurrentTrack - 1)), (PicDisplay.Height * (h / 100)))-((w * CurrentTrack), 0), vbWhite, BF

                PicVolscale.Line ((w * (CurrentTrack - 1)), (PicDisplay.Height * (h / 100)))-((w * CurrentTrack), (PicDisplay.Height * (h / 100))), vbBlack
                
                PicVolscale.Refresh

            Next

            y1 = 0

            DoEvents
        
            If SoundPlay = False Then Exit Do
        
        Loop Until PlayCursor.X1 > stopMS
        
        RenderVolScale
        
        PlayCursor.Visible = False

        PlayButton.Enabled = True
        StopButton.Enabled = False

        SoundPlay = False

    End If

    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub

Public Sub WaveFromData(ByRef wData() As Byte)

    On Error GoTo catcherr
    
    WaveResetRecord AudioFile
    
    AudioFile = WaveBytesAsAudio(wData)
    
    WaveInitial

    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub


Public Sub WaveFromFile(ByVal FileName As String)

    On Error GoTo catcherr

    If FileLoaded <> "" Then
        WaveResetRecord AudioFile
        FileLoaded = ""
    End If
    
    WaveLoadFromFile FileName, AudioFile
    
    WaveInitial

    FileLoaded = FileName
    
    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub

Private Sub RenderTimeLine()
    If FileLoaded <> "" Then
        Dim i As Single
        Dim x As Single
        
        i = ((PicDisplay.Width / (((AudioFile.Datas(1).lBytes / 2) / AudioFile.Infos(1).wChannelBandwidth) / AudioFile.Infos(1).lSamplesPerSecond)) * 0.1)
        If i > 0 Then
            If PicDisplay.Width / i > Screen.TwipsPerPixelX Then
                For x = 1 To PicDisplay.Width Step i
                    PicDisplay.Line (x, (UserControl.Height - PicSlidebar.Height) - ((UserControl.Height - PicSlidebar.Height) * IIf((x / i) Mod 10 = 0, 0.09, IIf((x / i) Mod 5 = 0, 0.06, 0.03))))-(x, (UserControl.Height - PicSlidebar.Height)), vbBlack
                    UserControl.Line (x, (UserControl.Height - PicSlidebar.Height) - ((UserControl.Height - PicSlidebar.Height) * IIf((x / i) Mod 10 = 0, 0.09, IIf((x / i) Mod 5 = 0, 0.06, 0.03))))-(x, (UserControl.Height - PicSlidebar.Height)), vbBlack
                
                Next
            End If
        End If
    End If
End Sub

Private Sub RenderCenters()
    If FileLoaded <> "" Then
        Dim i As Long
        Dim y As Single
        If (PicDisplay.Width < (UserControl.Width - PicVolscale.Width)) Then
            For i = 1 To AudioFile.Infos(1).wNumberOfChannels
                y = ((UserControl.Height - PicSlidebar.Height) / (AudioFile.Infos(1).wNumberOfChannels + 1) * i) - _
                    (((UserControl.Height - PicSlidebar.Height) / 2) - (PicDisplay.Height / 2))

                UserControl.Line (0, y)-(UserControl.Width - PicVolscale.Width, y), GetColorShade(i, 0)
            Next
        End If
    End If
End Sub
Private Sub RenderStatus(ByVal Msg As String, Optional ByVal NoCls As Boolean = False)
    If Not NoCls Then PicDisplay.Cls
    
    PicDisplay.CurrentY = 0
    PicDisplay.CurrentX = -PicDisplay.Left + (Screen.TwipsPerPixelX * 3)
    PicDisplay.CurrentY = -(((UserControl.Height - PicSlidebar.Height) / 2) - (PicDisplay.Height / 2))
    PicDisplay.ForeColor = vbBlack
    PicDisplay.Print Msg
  '  PicDisplay.Refresh
    
    UserControl.Cls
    UserControl.ForeColor = vbBlack
    UserControl.CurrentX = 0
    UserControl.CurrentY = (Screen.TwipsPerPixelX * 3)
    UserControl.Print Msg
   ' UserControl.Refresh
    
    DoEvents
End Sub
Private Sub RenderVolScale()
    PicVolscale.Cls
    If FileLoaded <> "" Then
        Dim i As Long
        Dim y As Single
        PicVolscale.ForeColor = vbBlack
        For i = 1 To AudioFile.Infos(1).wNumberOfChannels
            y = ((UserControl.Height - PicSlidebar.Height) / (AudioFile.Infos(1).wNumberOfChannels + 1) * i) - _
                (((UserControl.Height - PicSlidebar.Height) / 2) - (PicDisplay.Height / 2))
        
            PicVolscale.Line (0, y)-((PicVolscale.Width / 2), y), vbBlack
            PicVolscale.CurrentX = (PicVolscale.Width - PicVolscale.TextWidth("00"))
            PicVolscale.CurrentY = y - (PicVolscale.TextHeight("0") / 2)
            If (PicDisplay.Width < (UserControl.Width - PicVolscale.Width)) Then
                UserControl.ForeColor = GetColorShade(i, 0)
                UserControl.CurrentX = PicVolscale.CurrentX
                UserControl.CurrentY = PicVolscale.CurrentY
            End If
            PicVolscale.Print "0"
            
        Next
    End If
End Sub

Private Sub RenderFileInfo(ByVal clr As Long)
    If FileLoaded <> "" Then
        PicDisplay.CurrentX = -PicDisplay.Left: PicDisplay.CurrentY = -(((UserControl.Height - PicSlidebar.Height) / 2) - (PicDisplay.Height / 2))
        PicDisplay.ForeColor = clr
        Dim info As String
        info = CStr((TotalTime / 1000))
        If InStr(info, ".") > 0 Then info = Left(info, InStr(info, ".") - 1) & Mid(info, InStr(info, "."), 3)
        
        Select Case AudioFile.Infos(1).wNumberOfChannels
            Case 1
                info = "Standard PCM " & AudioFile.Infos(1).lSamplesPerSecond & " Hertz, " & _
                            AudioFile.Infos(1).wBitsPerSample & "-bits, " & "Mono, " & (AudioFile.Datas(1).lBytes / 2) & " KBytes, " & info & " Seconds"
            Case 2
                info = "Standard PCM " & AudioFile.Infos(1).lSamplesPerSecond & " Hertz, " & _
                            AudioFile.Infos(1).wBitsPerSample & "-bits, " & "Stereo, " & (AudioFile.Datas(1).lBytes / 2) & " KBytes, " & info & " Seconds"
            Case 3
                info = "Standard PCM " & AudioFile.Infos(1).lSamplesPerSecond & " Hertz, " & _
                            AudioFile.Infos(1).wBitsPerSample & "-bits, " & "3 Channels, " & (AudioFile.Datas(1).lBytes / 2) & " KBytes, " & info & " Seconds"
            Case 4
                info = "Standard PCM " & AudioFile.Infos(1).lSamplesPerSecond & " Hertz, " & _
                            AudioFile.Infos(1).wBitsPerSample & "-bits, " & "Quad, " & (AudioFile.Datas(1).lBytes / 2) & " KBytes, " & info & " Seconds"
            Case Is >= 5
                info = "Standard PCM " & AudioFile.Infos(1).lSamplesPerSecond & " Hertz, " & _
                            AudioFile.Infos(1).wBitsPerSample & "-bits, " & AudioFile.Infos(1).wNumberOfChannels & " Channels, " & (AudioFile.Datas(1).lBytes / 2) & " KBytes, " & info & " Seconds"
        End Select
        PicDisplay.Print info
        UserControl.Cls
        UserControl.ForeColor = vbBlack
        UserControl.CurrentX = 0
        UserControl.CurrentY = 0
        UserControl.Print info
    End If
End Sub


Private Sub WaveInitial()

    Dim i As Single
    Dim d As Single

    RenderStatus "Loading wave file..."

    Peak = 0: Dip = 0
    TotalTime = 0
    ChannelTap = 0
                
    If AudioFile.wDataLen > 0 And AudioFile.wInfoLen > 0 Then

        Dim elapse As Single
        elapse = Timer

        For d = 1 To (AudioFile.Datas(1).lBytes / 2)

            If AudioFile.Datas(1).wWave(d) < Dip Then Dip = CSng(AudioFile.Datas(1).wWave(d))
            If AudioFile.Datas(1).wWave(d) > Peak Then Peak = CSng(AudioFile.Datas(1).wWave(d))


            If Timer - elapse > 0.3 Then
                elapse = Timer
                RenderStatus "Loading wave file (" & CInt(((d / (AudioFile.Datas(1).lBytes / 2)) * 100)) & "%)..."
            End If
        Next

        
        If ChannelTap < AudioFile.Infos(1).wNumberOfChannels Then ChannelTap = AudioFile.Infos(1).wNumberOfChannels
        
        TotalTime = WaveMilliseconds(AudioFile)
        
        MRatio = -(PicRender.Height / AudioFile.Infos(1).wNumberOfChannels)
        
        MRatio = MRatio / (Abs(Peak) + Abs(Dip))
        DBZoom = 0
             
        PicRender.Width = ((TotalTime / Screen.TwipsPerPixelY) * (AudioFile.Infos(AudioFile.wInfoLen).wBitsPerSample * AudioFile.Infos(AudioFile.wInfoLen).lSamplesPerSecond))
        
       ' PicRender.Width = ((TotalTime * 1000) * (Screen.TwipsPerPixelX / 100))
       
        PicRender.Height = ((UserControl.Height - PicSlidebar.Height) / ChannelTap)
        PicDisplay.Left = 0
        PicDisplay.Top = 0
        PicDisplay.Width = (UserControl.ScaleWidth - PicVolscale.Width)
        PicDisplay.Height = (UserControl.Height - PicSlidebar.Height)
        
        ViewSlider.Width = PicSlidebar.Width
        
        If Not FileLoaded = "" Then

        
            FileLoaded = ""
        End If

        WaveDisplay ".\"

    End If
    
    PicDisplay.ForeColor = vbWhite
    PicDisplay.CurrentX = 0: PicDisplay.CurrentY = 0

    LeftMarker.X1 = 0
    LeftMarker.X2 = 0
    LeftMarker.Visible = True

    RightMarker.X1 = PicDisplay.Width
    RightMarker.X2 = PicDisplay.Width
    RightMarker.Visible = True


    LaststartMS = WaveMarkerStartMS
    LaststopMS = WaveMarkerStopMS
        
    PicDisplay.ForeColor = vbBlack
End Sub

Public Sub WaveDisplay(Optional ByVal fname As String = "")

    On Error GoTo catcherr
    
    If fname <> "" Or FileLoaded <> "" Then

        Dim CurrentTrack As Integer
        Dim i As Single
        
        Dim x As Single, y As Single
        
        Dim y1 As Single

        Dim clrShade As Long

        dBitsPerTwip = Screen.TwipsPerPixelX / AudioFile.Infos(1).wChannelBandwidth
       ' dBitsPerTwip = AudioFile.Infos(1).wNumberOfChannels * AudioFile.Infos(1).wChannelBandwidth
       ' dBitsPerTwip = ((AudioFile.Datas(1).lBytes / AudioFile.Infos(1).wNumberOfChannels) / PicRender.Width)

        RenderStatus "Rendering wave file..."
                
        PicRender.Cls

        Dim elapse As Single
        elapse = Timer

        For i = 1 To (AudioFile.Datas(1).lBytes / 2) - AudioFile.Infos(1).wNumberOfChannels Step AudioFile.Infos(1).wNumberOfChannels * Format(dBitsPerTwip, "##000")

            If Timer - elapse > 0.3 Then
                elapse = Timer
                RenderStatus "Rendering wave file (" & CInt((i / ((AudioFile.Datas(1).lBytes / 2) - AudioFile.Infos(1).wNumberOfChannels)) * 100) & "%)..."
            End If

            x = x + ((PicRender.Width / (AudioFile.Datas(1).lBytes / AudioFile.Infos(1).wChannelBandwidth)) * Format(dBitsPerTwip, "##000"))
      '      X = X + (PicRender.Width / (AudioFile.Datas(1).lBytes / AudioFile.Infos(1).wChannelBandwidth))
            
            For CurrentTrack = 1 To AudioFile.Infos(1).wNumberOfChannels
'
'                PicRender.ForeColor = GetColorShade(CurrentTrack, clrShade)
'                If CurrentTrack Mod AudioFile.Infos(1).wNumberOfChannels = 0 Then
'                    Select Case clrShade Mod (AudioFile.Infos(1).wChannelBandwidth + CurrentTrack)
'                        Case 0
'                            clrShade = 1
'                        Case 1
'                            clrShade = 2
'                        Case 2
'                            clrShade = 3
'                        Case 3
'                            clrShade = 4
'                        Case 4
'                            clrShade = 5
'                        Case 5
'                            clrShade = 0
'                    End Select
'                End If

                y = (PicRender.Height / (AudioFile.Infos(1).wNumberOfChannels + 1) * CurrentTrack)
                
                y1 = (y + (AudioFile.Datas(1).wWave(i + CurrentTrack) * MRatio))
                
                PicRender.ForeColor = GetColorShade(CurrentTrack, (Abs(y - y1) / Round((PicRender.Height / ((AudioFile.Infos(1).wNumberOfChannels + 1) * 2)) / 6)))

                If Not DotsOnly Then

                    PicRender.Line (x, y)-(x, y1)

                    If i = 1 Then
                        PicRender.Line (0, y)-(PicRender.Width, y)
                        UserControl.Line (0, y)-(PicRender.Width, y), PicRender.ForeColor
                    End If
                Else
                    PicRender.Line (x, y1)-(x, y1), , BF
                    
                    If i = 1 Then
                        PicRender.Line (0, y)-(PicRender.Width, y)
                        UserControl.Line (0, y)-(PicRender.Width, y), PicRender.ForeColor
                    End If

                End If

            Next CurrentTrack

        Next i
        
        LeftMarker.Visible = True
        RightMarker.Visible = True
        LeftMarker.X1 = 0
        LeftMarker.X2 = 0
        RightMarker.X1 = PicDisplay.Width
        RightMarker.X2 = PicDisplay.Width
            
        If FileLoaded = "" And fname <> "" Then
            FileLoaded = fname
        End If

        RenderVolScale
        
        PicDisplay.PaintPicture PicRender.Image, 0, 0, PicDisplay.Width, PicDisplay.Height, 0, 0, PicRender.Width, PicRender.Height

        PicSlidebar.PaintPicture PicRender.Image, 0, 0, PicSlidebar.Width, PicSlidebar.Height, 0, 0, PicRender.Width, PicRender.Height
        
        ViewSlider.Width = (PicSlidebar.Width * ((UserControl.Width - PicVolscale.Width) / PicDisplay.Width))
        ViewSlider.Left = 0
        ViewSlider.Visible = True
    
        RenderTimeLine

        RenderFileInfo vbBlack
        
        ResetButton.Enabled = True
        PlayButton.Enabled = True
        StopButton.Enabled = True
        OpenButton.Enabled = False

    Else
        ResetButton.Enabled = False
        PlayButton.Enabled = False
        StopButton.Enabled = False
        OpenButton.Enabled = True
        LeftMarker.Visible = False
        RightMarker.Visible = False
        PlayCursor.Visible = False
        
        ViewSlider.Visible = False
        PicSlidebar.Cls
        PicVolscale.Cls
        PicDisplay.Cls
        UserControl.Cls
        
        
    End If
    
    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub

Private Sub OpenButton_Click()
    On Error Resume Next
    BrowseForWave.CancelError = True
    BrowseForWave.DefaultExt = "*.wav"
    BrowseForWave.Filter = "Wave Format|*.WAV"
    BrowseForWave.FilterIndex = 0
    BrowseForWave.FileName = FileLoaded
    BrowseForWave.ShowOpen
    
    If err.Number = cdlCancel Then
        err.Clear
    Else
        On Error GoTo 0
        WaveFromFile BrowseForWave.FileName
    End If
    PicDisplay.SetFocus
End Sub

Private Sub PlayButton_Click()
    PicDisplay.SetFocus
    WavePlay

End Sub

Private Sub StopButton_Click()
    PicDisplay.SetFocus
    WaveStop
End Sub

Private Sub ResetButton_Click()
    PicDisplay.SetFocus
    WaveClose
End Sub

Private Sub PicSlidebar_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 1 Then
        PicSlidebar.Tag = x
    Else
        PicSlidebar.Tag = 0
    End If
End Sub

Private Sub PicSlidebar_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    If Button = 1 Then
        If PicSlidebar.Tag <> 0 And ViewSlider.Width < PicSlidebar.Width Then
            PicSlidebar.Tag = Abs(PicSlidebar.Tag)
            MoveViewPort x

            PicSlidebar.Tag = -x

        End If
    Else
        PicSlidebar.Tag = 0
    End If
End Sub

Private Sub PicSlidebar_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    If x = PicSlidebar.Tag And ViewSlider.Width < PicSlidebar.Width Then
        PicSlidebar.Tag = (ViewSlider.Width / 2)
        MoveViewPort (x - ViewSlider.Left)
    Else
        PicSlidebar.Tag = 0
    End If
End Sub

Private Sub PicVolscale_Click()
    PicDisplay.SetFocus
    DotsOnly = Not DotsOnly
    WaveDisplay FileLoaded
End Sub

Private Sub PicDisplay_Click()
    RaiseEvent Click
End Sub

Private Sub PicDisplay_DblClick()
    RightMarker.X1 = PicDisplay.Width - Screen.TwipsPerPixelX
    RightMarker.X2 = PicDisplay.Width - Screen.TwipsPerPixelX
    LeftMarker.X1 = Screen.TwipsPerPixelX
    LeftMarker.X2 = Screen.TwipsPerPixelX
End Sub

Private Sub PicDisplay_KeyPress(KeyAscii As Integer)

    On Error GoTo catcherr
    
    If KeyAscii >= Asc("1") And KeyAscii <= Asc("9") Then

        FilterScripts.Reset
        FilterScripts.AddCode GetSetting(App.Title, "Filters", "VBScript")
        
        WaveFilter AudioFile, WaveMarkerStartMS, WaveMarkerStopMS - WaveMarkerStartMS, FilterScripts, "func" & Chr(KeyAscii) & "(%)"

        WaveDisplay
    End If

    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub

Private Sub PicDisplay_LostFocus()
    LastX = 0
    LastY = 0
End Sub

Private Sub PicDisplay_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)

    Static lX As Single
  
    If ((Button = 1) And (PicDisplay.MousePointer = 0)) Then

        If (lX <> Screen.TwipsPerPixelX) Then
            RightMarker.Tag = LeftMarker.X1
        End If
        LeftMarker.Tag = x

        If LeftMarker.X1 = RightMarker.X1 Then
            RightMarker.Tag = PicDisplay.Width - Screen.TwipsPerPixelX
        ElseIf (RightMarker.X2 = PicDisplay.Width - Screen.TwipsPerPixelX) Then
            LeftMarker.Tag = Screen.TwipsPerPixelX
        End If
        lX = x
        
    ElseIf (Button = 2) Then

    ElseIf (Button = 0) Then
        lX = Screen.TwipsPerPixelX
    End If

End Sub

Private Sub PicDisplay_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    If (x <> LastX And y <> LastY And Button = 1) And (LastBtn = 1 And LastX <> 0 And LastY <> 0) Then

        PicDisplay.OLEDrag

    Else
        If Button = 0 Then
            If PicDisplay.MousePointer <> 0 Then
                PicDisplay.MousePointer = 0
            End If
        End If
    End If
    
    LastX = x
    LastY = y
    LastBtn = Button
    PicDisplay.SetFocus
End Sub

Private Sub PicDisplay_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
    
    If (PicDisplay.MousePointer <> 0) Then
    
        If PicDisplay.MousePointer = 99 Then
            PicDisplay.Drag DragConstants.vbEndDrag
        ElseIf PicDisplay.MousePointer = 12 Then
            PicDisplay.Drag DragConstants.vbCancel
        End If
        LeftMarker.Tag = ""
        RightMarker.Tag = ""
    Else

        If IsNumeric(LeftMarker.Tag) Then
            LeftMarker.X1 = LeftMarker.Tag
            LeftMarker.X2 = LeftMarker.Tag
            LeftMarker.Tag = ""
        End If
        If IsNumeric(RightMarker.Tag) Then
            RightMarker.X1 = RightMarker.Tag
            RightMarker.X2 = RightMarker.Tag
            RightMarker.Tag = ""
        End If
        
    End If

End Sub

Private Sub PicDisplay_OLECompleteDrag(Effect As Long)

    Screen.MousePointer = 0

End Sub

Private Sub PicDisplay_OLEDragDrop(Data As DataObject, Effect As Long, Button As Integer, Shift As Integer, x As Single, y As Single)

    On Error GoTo catcherr
    
    If Data.GetFormat(MyFormat) Then
        If Screen.MousePointer = 99 Then
            
            Dim temp() As Byte
            temp = Data.GetData(MyFormat)

            If Shift Then


            Else
                Dim af As AudioFile
                af = WaveBytesAsAudio(temp)
                If af.wInfoLen > 0 And af.wDataLen > 0 Then
                    Dim SourceDurationTimeMS As Single
                    SourceDurationTimeMS = Round(((af.Datas(1).lBytes / af.Infos(af.wInfoLen).lBytesPerSecond) * 1000))
                    If SourceDurationTimeMS - (WaveMilliseconds(af) - WaveMarkerStartMS) < 0 Then
                        err.Raise 8, "WaveData", "Invalid or exceeding duration time."
                    Else
                        AudioFile = WaveBytesAsAudio(WaveCombine(WaveData, WaveMarkerStartMS, temp, SourceDurationTimeMS))
                        WaveInitial
                        
                    End If
                End If
            End If
            Erase temp

        End If
    End If
    
    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
End Sub

Private Sub DragDetermine()

    Dim pt As POINTAPI
    GetCursorPos pt
    If CStr(WindowFromPoint(pt.x, pt.y)) = CStr(PicDisplay.hwnd) Then
         Screen.MousePointer = 12
    Else
         Set Screen.MouseIcon = DragIcon.Picture
         Screen.MousePointer = 99
    End If
End Sub
Private Sub PicDisplay_OLEGiveFeedback(Effect As Long, DefaultCursors As Boolean)

    Effect = vbDropEffectMove Or vbDropEffectCopy
    DefaultCursors = False

    DragDetermine
End Sub

Private Sub PicDisplay_OLEStartDrag(Data As DataObject, AllowedEffects As Long)
    
    AllowedEffects = vbDropEffectMove Or vbDropEffectCopy
    
    If FileLoaded <> "" Then

        Data.Clear
        Data.SetData WaveData, MyFormat

        PicDisplay.MousePointer = 12

    End If

End Sub

Private Sub Blinkers_Timer()

    LeftMarker.BorderColor = IIf((LeftMarker.BorderColor = &H808080), &H404040, &H808080)
    RightMarker.BorderColor = IIf((RightMarker.BorderColor = &H808080), &H404040, &H808080)

    PlayCursor.BorderColor = IIf((RightMarker.BorderColor = &H0&), &HE0E0E0, &H0&)
   
    If LeftMarker.Visible <> (FileLoaded <> "") Then LeftMarker.Visible = (FileLoaded <> "")
    If RightMarker.Visible <> (FileLoaded <> "") Then RightMarker.Visible = (FileLoaded <> "")

End Sub


Private Sub UserControl_Click()
    PicDisplay.SetFocus
End Sub

Private Sub UserControl_Hide()
    WaveStop
End Sub

Public Sub WaveClose()

    On Error GoTo catcherr
    
    WaveStop

    If FileLoaded <> "" Then
        FileLoaded = ""
    End If

    WaveResetRecord AudioFile

    WaveDisplay
    Exit Sub
catcherr:
    MsgBox err.Description, vbCritical, "An error occured"
    err.Clear
    
End Sub

Private Sub UserControl_Initialize()
    MyFormat = RegisterClipboardFormat("audio/wav")
    
    PicSlidebar.Tag = 0
    WaveDisplay
    UserControl_Resize
End Sub

Public Property Get hwnd() As Long
    hwnd = UserControl.hwnd
End Property

Private Sub UserControl_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
    PicDisplay.SetFocus
End Sub

Private Sub UserControl_Resize()
    If UserControl.Width < ((17 * Screen.TwipsPerPixelX) * 2) Then
        UserControl.Width = ((17 * Screen.TwipsPerPixelX) * 2)
    End If
    If UserControl.Height < ((17 * Screen.TwipsPerPixelY) * 2) Then
        UserControl.Height = ((17 * Screen.TwipsPerPixelY) * 2)
    End If
    
    PicDisplay.Left = 0
    PicDisplay.Top = 0
    PicDisplay.Height = (UserControl.ScaleHeight - PicSlidebar.Height)
    PicDisplay.Width = (UserControl.ScaleWidth - PicVolscale.Width)

    PicSlidebar.Height = (17 * Screen.TwipsPerPixelY)
    PicVolscale.Width = (17 * Screen.TwipsPerPixelX)
    PicSlidebar.Left = 0
    PicSlidebar.Top = (UserControl.Height - PicSlidebar.Height)
    PicSlidebar.Width = (UserControl.Width - (OpenButton.Width + PlayButton.Width + StopButton.Width + ResetButton.Width))
    PicVolscale.Left = (UserControl.Width - PicVolscale.Width)
    PicVolscale.Top = 0
    PicVolscale.Height = (UserControl.Height - OpenButton.Height)
    ViewSlider.Top = 0
    ViewSlider.Height = PicSlidebar.Height
    
    ResetButton.Top = (UserControl.Height - ResetButton.Height)
    ResetButton.Left = (UserControl.Width - ResetButton.Width)


    StopButton.Top = (UserControl.Height - PicSlidebar.Height)
    StopButton.Left = (ResetButton.Left - StopButton.Width)
    
    PlayButton.Top = (UserControl.Height - PicSlidebar.Height)
    PlayButton.Left = (StopButton.Left - PlayButton.Width)
    
    OpenButton.Top = (UserControl.Height - PicSlidebar.Height)
    OpenButton.Left = (PlayButton.Left - OpenButton.Width)

    LeftMarker.y1 = 0
    LeftMarker.y2 = UserControl.Height
    RightMarker.y1 = 0
    RightMarker.y2 = UserControl.Height
    PlayCursor.y1 = 0
    PlayCursor.y2 = UserControl.Height


    If FileLoaded <> "" Then

        PicDisplay.PaintPicture PicRender.Image, 0, 0, PicDisplay.Width, PicDisplay.Height, 0, 0, PicRender.Width, PicRender.Height

        PicSlidebar.PaintPicture PicRender.Image, 0, 0, PicSlidebar.Width, PicSlidebar.Height, 0, 0, PicRender.Width, PicRender.Height

        ViewSlider.Width = (PicSlidebar.Width * ((UserControl.Width - PicVolscale.Width) / PicDisplay.Width))
        
        If PicDisplay.Width < (UserControl.Width - PicVolscale.Width) Then
            PicDisplay.Left = 0
            ViewSlider.Left = 0
        Else
            If PicDisplay.Left < 0 Then
                ViewSlider.Left = (PicSlidebar.Width * (-PicDisplay.Left / PicDisplay.Width))
            End If
            If (PicDisplay.Width + PicDisplay.Left) < (UserControl.Width - PicVolscale.Width) _
                And ((PicDisplay.Left < 0) And (PicDisplay.Width > (UserControl.Width - PicVolscale.Width))) Then
                PicDisplay.Left = -(PicDisplay.Width - (UserControl.Width - PicVolscale.Width))
                ViewSlider.Left = PicSlidebar.Width - ViewSlider.Width
            End If
        End If

        If TotalTime > 0 Then
            LeftMarker.X1 = (PicDisplay.Width * (LaststartMS / TotalTime))
            LeftMarker.X2 = LeftMarker.X1
            RightMarker.X1 = (PicDisplay.Width * (LaststopMS / TotalTime))
            RightMarker.X2 = RightMarker.X1
        End If
        RenderVolScale

        RenderTimeLine
        RenderFileInfo vbBlack
        


        LaststartMS = WaveMarkerStartMS
        LaststopMS = WaveMarkerStopMS
        

    End If
End Sub

Private Sub UserControl_Terminate()
    WaveClose
End Sub

Private Function GetColorShade(ByVal Track As Long, ByVal Shade As Long) As Long
    Select Case Track
        Case 1
            Select Case Shade
            'blue
                Case 0
                    GetColorShade = &HFFC0C0
                Case 1
                    GetColorShade = &HFF8080
                Case 2
                    GetColorShade = &HFF0000
                Case 3
                    GetColorShade = &HC00000
                Case 4
                    GetColorShade = &H800000
                Case 5
                    GetColorShade = &H400000
            End Select
        Case 2
            Select Case Shade
            'red
                Case 0
                    GetColorShade = &HC0C0FF
                Case 1
                    GetColorShade = &H8080FF
                Case 2
                    GetColorShade = &HFF&
                Case 3
                    GetColorShade = &HC0&
                Case 4
                    GetColorShade = &H80&
                Case 5
                    GetColorShade = &H40&
            End Select
        Case 3
            Select Case Shade
            'green
                Case 0
                    GetColorShade = &HC0FFC0
                Case 1
                    GetColorShade = &H80FF80
                Case 2
                    GetColorShade = &HFF00&
                Case 3
                    GetColorShade = &HC000&
                Case 4
                    GetColorShade = &H8000&
                Case 5
                    GetColorShade = &H4000&
            End Select
        Case 4
            Select Case Shade
            'yellow
                Case 0
                    GetColorShade = &HC0FFFF
                Case 1
                    GetColorShade = &H80FFFF
                Case 2
                    GetColorShade = &HFFFF&
                Case 3
                    GetColorShade = &HC0C0&
                Case 4
                    GetColorShade = &H8080&
                Case 5
                    GetColorShade = &H4040&
            End Select
        Case 5
            Select Case Shade
            'teal
                Case 0
                    GetColorShade = &HFFFFC0
                Case 1
                    GetColorShade = &HFFFF80
                Case 2
                    GetColorShade = &HFFFF00
                Case 3
                    GetColorShade = &HC0C000
                Case 4
                    GetColorShade = &H808000
                Case 5
                    GetColorShade = &H404000
            End Select
        Case 6
            Select Case Shade
            'purple
                Case 0
                    GetColorShade = &HFFC0FF
                Case 1
                    GetColorShade = &HFF80FF
                Case 2
                    GetColorShade = &HFF00FF
                Case 3
                    GetColorShade = &HC000C0
                Case 4
                    GetColorShade = &H800080
                Case 5
                    GetColorShade = &H400040
            End Select
        Case 7
            Select Case Shade
            'black
                Case 0
                    GetColorShade = &HFFFFFF
                Case 1
                    GetColorShade = &HE0E0E0
                Case 2
                    GetColorShade = &HC0C0C0
                Case 3
                    GetColorShade = &H808080
                Case 4
                    GetColorShade = &H404040
                Case 5
                    GetColorShade = &H0&
            End Select
        Case 8
            Select Case Shade
            'orange
                Case 0
                    GetColorShade = &HC0E0FF
                Case 1
                    GetColorShade = &H80C0FF
                Case 2
                    GetColorShade = &H80FF&
                Case 3
                    GetColorShade = &H40C0&
                Case 4
                    GetColorShade = &H4080&
                Case 5
                    GetColorShade = &H404080
            End Select
    End Select

End Function
