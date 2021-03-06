﻿Imports System.IO
Imports System.Windows.Forms
Imports System.Drawing
Imports System.Linq

Module ClientDataBase
    Public Function GetFileContents(ByVal FullPath As String, Optional ByRef ErrInfo As String = "") As String
        Dim strContents As String
        Dim objReader As StreamReader
        strContents = ""
        Try
            objReader = New StreamReader(FullPath)
            strContents = objReader.ReadToEnd()
            objReader.Close()
        Catch Ex As Exception
            ErrInfo = Ex.Message
        End Try
        Return strContents
    End Function

#Region "Assets Check"
    Public Sub CheckTilesets()
        Dim i As Integer
        Dim tmp As Bitmap
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "\tilesets\" & i & GFX_EXT)
            NumTileSets = NumTileSets + 1
            i = i + 1
        End While

        ReDim TilesetsClr(0 To NumTileSets)

        For i = 1 To NumTileSets
            tmp = New Bitmap(Application.StartupPath & GFX_PATH & "\tilesets\" & i & GFX_EXT)
            TilesetsClr(NumTileSets) = tmp.GetPixel(0, 0)
        Next
        If NumTileSets = 0 Then Exit Sub

    End Sub

    Public Sub CheckCharacters()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "characters\" & i & GFX_EXT)
            NumCharacters = NumCharacters + 1
            i = i + 1
        End While

        If NumCharacters = 0 Then Exit Sub
    End Sub

    Public Sub CheckPaperdolls()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "paperdolls\" & i & GFX_EXT)
            NumPaperdolls = NumPaperdolls + 1
            i = i + 1
        End While

        If NumPaperdolls = 0 Then Exit Sub
    End Sub

    Public Sub CheckAnimations()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "animations\" & i & GFX_EXT)
            NumAnimations = NumAnimations + 1
            i = i + 1
        End While

        If NumAnimations = 0 Then Exit Sub
    End Sub

    Public Sub CheckItems()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "Items\" & i & GFX_EXT)
            NumItems = NumItems + 1
            i = i + 1
        End While

        If NumItems = 0 Then Exit Sub
    End Sub

    Public Sub CheckResources()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "Resources\" & i & GFX_EXT)
            NumResources = NumResources + 1
            i = i + 1
        End While

        If NumResources = 0 Then Exit Sub
    End Sub

    Public Sub CheckSkillIcons()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "SkillIcons\" & i & GFX_EXT)
            NumSkillIcons = NumSkillIcons + 1
            i = i + 1
        End While

        If NumSkillIcons = 0 Then Exit Sub
    End Sub

    Public Sub CheckFaces()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "Faces\" & i & GFX_EXT)
            NumFaces = NumFaces + 1
            i = i + 1
        End While

        If NumFaces = 0 Then Exit Sub
    End Sub

    Public Sub CheckFog()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "Fogs\" & i & GFX_EXT)
            NumFogs = NumFogs + 1
            i = i + 1
        End While

        If NumFogs = 0 Then Exit Sub
    End Sub

    Public Sub CheckEmotes()
        Dim i As Integer
        i = 1

        While FileExist(Application.StartupPath & GFX_PATH & "Emotes\" & i & GFX_EXT)
            NumEmotes = NumEmotes + 1
            i = i + 1
        End While

        If NumEmotes = 0 Then Exit Sub
    End Sub

    Public Sub CacheMusic()
        Dim Files As String() = Directory.GetFiles(Application.StartupPath & MUSIC_PATH, "*.ogg")
        Dim MaxNum As String = Directory.GetFiles(Application.StartupPath & MUSIC_PATH, "*.ogg").Count
        Dim Counter As Integer = 1

        For Each FileName In Files
            ReDim Preserve MusicCache(Counter)

            MusicCache(Counter) = Path.GetFileName(FileName)
            Counter = Counter + 1
            Application.DoEvents()
        Next

    End Sub

    Public Sub CacheSound()
        Dim Files As String() = Directory.GetFiles(Application.StartupPath & SOUND_PATH, "*.ogg")
        Dim MaxNum As String = Directory.GetFiles(Application.StartupPath & SOUND_PATH, "*.ogg").Count
        Dim Counter As Integer = 1

        For Each FileName In Files
            ReDim Preserve SoundCache(Counter)

            SoundCache(Counter) = Path.GetFileName(FileName)
            Counter = Counter + 1
            Application.DoEvents()
        Next

    End Sub
#End Region

#Region "Options"
    Public Sub SaveOptions()
        Dim FileName As String

        FileName = Application.StartupPath & "\Data Files\config.ini"

        PutVar(FileName, "Options", "Username", Trim$(Options.Username))
        PutVar(FileName, "Options", "Password", Trim$(Options.Password))
        PutVar(FileName, "Options", "SavePass", Str(Options.SavePass))
        PutVar(FileName, "Options", "IP", Options.IP)
        PutVar(FileName, "Options", "Port", Str(Options.Port))
        PutVar(FileName, "Options", "MenuMusic", Trim$(Options.MenuMusic))
        PutVar(FileName, "Options", "Music", Str(Options.Music))
        PutVar(FileName, "Options", "Sound", Str(Options.Sound))
        PutVar(FileName, "Options", "Volume", Str(Options.Volume))
        PutVar(FileName, "Options", "ScreenSize", Str(Options.ScreenSize))
        PutVar(FileName, "Options", "HighEnd", Str(Options.HighEnd))
        PutVar(FileName, "Options", "ShowNpcBar", Str(Options.ShowNpcBar))
    End Sub

    Public Sub LoadOptions()
        Dim FileName As String

        FileName = Application.StartupPath & "\Data Files\config.ini"

        If Not FileExist(FileName) Then
            Options.Password = ""
            Options.SavePass = False
            Options.Username = ""
            Options.IP = "127.0.0.1"
            Options.Port = 7001
            Options.MenuMusic = ""
            Options.Music = 1
            Options.Sound = 1
            Options.Volume = 100
            Options.ScreenSize = 0
            Options.HighEnd = 0
            Options.ShowNpcBar = 0
            SaveOptions()
        Else
            Options.Username = Getvar(FileName, "Options", "Username")
            Options.Password = Getvar(FileName, "Options", "Password")
            Options.SavePass = Getvar(FileName, "Options", "SavePass")
            Options.IP = Getvar(FileName, "Options", "IP")
            Options.Port = Val(Getvar(FileName, "Options", "Port"))
            Options.MenuMusic = Getvar(FileName, "Options", "MenuMusic")
            Options.Music = Getvar(FileName, "Options", "Music")
            Options.Sound = Getvar(FileName, "Options", "Sound")
            If Getvar(FileName, "Options", "Volume") = "" Then
                Options.Volume = 100
                SaveOptions()
            End If
            Options.Volume = Val(Getvar(FileName, "Options", "Volume"))
            Options.ScreenSize = Getvar(FileName, "Options", "ScreenSize")
            Options.HighEnd = Val(Getvar(FileName, "Options", "HighEnd"))
            Options.ShowNpcBar = Val(Getvar(FileName, "Options", "ShowNpcBar"))
        End If

        ' show in GUI
        If Options.Music = 1 Then
            frmOptions.optMOn.Checked = True
        Else
            frmOptions.optMOff.Checked = False
        End If

        If Options.Music = 1 Then
            frmOptions.optSOn.Checked = True
        Else
            frmOptions.optSOff.Checked = False
        End If

        frmOptions.lblVolume.Text = "Volume: " & Options.Volume
        frmOptions.scrlVolume.Value = Options.Volume

        frmOptions.cmbScreenSize.SelectedIndex = Options.ScreenSize

    End Sub
#End Region

#Region "Maps"
    Sub ClearMap()

        SyncLock MapLock
            Map.Name = ""
            Map.tileset = 1
            Map.MaxX = SCREEN_MAPX
            Map.MaxY = SCREEN_MAPY
            Map.BootMap = 0
            Map.BootX = 0
            Map.BootY = 0
            Map.Down = 0
            Map.Left = 0
            Map.Moral = 0
            Map.Music = ""
            Map.Revision = 0
            Map.Right = 0
            Map.Up = 0

            ReDim Map.Npc(0 To MAX_MAP_NPCS)
            ReDim Map.Tile(0 To Map.MaxX, 0 To Map.MaxY)

            For x = 0 To SCREEN_MAPX
                For y = 0 To SCREEN_MAPY
                    ReDim Map.Tile(x, y).Layer(0 To MapLayer.Count - 1)
                    For l = 0 To MapLayer.Count - 1
                        Map.Tile(x, y).Layer(l).Tileset = 0
                        Map.Tile(x, y).Layer(l).X = 0
                        Map.Tile(x, y).Layer(l).Y = 0
                        Map.Tile(x, y).Layer(l).AutoTile = 0
                    Next

                Next
            Next

        End SyncLock

    End Sub

    Sub ClearMapItems()
        Dim i As Integer

        For i = 1 To MAX_MAP_ITEMS
            ClearMapItem(i)
        Next

    End Sub

    Sub ClearMapItem(ByVal Index As Integer)
        MapItem(Index).Frame = 0
        MapItem(Index).Num = 0
        MapItem(Index).Value = 0
        MapItem(Index).X = 0
        MapItem(Index).Y = 0
    End Sub

    Sub ClearMapNpc(ByVal Index As Integer)
        MapNpc(Index).Attacking = 0
        MapNpc(Index).AttackTimer = 0
        MapNpc(Index).Dir = 0
        MapNpc(Index).Map = 0
        MapNpc(Index).Moving = 0
        MapNpc(Index).Num = 0
        MapNpc(Index).Steps = 0
        MapNpc(Index).Target = 0
        MapNpc(Index).TargetType = 0
        MapNpc(Index).Vital(Vitals.HP) = 0
        MapNpc(Index).Vital(Vitals.MP) = 0
        MapNpc(Index).Vital(Vitals.SP) = 0
        MapNpc(Index).X = 0
        MapNpc(Index).XOffset = 0
        MapNpc(Index).Y = 0
        MapNpc(Index).YOffset = 0
    End Sub

    Sub ClearMapNpcs()
        Dim i As Integer

        For i = 1 To MAX_MAP_NPCS
            ClearMapNpc(i)
        Next

    End Sub

    Sub ClearBlood()
        For I = 1 To Byte.MaxValue
            Blood(I).Timer = 0
        Next
    End Sub

#End Region

#Region "Items"
    Public Sub ClearItem(ByVal Index As Integer)
        Index = Index - 1
        Item(Index) = Nothing
        Item(Index) = New ItemRec
        For x = 0 To Stats.Count - 1
            ReDim Item(Index).Add_Stat(x)
        Next
        For x = 0 To Stats.Count - 1
            ReDim Item(Index).Stat_Req(x)
        Next

        ReDim Item(Index).FurnitureBlocks(0 To 3, 0 To 3)
        ReDim Item(Index).FurnitureFringe(0 To 3, 0 To 3)

        Item(Index).Name = ""
    End Sub

    Public Sub ClearChanged_Item()
        For i = 1 To MAX_ITEMS
            Item_Changed(i) = Nothing
        Next i
        ReDim Item_Changed(0 To MAX_ITEMS)
    End Sub

    Sub ClearItems()
        Dim i As Integer

        For i = 1 To MAX_ITEMS
            ClearItem(i)
        Next

    End Sub
#End Region

#Region "Resources"
    Public Sub ClearChanged_Resource()
        For i = 1 To MAX_RESOURCES
            Resource_Changed(i) = Nothing
        Next i
        ReDim Resource_Changed(0 To MAX_RESOURCES)
    End Sub

    Sub ClearResource(ByVal Index As Integer)
        Resource(Index) = Nothing
        Resource(Index) = New ResourceRec
        Resource(Index).Name = ""
    End Sub

    Sub ClearResources()
        Dim i As Integer

        For i = 1 To MAX_RESOURCES
            ClearResource(i)
        Next

    End Sub
#End Region

#Region "Npc's"
    Sub ClearNpcs()
        Dim i As Integer

        For i = 1 To MAX_NPCS
            ClearNpc(i)
        Next

    End Sub

    Sub ClearNpc(ByVal Index As Integer)
        Npc(Index) = Nothing
        Npc(Index) = New NpcRec

        Npc(Index).Name = ""
        Npc(Index).AttackSay = ""
        ReDim Npc(Index).Stat(0 To Stats.Count - 1)
        ReDim Npc(Index).Skill(0 To MAX_NPC_SKILLS)
    End Sub
#End Region

#Region "Animations"
    Sub ClearAnimation(ByVal Index As Integer)
        Animation(Index) = Nothing
        Animation(Index) = New AnimationRec
        For x = 0 To 1
            ReDim Animation(Index).Sprite(x)
        Next
        For x = 0 To 1
            ReDim Animation(Index).Frames(x)
        Next
        For x = 0 To 1
            ReDim Animation(Index).LoopCount(x)
        Next
        For x = 0 To 1
            ReDim Animation(Index).looptime(x)
        Next
        Animation(Index).Name = ""
    End Sub

    Sub ClearAnimations()
        Dim i As Integer

        For i = 1 To MAX_ANIMATIONS
            ClearAnimation(i)
        Next

    End Sub

    Sub ClearAnimInstance(ByVal index As Integer)
        AnimInstance(index).Animation = 0
        AnimInstance(index).X = 0
        AnimInstance(index).Y = 0
        For i = 0 To UBound(AnimInstance(index).Used)
            AnimInstance(index).Used(i) = False
        Next
        For i = 0 To UBound(AnimInstance(index).Timer)
            AnimInstance(index).Timer(i) = False
        Next
        For i = 0 To UBound(AnimInstance(index).FrameIndex)
            AnimInstance(index).FrameIndex(i) = False
        Next

        AnimInstance(index).LockType = 0
        AnimInstance(index).lockindex = 0
    End Sub
#End Region

#Region "Skills"
    Sub ClearSkills()
        Dim i As Integer

        For i = 1 To MAX_SKILLS
            ClearSkill(i)
        Next

    End Sub

    Sub ClearSkill(ByVal Index As Integer)
        Skill(Index) = Nothing
        Skill(Index) = New SkillRec
        Skill(Index).Name = ""
    End Sub
#End Region

#Region "Shops"
    Sub ClearShop(ByVal Index As Integer)
        Shop(Index) = Nothing
        Shop(Index) = New ShopRec
        Shop(Index).Name = ""
        ReDim Shop(Index).TradeItem(MAX_TRADES)
    End Sub

    Sub ClearShops()
        Dim i As Integer

        For i = 1 To MAX_SHOPS
            ClearShop(i)
        Next

    End Sub
#End Region

End Module
