Imports System.IO
Imports System.Linq
Imports System.Windows.Forms
Imports Microsoft.Xna.Framework
Imports Microsoft.Xna.Framework.Graphics
Imports Microsoft.Xna.Framework.Input
Imports MonoGame.Extended

Public Class Window : Inherits Game

    ' MonoGame Rendering Variables.
    Private Device As GraphicsDeviceManager
    Private View As SpriteBatch

    ' Resolution variables
    Private Fullscreen As Boolean
    Private ResolutionX As Integer
    Private ResolutionY As Integer

    ' Logic Variables.
    Private HasBeenResized As Boolean
    Private Viewport As Camera2D
    Private ViewPortX As List(Of Integer)
    Private ViewPortY As List(Of Integer)
    Private FrameRate As Integer

    ' Location Variables
    Private AppLocation As String

    ' Textures
    Private TexTextCache As Dictionary(Of String, TextCacheRec)
    Private TexAnimations() As TextureRec
    Private TexCharacters() As TextureRec
    Private TexTilesets() As TextureRec
    Private TexEmotes() As TextureRec
    Private TexFaces() As TextureRec
    Private TexFog() As TextureRec
    Private TexFurniture() As TextureRec
    Private TexItems() As TextureRec
    Private TexPaperdolls() As TextureRec
    Private TexProjectiles() As TextureRec
    Private TexResources() As TextureRec
    Private TexSkillicons() As TextureRec
    Private TexMisc As Dictionary(Of String, TextureRec)

    ' Fonts
    Private GameFonts As Dictionary(Of Integer, SpriteFont)


    Public Sub New(ByVal ResX As Integer, ByVal ResY As Integer, ByVal IsFullscreen As Boolean)
        ' Create a brand new graphics device.
        Device = New GraphicsDeviceManager(Me)

        ' Set our resolution variables.
        Fullscreen = IsFullscreen
        ResolutionX = ResX
        ResolutionY = ResY

    End Sub

    Protected Overrides Sub Initialize()
        ' Set our window size according to settings.
        Device.PreferredBackBufferWidth = ResolutionX
        Device.PreferredBackBufferHeight = ResolutionY
        Device.IsFullScreen = Fullscreen

        ' Stop limiting us to 60fps!
        ' Device.SynchronizeWithVerticalRetrace = False
        ' IsFixedTimeStep = False

        ' Apply our settings.
        Device.ApplyChanges()

        ' Allow the mouse to be visible.
        IsMouseVisible = True

        ' Allow our window to be resized.
        Window.AllowUserResizing = True
        AddHandler Window.ClientSizeChanged, AddressOf HandleClientSizeChanged

        ' Set our application location.
        AppLocation = Path.GetDirectoryName(Application.ExecutablePath)

        ' Set our content location.
        Content.RootDirectory = Path.Combine(AppLocation, DIR_ROOT)

        ' Initialize all our rendering arrays and variables.
        Viewport = New Camera2D(Device.GraphicsDevice)
        ViewPortX = New List(Of Integer)()
        ViewPortY = New List(Of Integer)()
        InitTextures()

        MyBase.Initialize()     ' Do not touch
    End Sub

    Protected Overrides Sub LoadContent()
        ' Create our MonoGame objects.
        View = New SpriteBatch(Device.GraphicsDevice)

        ' Load all our font sizes.
        LoadFonts()

    End Sub

    Protected Overrides Sub UnloadContent()
        ' Destroy all our textures.
        DestroyTextures()

        ' Destroy our devices.
        View.Dispose()
        Device.Dispose()

        MyBase.UnloadContent()      ' Do not touch!
    End Sub

    Protected Overrides Sub Update(Time As GameTime)

        ' Update our camera position.
        UpdateCamera(Time)

        ' Unload all our unused textures.
        UnloadTextures()

        ' Keyboard input
        HandleKeyboard()

        ' If we have to, resize our backbuffer.
        If HasBeenResized Then
            Device.PreferredBackBufferWidth = Window.ClientBounds.Width
            Device.PreferredBackBufferHeight = Window.ClientBounds.Height
            Device.ApplyChanges()
            HasBeenResized = False
        End If

        ' Cache all the text we'll need to render soon.
        CacheText()

        ' Update Framerate
        FrameRate = CType(1 / Time.ElapsedGameTime.TotalSeconds, Integer)

        MyBase.Update(Time) '   Do not touch
    End Sub

    Protected Overrides Sub Draw(Time As GameTime)
        ' Clear our screen and give it a lovely black background colour then start rendering new stuff!
        GraphicsDevice.Clear(Color.Black)
        View.Begin(transformMatrix:=Viewport.GetViewMatrix(), blendState:=BlendState.NonPremultiplied, samplerState:=SamplerState.PointClamp)

        ' TODO: Render Graphics
        If Not GettingMap Then

            ' Draw our bottom layers.
            For Layer = MapLayer.Ground To MapLayer.Mask2
                DrawMapLayer(Layer)
            Next

            ' Furniture
            If FurnitureHouse > 0 AndAlso FurnitureHouse = Player(MyIndex).InHouse AndAlso FurnitureCount > 0 Then
                For I = 1 To FurnitureCount
                    If Furniture(I).ItemNum > 0 Then DrawFurniture(I, 0)
                Next
            End If

            ' events
            If Map.CurrentEvents > 0 And Map.CurrentEvents <= Map.EventCount Then
                For I = 1 To Map.CurrentEvents
                    If Map.MapEvents(I).Position = 0 Then DrawEvent(I)
                Next
            End If

            'blood
            For I = 1 To Byte.MaxValue
                DrawBlood(I)
            Next

            ' Items
            For I = 1 To MAX_MAP_ITEMS
                If MapItem(I).Num > 0 Then DrawItem(I)
            Next

            For X = 1 To Map.MaxX
                For Y = 1 To Map.MaxY
                    If Map.Tile(X, Y).Type = TileType.Door Then DrawDoor(X, Y)
                Next
            Next

            ' Draw our animations that go below our characters.
            For I = 1 To Byte.MaxValue
                If AnimInstance(I).Used(0) Then DrawAnimation(I, 0)
            Next

            ' Y based rendering, so things overlap accordingly.
            For Y = 0 To Map.MaxY

                ' Players
                For i = 1 To MAX_PLAYERS
                    If IsPlaying(i) And GetPlayerMap(i) = GetPlayerMap(MyIndex) AndAlso Player(i).Y = Y Then
                        DrawPlayer(i)
                        ' TODO: Draw emotes and paperdoll
                    End If
                    If PetAlive(i) AndAlso Player(i).Pet.Y = Y Then
                        DrawPet(i)
                    End If
                Next

                ' Npcs
                For i = 1 To MAX_MAP_NPCS
                    If MapNpc(i).Num > 0 AndAlso MapNpc(i).Vital(Vitals.HP) > 0 AndAlso MapNpc(i).Y = Y Then
                        DrawMapNpc(i)
                        ' TODO: Draw emotes
                    End If
                Next

                ' events
                If Map.CurrentEvents > 0 AndAlso Map.CurrentEvents <= Map.EventCount Then
                    For I = 1 To Map.CurrentEvents
                        If Map.MapEvents(I).Y = Y AndAlso Map.MapEvents(I).Position = 1 Then DrawEvent(I)
                    Next
                End If

            Next

            ' events
            If Map.CurrentEvents > 0 AndAlso Map.CurrentEvents <= Map.EventCount Then
                For I = 1 To Map.CurrentEvents
                    If Map.MapEvents(I).Position = 2 Then DrawEvent(I)
                Next
            End If

            ' Draw our top layers.
            For Layer = MapLayer.Fringe To MapLayer.Fringe2
                DrawMapLayer(Layer)
            Next

            ' Furniture
            If FurnitureHouse > 0 AndAlso FurnitureHouse = Player(MyIndex).InHouse AndAlso FurnitureCount > 0 Then
                For I = 1 To FurnitureCount
                    If Furniture(I).ItemNum > 0 Then DrawFurniture(I, 1)
                Next
            End If

            ' Draw names
            For i = 1 To MAX_PLAYERS
                If IsPlaying(i) And GetPlayerMap(i) = GetPlayerMap(MyIndex) Then
                    DrawPlayerName(i, 10)
                End If
            Next
            For i = 1 To MAX_MAP_NPCS
                If MapNpc(i).Num > 0 And MapNpc(i).Vital(Vitals.HP) > 0 Then
                    DrawMapNpcName(i, 10)
                End If
            Next

        End If

        ' Draw debug info
        DrawText(String.Format("Framerate: {0}", FrameRate), 10, New Vector2(5, 5), Color.Yellow, Color.Black, ToScreen:=True)
        DrawText(String.Format("Camera X: {0} Y: {1}", Viewport.Position.X, Viewport.Position.Y), 10, New Vector2(5, 20), Color.Yellow, Color.Black, ToScreen:=True)

        ' Draw everything to the screen. Do not put anything beyond this point.
        View.End()

        MyBase.Draw(Time)   ' Do not touch.
    End Sub

#Region "Init Data"
    Private Sub InitTextures()
        Dim Dir = Path.Combine(AppLocation, "Data Files", "Graphics")
        InitGraphics(Path.Combine(Dir, "Tilesets"), TexTilesets)
        InitGraphics(Path.Combine(Dir, "Characters"), TexCharacters)
        InitGraphics(Path.Combine(Dir, "Animations"), TexAnimations)
        InitGraphics(Path.Combine(Dir, "Emotes"), TexEmotes)
        InitGraphics(Path.Combine(Dir, "Faces"), TexFaces)
        InitGraphics(Path.Combine(Dir, "Fog"), TexFog)
        InitGraphics(Path.Combine(Dir, "Furniture"), TexFurniture)
        InitGraphics(Path.Combine(Dir, "Items"), TexItems)
        InitGraphics(Path.Combine(Dir, "Paperdolls"), TexPaperdolls)
        InitGraphics(Path.Combine(Dir, "Projectiles"), TexProjectiles)
        InitGraphics(Path.Combine(Dir, "Resources"), TexResources)
        InitGraphics(Path.Combine(Dir, "Skillicons"), TexSkillicons)

        ' All miscellanious files.
        InitMiscGraphics()

        ' Our text rendering cache.
        TexTextCache = New Dictionary(Of String, TextCacheRec)()
    End Sub

    Private Sub InitGraphics(ByVal Dir As String, ByRef Array() As TextureRec)
        Dim IsLooking As Boolean = True
        Dim Files As New List(Of String)
        Dim Id = 1

        ' Look for our files.
        While IsLooking
            Dim Fname = Path.Combine(Dir, String.Format("{0}{1}", Id, GFX_EXT))
            If (File.Exists(Fname)) Then
                Files.Add(Fname)
                Id += 1
            Else
                IsLooking = False
            End If
        End While

        ' Redim our array and add filenames to it.
        ReDim Array(0 To Files.Count)
        For i = 1 To Files.Count
            Array(i) = New TextureRec()
            Array(i).FileName = Files(i - 1)
            Array(i).LastAccess = DateTime.MinValue
        Next
    End Sub
    Private Sub InitMiscGraphics()
        TexMisc = New Dictionary(Of String, TextureRec)()
        Dim Dir = New DirectoryInfo(Path.Combine(AppLocation, DIR_ROOT, DIR_GRAPHICS))
        For Each File In Dir.EnumerateFiles(String.Format("*{0}", GFX_EXT), SearchOption.TopDirectoryOnly)
            InitMisc(Path.GetFileNameWithoutExtension(File.Name))
        Next
    End Sub
    Private Sub InitMisc(ByVal File As String)
        Dim t = New TextureRec()
        t.FileName = Path.Combine(AppLocation, DIR_ROOT, DIR_GRAPHICS, String.Format("{0}{1}", File, GFX_EXT))
        t.LastAccess = DateTime.MinValue
        TexMisc.Add(File, t)
    End Sub
#End Region

#Region "Loading Data"
    Private Sub LoadTexture(ByVal Texture As TextureRec)
        ' No point loading an existing texture.
        If (Texture.Texture Is Nothing) Then
            Using fs As New FileStream(Texture.FileName, FileMode.Open)
                Texture.Texture = Texture2D.FromStream(Device.GraphicsDevice, fs)
            End Using
        End If

        ' It's been accessed, so let's set this straight.
        Texture.LastAccess = DateTime.Now
    End Sub

    Private Sub LoadFonts()
        ' Create a brand new instance of our fonts dictionary.
        GameFonts = New Dictionary(Of Integer, SpriteFont)()

        ' Load all our fonts.
        LoadFont(8)
        LoadFont(10)
        LoadFont(12)
        LoadFont(14)
        LoadFont(16)
        LoadFont(18)
        LoadFont(20)
        LoadFont(22)
        LoadFont(24)
        LoadFont(26)
    End Sub
    Private Sub LoadFont(ByVal Size As Integer)
        GameFonts.Add(Size, Content.Load(Of SpriteFont)(Path.Combine(DIR_GRAPHICS, DIR_FONTS, Size)))
    End Sub
#End Region

#Region "Unloading Data"
    Private Sub UnloadTextures()
        ' Unload Graphics
        UnloadGraphics(TexTilesets)
        UnloadGraphics(TexCharacters)
        UnloadGraphics(TexAnimations)
        UnloadGraphics(TexEmotes)
        UnloadGraphics(TexFaces)
        UnloadGraphics(TexFog)
        UnloadGraphics(TexFurniture)
        UnloadGraphics(TexItems)
        UnloadGraphics(TexPaperdolls)
        UnloadGraphics(TexProjectiles)
        UnloadGraphics(TexResources)
        UnloadGraphics(TexSkillicons)
        UnloadMiscGraphics()

        ' Unload our text cache.
        UnloadTextCache()
    End Sub

    Private Sub UnloadGraphics(ByRef Array() As TextureRec)
        For Each T In Array
            If Not T Is Nothing AndAlso Not T.Texture Is Nothing AndAlso T.LastAccess > DateTime.MinValue AndAlso DateTime.Now.Subtract(T.LastAccess).Minutes > 5 Then
                T.Texture = Nothing
                T.LastAccess = DateTime.MinValue
            End If
        Next
    End Sub
    Private Sub UnloadMiscGraphics()
        For Each T In TexMisc
            If Not T.Value Is Nothing AndAlso Not T.Value.Texture Is Nothing AndAlso T.Value.LastAccess > DateTime.MinValue AndAlso DateTime.Now.Subtract(T.Value.LastAccess).Minutes > 5 Then
                T.Value.Texture = Nothing
                T.Value.LastAccess = DateTime.MinValue
            End If
        Next
    End Sub
    Private Sub UnloadTextCache()
        Dim OldItems = New List(Of String)
        For Each T In TexTextCache
            If Not T.Value Is Nothing AndAlso Not T.Value.Texture Is Nothing AndAlso T.Value.LastAccess > DateTime.MinValue AndAlso DateTime.Now.Subtract(T.Value.LastAccess).Minutes > 5 Then
                T.Value.Texture = Nothing
                T.Value.LastAccess = DateTime.MinValue
                OldItems.Add(T.Key)
            End If
        Next
        For Each I In OldItems
            TexTextCache.Remove(I)
        Next
    End Sub
#End Region

#Region "Destroy Data"
    Private Sub DestroyTextures()
        For Each x In TexTilesets
            DestroyTexture(x)
        Next
        For Each x In TexCharacters
            DestroyTexture(x)
        Next
        For Each x In TexAnimations
            DestroyTexture(x)
        Next
        For Each x In TexEmotes
            DestroyTexture(x)
        Next
        For Each x In TexFaces
            DestroyTexture(x)
        Next
        For Each x In TexFog
            DestroyTexture(x)
        Next
        For Each x In TexFurniture
            DestroyTexture(x)
        Next
        For Each x In TexItems
            DestroyTexture(x)
        Next
        For Each x In TexPaperdolls
            DestroyTexture(x)
        Next
        For Each x In TexProjectiles
            DestroyTexture(x)
        Next
        For Each x In TexResources
            DestroyTexture(x)
        Next
        For Each x In TexSkillicons
            DestroyTexture(x)
        Next
        For Each x In TexMisc
            DestroyTexture(x.Value)
        Next
    End Sub
    Private Sub DestroyTexture(ByRef Texture As TextureRec)
        If Not Texture Is Nothing AndAlso Not Texture.Texture Is Nothing Then
            Texture.Texture.Dispose()
            Texture = Nothing
        End If
    End Sub
#End Region

#Region "Render Data"
    Private Sub DrawMapLayer(ByVal Layer As Integer)
        If GettingMap Then Exit Sub
        If Map.Tile Is Nothing Then Exit Sub

        For X = 0 To Map.MaxX
            For Y = 0 To Map.MaxY
                DrawMapTile(Layer, X, Y)
            Next
        Next
    End Sub
    Private Sub DrawMapTile(ByVal Layer As Integer, ByVal X As Integer, ByVal Y As Integer)
        With Map.Tile(X, Y).Layer(Layer)
            Select Case Autotile(X, Y).Layer(Layer).renderState
                Case RENDER_STATE_NORMAL
                    RenderTexture(TexTilesets(Map.Tile(X, Y).Layer(Layer).Tileset), New Vector2(X * PIC_X, Y * PIC_Y), New Rectangle(Map.Tile(X, Y).Layer(Layer).X * PIC_X, Map.Tile(X, Y).Layer(Layer).Y * PIC_Y, PIC_X, PIC_Y))
                Case RENDER_STATE_AUTOTILE
                    DrawAutoTile(Layer, X * PIC_X, Y * PIC_Y, 1, X, Y, 0, False)
                    DrawAutoTile(Layer, X * PIC_X + 16, Y * PIC_Y, 2, X, Y, 0, False)
                    DrawAutoTile(Layer, X * PIC_X, Y * PIC_Y + 16, 3, X, Y, 0, False)
                    DrawAutoTile(Layer, X * PIC_X + 16, Y * PIC_Y + 16, 4, X, Y, 0, False)
            End Select
        End With
    End Sub
    Public Sub DrawAutoTile(ByVal layerNum As Integer, ByVal destX As Integer, ByVal destY As Integer, ByVal quarterNum As Integer, ByVal X As Integer, ByVal Y As Integer, Optional forceFrame As Integer = 0, Optional strict As Boolean = True)
        If Map.Tile(X, Y).Layer Is Nothing Or Map.Tile(X, Y).Layer(layerNum).AutoTile = Nothing Then Exit Sub
        ' calculate the offset
        If forceFrame > 0 Then
            Select Case forceFrame - 1
                Case 0
                    WaterfallFrame = 1
                Case 1
                    WaterfallFrame = 2
                Case 2
                    WaterfallFrame = 0
            End Select
            ' animate autotiles
            Select Case forceFrame - 1
                Case 0
                    AutoTileFrame = 1
                Case 1
                    AutoTileFrame = 2
                Case 2
                    AutoTileFrame = 0
            End Select
        End If

        Dim YOffset = 0
        Dim XOffset = 0
        Select Case Map.Tile(X, Y).Layer(layerNum).AutoTile
            Case AUTOTILE_WATERFALL
                YOffset = (WaterfallFrame - 1) * 32
            Case AUTOTILE_ANIM
                XOffset = AutoTileFrame * 64
            Case AUTOTILE_CLIFF
                YOffset = -32
        End Select

        ' Draw the quarter
        RenderTexture(TexTilesets(Map.Tile(X, Y).Layer(layerNum).Tileset), New Vector2(destX, destY), New Rectangle(Autotile(X, Y).Layer(layerNum).srcX(quarterNum) + XOffset, Autotile(X, Y).Layer(layerNum).srcY(quarterNum) + YOffset, 16, 16))

    End Sub
    Public Sub DrawFurniture(ByVal Index As Integer, Layer As Integer)
        Dim i As Integer, ItemNum As Integer
        Dim X As Integer, Y As Integer, Width As Integer, Height As Integer, X1 As Integer, Y1 As Integer

        ItemNum = Furniture(Index).ItemNum

        If Item(ItemNum).Type <> ItemType.Furniture Then Exit Sub

        i = Item(ItemNum).Data2

        LoadTexture(TexFurniture(i))

        Width = Item(ItemNum).FurnitureWidth
        Height = Item(ItemNum).FurnitureHeight

        If Width > 4 Then Width = 4
        If Height > 4 Then Height = 4
        If i <= 0 Or i > TexFurniture.Length Then Exit Sub

        ' make sure it's not out of map
        If Furniture(Index).X > Map.MaxX Then Exit Sub
        If Furniture(Index).Y > Map.MaxY Then Exit Sub

        For X1 = 0 To Width - 1
            For Y1 = 0 To Height
                If Item(Furniture(Index).ItemNum).FurnitureFringe(X1, Y1) = Layer Then
                    ' Set base x + y, then the offset due to size
                    X = (Furniture(Index).X * 32) + (X1 * 32)
                    Y = (Furniture(Index).Y * 32 - (Height * 32)) + (Y1 * 32)
                    RenderTexture(TexFurniture(i), New Vector2(X, Y), New Rectangle(0 + (X1 * 32), 0 + (Y1 * 32), 32, 32))
                End If
            Next
        Next

    End Sub
    Public Sub DrawEvent(id As Integer)
        Dim X As Integer, Y As Integer, Width As Integer, Height As Integer, sRect As Rectangle, Anim As Integer, spritetop As Integer

        If Map.MapEvents(id).Visible = 0 Then Exit Sub
        If InMapEditor Then Exit Sub
        Select Case Map.MapEvents(id).GraphicType
            Case 0
                Exit Sub
            Case 1
                If Map.MapEvents(id).GraphicNum <= 0 Or Map.MapEvents(id).GraphicNum > TexCharacters.Length Then Exit Sub

                ' Reset frame
                If Map.MapEvents(id).Steps = 3 Then
                    Anim = 0
                ElseIf Map.MapEvents(id).Steps = 1 Then
                    Anim = 2
                End If

                Select Case Map.MapEvents(id).dir
                    Case Direction.Up
                        If (Map.MapEvents(id).YOffset > 8) Then Anim = Map.MapEvents(id).Steps
                    Case Direction.Down
                        If (Map.MapEvents(id).YOffset < -8) Then Anim = Map.MapEvents(id).Steps
                    Case Direction.Left
                        If (Map.MapEvents(id).XOffset > 8) Then Anim = Map.MapEvents(id).Steps
                    Case Direction.Right
                        If (Map.MapEvents(id).XOffset < -8) Then Anim = Map.MapEvents(id).Steps
                End Select

                ' Set the left
                Select Case Map.MapEvents(id).ShowDir
                    Case Direction.Up
                        spritetop = 3
                    Case Direction.Right
                        spritetop = 2
                    Case Direction.Down
                        spritetop = 0
                    Case Direction.Left
                        spritetop = 1
                End Select

                If Map.MapEvents(id).WalkAnim = 1 Then Anim = 0
                If Map.MapEvents(id).Moving = 0 Then Anim = Map.MapEvents(id).GraphicX

                ' Make sure our texture is loaded.
                Dim Tex = TexCharacters(Map.MapEvents(id).GraphicNum)
                LoadTexture(Tex)

                Width = TexCharacters(id).Texture.Width / 4
                Height = TexCharacters(id).Texture.Height / 4

                sRect = New Rectangle((Anim) * (Tex.Texture.Width / 4), spritetop * (Tex.Texture.Height / 4), (Tex.Texture.Width / 4), (Tex.Texture.Height / 4))
                ' Calculate the X
                X = Map.MapEvents(id).X * PIC_X + Map.MapEvents(id).XOffset - ((Tex.Texture.Width / 4 - 32) / 2)

                ' Is the player's height more than 32..?
                If (Tex.Texture.Height * 4) > 32 Then
                    ' Create a 32 pixel offset for larger sprites
                    Y = Map.MapEvents(id).Y * PIC_Y + Map.MapEvents(id).YOffset - ((Tex.Texture.Height / 4) - 32)
                Else
                    ' Proceed as normal
                    Y = Map.MapEvents(id).Y * PIC_Y + Map.MapEvents(id).YOffset
                End If
                ' render the actual sprite
                RenderTexture(Tex, New Vector2(X, Y), sRect)
            Case 2
                If Map.MapEvents(id).GraphicNum < 1 Or Map.MapEvents(id).GraphicNum > TexTilesets.Length Then Exit Sub
                If Map.MapEvents(id).GraphicY2 > 0 Or Map.MapEvents(id).GraphicX2 > 0 Then
                    With sRect
                        .X = Map.MapEvents(id).GraphicX * 32
                        .Y = Map.MapEvents(id).GraphicY * 32
                        .Width = Map.MapEvents(id).GraphicX2 * 32
                        .Height = Map.MapEvents(id).GraphicY2 * 32
                    End With
                Else
                    With sRect
                        .X = Map.MapEvents(id).GraphicY * 32
                        .Height = .Top + 32
                        .Y = Map.MapEvents(id).GraphicX * 32
                        .Width = .Left + 32
                    End With
                End If
                X = Map.MapEvents(id).X * 32
                Y = Map.MapEvents(id).Y * 32
                X = X - ((sRect.Right - sRect.Left) / 2)
                Y = Y - (sRect.Bottom - sRect.Top) + 32
                If Map.MapEvents(id).GraphicY2 > 0 Then
                    RenderTexture(TexTilesets(Map.MapEvents(id).GraphicNum), New Vector2(Map.MapEvents(id).X * 32, Map.MapEvents(id).Y * 32 - Map.MapEvents(id).GraphicY2 * 32 + 32), sRect)
                Else
                    RenderTexture(TexTilesets(Map.MapEvents(id).GraphicNum), New Vector2(Map.MapEvents(id).X * 32, Map.MapEvents(id).Y * 32), sRect)
                End If
        End Select

    End Sub
    Public Sub DrawBlood(ByVal Index As Integer)
        Dim srcrec As Rectangle
        Dim destrec As Rectangle

        With Blood(Index)
            ' check if we should be seeing it
            If .Timer + 20000 < GetTickCount() Then Exit Sub
            srcrec = New Rectangle((.Sprite - 1) * PIC_X, 0, PIC_X, PIC_Y)
            destrec = New Rectangle(.X * PIC_X, .Y * PIC_Y, PIC_X, PIC_Y)
            RenderTexture(TexMisc("Blood"), New Vector2(destrec.X, destrec.Y), srcrec)
        End With

    End Sub
    Public Sub DrawDoor(ByVal X As Integer, ByVal Y As Integer)
        Dim rec As Rectangle

        Dim x2 As Integer, y2 As Integer

        ' sort out animation
        With TempTile(X, Y)
            If .DoorAnimate = 1 Then ' opening
                If .DoorTimer + 100 < GetTickCount() Then
                    If .DoorFrame < 4 Then
                        .DoorFrame = .DoorFrame + 1
                    Else
                        .DoorAnimate = 2 ' set to closing
                    End If
                    .DoorTimer = GetTickCount()
                End If
            ElseIf .DoorAnimate = 2 Then ' closing
                If .DoorTimer + 100 < GetTickCount() Then
                    If .DoorFrame > 1 Then
                        .DoorFrame = .DoorFrame - 1
                    Else
                        .DoorAnimate = 0 ' end animation
                    End If
                    .DoorTimer = GetTickCount()
                End If
            End If

            If .DoorFrame = 0 Then .DoorFrame = 1
        End With

        ' Make sure our graphic loaded.
        LoadTexture(TexMisc("Door"))

        With rec
            .Y = 0
            .Height = TexMisc("Door").Texture.Height
            .X = ((TempTile(X, Y).DoorFrame - 1) * TexMisc("Door").Texture.Width / 4)
            .Width = DoorGFXInfo.Width / 4
        End With

        x2 = (X * PIC_X)
        y2 = (Y * PIC_Y) - (TexMisc("Door").Texture.Height / 2) + 4

        RenderTexture(TexMisc("Door"), New Vector2(X * PIC_X, Y * PIC_Y), rec)
    End Sub
    Public Sub DrawItem(ByVal itemnum As Integer)
        Dim srcrec As Rectangle
        Dim PicNum As Integer
        Dim x As Integer, y As Integer
        PicNum = Item(MapItem(itemnum).Num).Pic

        If PicNum < 1 Or PicNum > TexItems.Length Then Exit Sub

        ' Make sure the texture is loaded.
        LoadTexture(TexItems(PicNum))

        If TexItems(PicNum).Texture.Width > 32 Then ' has more than 1 frame
            srcrec = New Rectangle((MapItem(itemnum).Frame * 32), 0, 32, 32)
        Else
            srcrec = New Rectangle(0, 0, PIC_X, PIC_Y)
        End If

        x = MapItem(itemnum).X * PIC_X
        y = MapItem(itemnum).Y * PIC_Y

        RenderTexture(TexItems(PicNum), New Vector2(x, y), srcrec)
    End Sub
    Private Sub DrawAnimation(ByVal Index As Integer, ByVal Layer As Integer)
        ' Clear our animation if we've nothing left to render.
        If AnimInstance(Index).Animation = 0 Then
            ClearAnimInstance(Index)
            Exit Sub
        End If

        ' Set some things up.
        Dim Tex = Animation(AnimInstance(Index).Animation).Sprite(Layer)
        Dim FrameCount = Animation(AnimInstance(Index).Animation).Frames(Layer)

        ' Can we actually render this?
        If Tex < 1 Or Tex > TexAnimations.Length Then Exit Sub
        If FrameCount <= 0 Then Exit Sub
        If TexAnimations(Tex).Texture Is Nothing Then LoadTexture(TexAnimations(Tex))

        ' Get our source frame.
        Dim Width = TexAnimations(Tex).Texture.Width
        Dim Height = TexAnimations(Tex).Texture.Height
        Dim Source = New Rectangle((AnimInstance(Index).FrameIndex(Layer) - 1) * (Width / FrameCount), 0, Width / FrameCount, Height)

        ' Lock our animation to a target if we have to.
        Dim X As Integer
        Dim Y As Integer
        Dim LockIndex = AnimInstance(Index).lockindex
        Select Case AnimInstance(Index).LockType
            Case TargetType.None
                X = (AnimInstance(Index).X * 32) + 16 - (Width / 2)
                Y = (AnimInstance(Index).Y * 32) + 16 - (Height / 2)

            Case TargetType.Npc
                ' Make sure it's a valid Npc.
                If MapNpc(LockIndex).Num > 0 AndAlso MapNpc(LockIndex).Vital(Vitals.HP) > 0 Then
                    X = (MapNpc(LockIndex).X * PIC_X) + 16 - (Width / 2) + MapNpc(LockIndex).XOffset
                    Y = (MapNpc(LockIndex).Y * PIC_Y) + 16 - (Height / 2) + MapNpc(LockIndex).YOffset
                Else
                    ' This animation is not valid. Clear it.
                    ClearAnimInstance(Index)
                    Exit Sub
                End If

            Case TargetType.Player
                ' Is this player still on our map?
                If GetPlayerMap(LockIndex) = GetPlayerMap(MyIndex) Then
                    X = (GetPlayerX(LockIndex) * PIC_X) + 16 - (Width / 2) + Player(LockIndex).XOffset
                    Y = (GetPlayerY(LockIndex) * PIC_Y) + 16 - (Height / 2) + Player(LockIndex).YOffset
                End If

            Case TargetType.Pet
                ' Is this player still on our map?
                If GetPlayerMap(LockIndex) = GetPlayerMap(MyIndex) Then
                    X = (Player(LockIndex).Pet.X * PIC_X) + 16 - (Width / 2) + Player(LockIndex).Pet.XOffset
                    Y = (Player(LockIndex).Pet.Y * PIC_Y) + 16 - (Height / 2) + Player(LockIndex).Pet.YOffset
                End If

            Case Else
                Throw New NotImplementedException()
        End Select

        RenderTexture(TexAnimations(Tex), New Vector2(X, Y), Source)
    End Sub
    Private Sub DrawPlayer(ByVal Index As Integer)
        ' Make sure our sprite is valid.
        Dim Spritenum = GetPlayerSprite(Index)
        If Spritenum < 1 Or Spritenum > TexCharacters.Length Then Exit Sub

        ' Make sure our sprite exists.
        If TexCharacters(Spritenum).Texture Is Nothing Then LoadTexture(TexCharacters(Spritenum))

        ' Get wich frame we have to use.
        Dim Frame = 0
        Dim FrameRow As Integer
        Dim AttackSpeed = 1000
        If GetPlayerEquipment(Index, EquipmentType.Weapon) > 0 Then AttackSpeed = Item(GetPlayerEquipment(Index, EquipmentType.Weapon)).Speed
        If Player(Index).AttackTimer + (AttackSpeed / 2) > GetTickCount() Then If Player(Index).Attacking = 1 Then Frame = 3
        Select Case GetPlayerDir(Index)
            Case Direction.Up
                If Frame = 0 AndAlso Player(Index).YOffset > 8 Then Frame = Player(Index).Steps
                FrameRow = 3
            Case Direction.Down
                If Frame = 0 AndAlso Player(Index).YOffset < -8 Then Frame = Player(Index).Steps
                FrameRow = 0
            Case Direction.Left
                If Frame = 0 AndAlso Player(Index).XOffset > 8 Then Frame = Player(Index).Steps
                FrameRow = 1
            Case Direction.Right
                If Frame = 0 AndAlso Player(Index).XOffset < -8 Then Frame = Player(Index).Steps
                FrameRow = 2
        End Select
        Dim Source = New Rectangle((Frame) * (TexCharacters(Spritenum).Texture.Width / 4), FrameRow * (TexCharacters(Spritenum).Texture.Height / 4), (TexCharacters(Spritenum).Texture.Width / 4), (TexCharacters(Spritenum).Texture.Height / 4))

        Dim X = GetPlayerX(Index) * PIC_X + Player(Index).XOffset - ((TexCharacters(Spritenum).Texture.Width / 4 - 32) / 2)
        Dim Y As Integer
        If TexCharacters(Spritenum).Texture.Height > 32 Then
            Y = GetPlayerY(Index) * PIC_Y + Player(Index).YOffset - ((TexCharacters(Spritenum).Texture.Height / 4) - 32)
        Else
            Y = GetPlayerY(Index) * PIC_Y + Player(Index).YOffset
        End If

        ' render the actual sprite
        RenderTexture(TexCharacters(Spritenum), New Vector2(X, Y), Source)
    End Sub
    Public Sub DrawPet(ByVal Index As Integer)
        Dim Anim As Byte, X As Integer, Y As Integer
        Dim Sprite As Integer, spriteleft As Integer
        Dim srcrec As Rectangle
        Dim attackspeed As Integer

        Sprite = Pet(Player(Index).Pet.Num).Sprite

        If Sprite < 1 Or Sprite > TexCharacters.Length Then Exit Sub

        attackspeed = 1000

        ' Reset frame
        If Player(Index).Pet.Steps = 3 Then
            Anim = 0
        ElseIf Player(Index).Pet.Steps = 1 Then
            Anim = 2
        ElseIf Player(Index).Pet.Steps = 2 Then
            Anim = 3
        End If

        ' Check for attacking animation
        If Player(Index).Pet.AttackTimer + (attackspeed / 2) > GetTickCount() Then
            If Player(Index).Pet.Attacking = 1 Then
                Anim = 3
            End If
        Else
            ' If not attacking, walk normally
            Select Case Player(Index).Pet.dir
                Case Direction.Up
                    If (Player(Index).Pet.YOffset > 8) Then Anim = Player(Index).Pet.Steps
                Case Direction.Down
                    If (Player(Index).Pet.YOffset < -8) Then Anim = Player(Index).Pet.Steps
                Case Direction.Left
                    If (Player(Index).Pet.XOffset > 8) Then Anim = Player(Index).Pet.Steps
                Case Direction.Right
                    If (Player(Index).Pet.XOffset < -8) Then Anim = Player(Index).Pet.Steps
            End Select
        End If

        ' Check to see if we want to stop making him attack
        With Player(Index).Pet
            If .AttackTimer + attackspeed < GetTickCount() Then
                .Attacking = 0
                .AttackTimer = 0
            End If
        End With

        ' Set the left
        Select Case Player(Index).Pet.dir
            Case Direction.Up
                spriteleft = 3
            Case Direction.Right
                spriteleft = 2
            Case Direction.Down
                spriteleft = 0
            Case Direction.Left
                spriteleft = 1
        End Select

        ' Make sure that our sprite is loaded.
        LoadTexture(TexCharacters(Sprite))

        srcrec = New Rectangle((Anim) * (TexCharacters(Sprite).Texture.Width / 4), spriteleft * (TexCharacters(Sprite).Texture.Height / 4), (TexCharacters(Sprite).Texture.Width / 4), (TexCharacters(Sprite).Texture.Height / 4))

        ' Calculate the X
        X = Player(Index).Pet.X * PIC_X + Player(Index).Pet.XOffset - (TexCharacters(Sprite).Texture.Width / 4 - 32) / 2

        ' Is the player's height more than 32..?
        If (TexCharacters(Sprite).Texture.Height / 4) > 32 Then
            ' Create a 32 pixel offset for larger sprites
            Y = Player(Index).Pet.Y * PIC_Y + Player(Index).Pet.YOffset - (TexCharacters(Sprite).Texture.Width / 4) - 32
        Else
            ' Proceed as normal
            Y = Player(Index).Pet.Y * PIC_Y + Player(Index).Pet.YOffset
        End If

        ' render the actual sprite
        RenderTexture(TexCharacters(Sprite), New Vector2(X, Y), srcrec)

    End Sub
    Private Sub DrawMapNpc(ByVal MapNpcNum As Integer)
        Dim anim As Byte
        Dim X As Integer
        Dim Y As Integer
        Dim Sprite As Integer, spriteleft As Integer
        Dim destrec As Rectangle
        Dim srcrec As Rectangle
        Dim attackspeed As Integer

        If MapNpc(MapNpcNum).Num = 0 Then Exit Sub ' no npc set

        Sprite = Npc(MapNpc(MapNpcNum).Num).Sprite

        If Sprite < 1 Or Sprite > TexCharacters.Length Then Exit Sub

        attackspeed = 1000

        ' Reset frame
        anim = 0

        ' Check for attacking animation
        If MapNpc(MapNpcNum).AttackTimer + (attackspeed / 2) > GetTickCount() Then
            If MapNpc(MapNpcNum).Attacking = 1 Then
                anim = 3
            End If
        Else
            ' If not attacking, walk normally
            Select Case MapNpc(MapNpcNum).Dir
                Case Direction.Up
                    If (MapNpc(MapNpcNum).YOffset > 8) Then anim = MapNpc(MapNpcNum).Steps
                Case Direction.Down
                    If (MapNpc(MapNpcNum).YOffset < -8) Then anim = MapNpc(MapNpcNum).Steps
                Case Direction.Left
                    If (MapNpc(MapNpcNum).XOffset > 8) Then anim = MapNpc(MapNpcNum).Steps
                Case Direction.Right
                    If (MapNpc(MapNpcNum).XOffset < -8) Then anim = MapNpc(MapNpcNum).Steps
            End Select
        End If

        ' Check to see if we want to stop making him attack
        With MapNpc(MapNpcNum)
            If .AttackTimer + attackspeed < GetTickCount() Then
                .Attacking = 0
                .AttackTimer = 0
            End If
        End With

        ' Set the left
        Select Case MapNpc(MapNpcNum).Dir
            Case Direction.Up
                spriteleft = 3
            Case Direction.Right
                spriteleft = 2
            Case Direction.Down
                spriteleft = 0
            Case Direction.Left
                spriteleft = 1
        End Select

        ' Make sure our texture is loaded.
        LoadTexture(TexCharacters(Sprite))

        srcrec = New Rectangle((anim) * (TexCharacters(Sprite).Texture.Width / 4), spriteleft * (TexCharacters(Sprite).Texture.Height / 4), (TexCharacters(Sprite).Texture.Width / 4), (TexCharacters(Sprite).Texture.Height / 4))

        ' Calculate the X
        X = MapNpc(MapNpcNum).X * PIC_X + MapNpc(MapNpcNum).XOffset - ((TexCharacters(Sprite).Texture.Width / 4 - 32) / 2)

        ' Is the player's height more than 32..?
        If (TexCharacters(Sprite).Texture.Height / 4) > 32 Then
            ' Create a 32 pixel offset for larger sprites
            Y = MapNpc(MapNpcNum).Y * PIC_Y + MapNpc(MapNpcNum).YOffset - ((TexCharacters(Sprite).Texture.Height / 4) - 32)
        Else
            ' Proceed as normal
            Y = MapNpc(MapNpcNum).Y * PIC_Y + MapNpc(MapNpcNum).YOffset
        End If

        destrec = New Rectangle(X, Y, TexCharacters(Sprite).Texture.Width / 4, TexCharacters(Sprite).Texture.Height / 4)

        RenderTexture(TexCharacters(Sprite), New Vector2(X, Y), srcrec)
    End Sub

    Private Sub DrawPlayerName(ByVal Index As Integer, ByVal Size As Integer)
        Dim TextX As Integer
        Dim TextY As Integer
        Dim Name As String

        ' Get player name color.
        Dim Color As Color
        Dim BackColor As Color
        Select Case GetPlayerAccess(Index)
            Case 0
                Color = Color.Orange
                BackColor = Color.Black
            Case 1
                Color = Color.Black
                BackColor = Color.White
            Case 2
                Color = Color.Cyan
                BackColor = Color.Black
            Case 3
                Color = Color.Green
                BackColor = Color.Black
            Case 4
                Color = Color.Yellow
                BackColor = Color.Black
        End Select
        If GetPlayerPK(Index) = 2 Then Color = Color.Red
        ' Calculate where to put the player name.
        Name = GetPlayerName(Index).Trim()
        TextX = GetPlayerX(Index) * PIC_X + Player(Index).XOffset + (PIC_X \ 2)
        TextX = TextX - (GameFonts(Size).MeasureString(Name).X / 2)
        If GetPlayerSprite(Index) < 1 Or GetPlayerSprite(Index) > TexCharacters.Length Then
            TextY = (GetPlayerY(Index) * PIC_Y) + Player(Index).YOffset - 16
        Else
            LoadTexture(TexCharacters(GetPlayerSprite(Index)))
            TextY = (GetPlayerY(Index) * PIC_Y) + Player(Index).YOffset - TexCharacters(GetPlayerSprite(Index)).Texture.Height / 4 + 16
        End If

        ' Draw name
        Call DrawText(Name, Size, New Vector2(TextX, TextY), Color, BackColor, CacheText:=True)
    End Sub
    Private Sub DrawMapNpcName(ByVal Index As Integer, ByVal Size As Integer)
        Dim TextX As Integer
        Dim TextY As Integer
        Dim color As Color, backcolor As Color
        Dim npcNum As Integer

        npcNum = MapNpc(Index).Num

        Select Case Npc(npcNum).Behaviour
            Case 0 ' attack on sight
                color = Color.Red
                backcolor = Color.Black
            Case 1, 4 ' attack when attacked + guard
                color = Color.Green
                backcolor = Color.Black
            Case 2, 3, 5 ' friendly + shopkeeper + quest
                color = Color.Yellow
                backcolor = Color.Black
        End Select

        ' Load our texture.
        LoadTexture(TexCharacters(Npc(npcNum).Sprite))

        TextX = MapNpc(Index).X * PIC_X + MapNpc(Index).XOffset + (PIC_X \ 2) - GameFonts(Size).MeasureString(Npc(npcNum).Name.Trim()).X / 2
        If Npc(npcNum).Sprite < 1 Or Npc(npcNum).Sprite > TexCharacters.Length Then
            TextY = MapNpc(Index).Y * PIC_Y + MapNpc(Index).YOffset - 16
        Else
            TextY = MapNpc(Index).Y * PIC_Y + MapNpc(Index).YOffset - (TexCharacters(Npc(npcNum).Sprite).Texture.Height / 4) + 16
        End If

        ' Draw name
        DrawText(Npc(npcNum).Name.Trim(), Size, New Vector2(TextX, TextY), color, backcolor, CacheText:=True)
    End Sub

    Private Sub RenderTexture(ByVal Texture As TextureRec, ByVal Destination As Vector2, Source As Rectangle)
        RenderTexture(Texture, Destination, Source, New Color(255, 255, 255, 255))
    End Sub
    Private Sub RenderTexture(ByVal Texture As TextureRec, ByVal Destination As Vector2, Source As Rectangle, ByVal ColorMask As Color)
        ' First make sure our texture exists.
        If Texture Is Nothing Then Exit Sub
        LoadTexture(Texture)

        ' Draw to screen
        View.Draw(Texture.Texture, Destination, Source, ColorMask)
    End Sub

    Private Sub DrawText(ByVal Text As String, ByVal Size As Integer, ByVal Location As Vector2, ByVal ForeColor As Color, ByVal BackColor As Color, Optional ByVal CacheText As Boolean = False, Optional ByVal ToScreen As Boolean = False)
        If ToScreen Then Location = Viewport.ScreenToWorld(Location)

        If Not CacheText Then
            ' Draw our background text.
            View.DrawString(GameFonts(Size), Text, New Vector2(Location.X - 1, Location.Y - 1), BackColor)
            View.DrawString(GameFonts(Size), Text, New Vector2(Location.X - 1, Location.Y + 1), BackColor)
            View.DrawString(GameFonts(Size), Text, New Vector2(Location.X + 1, Location.Y - 1), BackColor)
            View.DrawString(GameFonts(Size), Text, New Vector2(Location.X + 1, Location.Y + 1), BackColor)

            ' Draw our foreground text.
            View.DrawString(GameFonts(Size), Text, Location, ForeColor)
        Else
            Dim Key = String.Format("{0}-{1}-{2}", Text, ForeColor.ToString(), BackColor.ToString())
            ' Check if our cache has this bit of text available, if not create it.
            If TexTextCache.ContainsKey(Key) AndAlso Not TexTextCache(Key).Texture Is Nothing Then
                ' Adjust for our render texture.
                Location.X -= 1
                Location.Y -= 1
                View.Draw(TexTextCache(Key).Texture, Location, Color.White)
                TexTextCache(Key).LastAccess = DateTime.Now
            ElseIf Not TexTextCache.ContainsKey(Key) Then
                ' Add an empty item to our cache so we can create it later.
                ' We can't render to a new target WHILE drawing, so we do it before we start drawing next frame.
                Dim Temp = New TextCacheRec()
                Temp.Text = Text
                Temp.Size = Size
                Temp.BackColor = BackColor
                Temp.ForeColor = ForeColor
                Temp.LastAccess = DateTime.MinValue
                TexTextCache.Add(Key, Temp)
            End If
        End If
    End Sub
    Private Sub CacheText()
        ' Get all our items we need to cache and go through each of them.
        Dim EmptyItems = TexTextCache.Where(Function(x) x.Value.Texture Is Nothing).Select(Function(x) x.Key)
        For Each Key In EmptyItems
            Dim Dimensions = GameFonts(TexTextCache(Key).Size).MeasureString(TexTextCache(Key).Text)
            Dim Tex = New RenderTarget2D(GraphicsDevice, Dimensions.X + 2, Dimensions.Y + 2)
            Dim Location = New Vector2(1, 1)

            GraphicsDevice.SetRenderTarget(Tex)
            GraphicsDevice.Clear(Color.Transparent)
            View.Begin(SpriteSortMode.Texture, BlendState.AlphaBlend, SamplerState.PointClamp, DepthStencilState.DepthRead, RasterizerState.CullNone)

            ' Draw our background text.
            With TexTextCache(Key)
                View.DrawString(GameFonts(.Size), .Text, New Vector2(Location.X - 1, Location.Y - 1), .BackColor)
                View.DrawString(GameFonts(.Size), .Text, New Vector2(Location.X - 1, Location.Y + 1), .BackColor)
                View.DrawString(GameFonts(.Size), .Text, New Vector2(Location.X + 1, Location.Y - 1), .BackColor)
                View.DrawString(GameFonts(.Size), .Text, New Vector2(Location.X + 1, Location.Y + 1), .BackColor)

                ' Draw our foreground text.
                View.DrawString(GameFonts(.Size), .Text, Location, .ForeColor)
            End With

            View.End()
            TexTextCache(Key).Texture = Tex
            GraphicsDevice.SetRenderTarget(Nothing)
        Next
    End Sub
#End Region

#Region "Logic Updates"
    Private Sub HandleClientSizeChanged(sender As Object, e As EventArgs)
        ' Notify our Update method that the game window has changed size.
        HasBeenResized = True
    End Sub
    Private Sub UpdateCamera(ByVal Time As GameTime)
        Dim CenterX As Integer
        Dim CenterY As Integer

        If Device.PreferredBackBufferWidth > Map.MaxX * PIC_X Then
            CenterX = (Map.MaxX * PIC_X) / 2 + 16
        Else
            CenterX = Player(MyIndex).X * PIC_X + Player(MyIndex).XOffset
        End If

        If Device.PreferredBackBufferHeight > Map.MaxY * PIC_Y Then
            CenterX = (Map.MaxY * PIC_Y) / 2 + 16
        Else
            CenterY = Player(MyIndex).Y * PIC_Y + Player(MyIndex).YOffset
        End If

        ' Smooth Camera 
        CameraAddValues(CenterX, CenterY, Time)
        If ViewPortX.Count() > 1 AndAlso ViewPortY.Count() > 1 Then Viewport.LookAt(New Vector2(ViewPortX.Average(), ViewPortY.Average()))
        ' Do not ever allow the camera to sit on float values, it will go funky real quick!
        Viewport.Position = New Vector2(CType(Viewport.Position.X, Integer), CType(Viewport.Position.Y, Integer))
    End Sub
    Private Sub CameraAddValues(ByVal X As Integer, ByVal Y As Integer, ByVal Time As GameTime)
        If Time.ElapsedGameTime.TotalSeconds = 0 Then Exit Sub
        Dim MaxEntries = FrameRate / 5
        ViewPortX.Add(X)
        If ViewPortX.Count() > MaxEntries Then ViewPortX.Remove(ViewPortX.First())
        ViewPortY.Add(Y)
        If ViewPortY.Count() > MaxEntries Then ViewPortY.Remove(ViewPortY.First())
    End Sub
#End Region

#Region "Game Input"
    Private Sub HandleKeyboard()
        Dim KeyState = Keyboard.GetState()
        ' W
        If KeyState.IsKeyDown(Input.Keys.W) And VbKeyUp = False Then VbKeyUp = True
        If KeyState.IsKeyUp(Input.Keys.W) And VbKeyUp = True Then VbKeyUp = False
        ' A
        If KeyState.IsKeyDown(Input.Keys.A) And VbKeyLeft = False Then VbKeyLeft = True
        If KeyState.IsKeyUp(Input.Keys.A) And VbKeyLeft = True Then VbKeyLeft = False
        ' S
        If KeyState.IsKeyDown(Input.Keys.S) And VbKeyDown = False Then VbKeyDown = True
        If KeyState.IsKeyUp(Input.Keys.S) And VbKeyDown = True Then VbKeyDown = False
        ' D
        If KeyState.IsKeyDown(Input.Keys.D) And VbKeyRight = False Then VbKeyRight = True
        If KeyState.IsKeyUp(Input.Keys.D) And VbKeyRight = True Then VbKeyRight = False
        ' Shift
        If KeyState.IsKeyDown(Input.Keys.LeftShift) And VbKeyShift = False Then VbKeyShift = True
        If KeyState.IsKeyUp(Input.Keys.LeftShift) And VbKeyShift = True Then VbKeyShift = False
        ' Control
        If KeyState.IsKeyDown(Input.Keys.LeftControl) And VbKeyControl = False Then VbKeyControl = True
        If KeyState.IsKeyUp(Input.Keys.LeftControl) And VbKeyControl = True Then VbKeyControl = False
        ' Alt
        If KeyState.IsKeyDown(Input.Keys.LeftAlt) And VbKeyAlt = False Then VbKeyAlt = True
        If KeyState.IsKeyUp(Input.Keys.LeftAlt) And VbKeyAlt = True Then VbKeyAlt = False
    End Sub
#End Region

End Class
