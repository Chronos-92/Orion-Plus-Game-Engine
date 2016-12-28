Imports System.IO
Imports System.Windows.Forms
Imports Microsoft.Xna.Framework
Imports Microsoft.Xna.Framework.Graphics
Imports Microsoft.Xna.Framework.Input

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
    Private RenderOffset As Vector2

    ' Location Variables
    Private AppLocation As String

    ' Textures
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
        Device.ApplyChanges()

        ' Be cheeky and disable MonoGame from slowing down when it loses focus.
        InactiveSleepTime = New TimeSpan(0)

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
        RenderOffset = New Vector2()
        InitTextures()

        MyBase.Initialize()     ' Do not touch
    End Sub

    Protected Overrides Sub LoadContent()
        ' Create our MonoGame objects.
        View = New SpriteBatch(GraphicsDevice)

        ' Load all our font sizes.
        LoadFonts()

    End Sub

    Protected Overrides Sub UnloadContent()
        ' TODO: Unload all our textures since we don't use Content.Load

        MyBase.UnloadContent()      ' Do not touch!
    End Sub

    Protected Overrides Sub Update(Time As GameTime)

        ' Update our camera position.
        UpdateCamera()

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

        MyBase.Update(Time) '   Do not touch
    End Sub

    Protected Overrides Sub Draw(Time As GameTime)
        ' Clear our screen and give it a lovely black background colour then start rendering new stuff!
        GraphicsDevice.Clear(Color.Black)
        View.Begin()

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
            Next

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

        ' Draw everything to the screen. Do not put anything beyond this point.
        View.End()

        MyBase.Draw(Time)   ' Do not touch.
    End Sub

    Private Sub HandleClientSizeChanged(sender As Object, e As EventArgs)
        ' Notify our Update method that the game window has changed size.
        HasBeenResized = True
    End Sub

#Region "Init Data"
    Private Sub InitTextures()
        Dim Dir = Path.Combine(AppLocation, "Data Files", "Graphics")
        ' InitTilesets(Dir)
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
    End Sub

    Private Sub UnloadGraphics(ByRef Array() As TextureRec)
        For Each T In Array
            If Not T Is Nothing AndAlso Not T.Texture Is Nothing AndAlso T.LastAccess > DateTime.MinValue AndAlso DateTime.Now.Subtract(T.LastAccess).Minutes > 5 Then
                T.Texture = Nothing
                T.LastAccess = DateTime.MinValue
            End If
        Next
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
                    RenderTexture(TexTilesets(Map.Tile(X, Y).Layer(Layer).Tileset), New Vector2(ConvertMapX(X * PIC_X), ConvertMapY(Y * PIC_Y)), New Rectangle(Map.Tile(X, Y).Layer(Layer).X * PIC_X, Map.Tile(X, Y).Layer(Layer).Y * PIC_Y, PIC_X, PIC_Y))
                Case RENDER_STATE_AUTOTILE
                    DrawAutoTile(Layer, ConvertMapX(X * PIC_X), ConvertMapY(Y * PIC_Y), 1, X, Y, 0, False)
                    DrawAutoTile(Layer, ConvertMapX(X * PIC_X) + 16, ConvertMapY(Y * PIC_Y), 2, X, Y, 0, False)
                    DrawAutoTile(Layer, ConvertMapX(X * PIC_X), ConvertMapY(Y * PIC_Y) + 16, 3, X, Y, 0, False)
                    DrawAutoTile(Layer, ConvertMapX(X * PIC_X) + 16, ConvertMapY(Y * PIC_Y) + 16, 4, X, Y, 0, False)
            End Select
        End With
    End Sub
    Public Sub DrawAutoTile(ByVal layerNum As Integer, ByVal destX As Integer, ByVal destY As Integer, ByVal quarterNum As Integer, ByVal X As Integer, ByVal Y As Integer, Optional forceFrame As Integer = 0, Optional strict As Boolean = True)
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
                    X = ConvertMapX(X)
                    Y = ConvertMapY(Y)

                    RenderTexture(TexFurniture(i), New Vector2(X, Y), New Rectangle(0 + (X1 * 32), 0 + (Y1 * 32), 32, 32))
                End If
            Next
        Next

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

        RenderTexture(TexAnimations(Tex), New Vector2(ConvertMapX(X), ConvertMapY(Y)), Source)
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
        RenderTexture(TexCharacters(Spritenum), New Vector2(ConvertMapX(X), ConvertMapY(Y)), Source)
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
        RenderTexture(TexCharacters(Sprite), New Vector2(ConvertMapX(X), ConvertMapY(Y)), srcrec)

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

        RenderTexture(TexCharacters(Sprite), New Vector2(ConvertMapX(X), ConvertMapY(Y)), srcrec)
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
            TextY = (GetPlayerY(Index) * PIC_Y) + Player(Index).YOffset - (CharacterGFXInfo(GetPlayerSprite(Index)).Height / 4) + 16
        End If

        ' Draw name
        Call DrawText(Name, Size, New Vector2(ConvertMapX(TextX), ConvertMapY(TextY)), Color, BackColor)
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

        TextX = ConvertMapX(MapNpc(Index).X * PIC_X) + MapNpc(Index).XOffset + (PIC_X \ 2) - getTextWidth((Trim$(Npc(npcNum).Name))) / 2
        If Npc(npcNum).Sprite < 1 Or Npc(npcNum).Sprite > TexCharacters.Length Then
            TextY = ConvertMapY(MapNpc(Index).Y * PIC_Y) + MapNpc(Index).YOffset - 16
        Else
            TextY = ConvertMapY(MapNpc(Index).Y * PIC_Y) + MapNpc(Index).YOffset - (CharacterGFXInfo(Npc(npcNum).Sprite).Height / 4) + 16
        End If

        ' Draw name
        DrawText(Npc(npcNum).Name.Trim(), Size, New Vector2(TextX, TextY), color, backcolor)
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

    Private Sub DrawText(ByVal Text As String, ByVal Size As Integer, ByVal Location As Vector2, ByVal ForeColor As Color, ByVal BackColor As Color)
        ' Draw our background text.
        View.DrawString(GameFonts(Size), Text, New Vector2(Location.X - 1, Location.Y - 1), BackColor)
        View.DrawString(GameFonts(Size), Text, New Vector2(Location.X - 1, Location.Y + 1), BackColor)
        View.DrawString(GameFonts(Size), Text, New Vector2(Location.X + 1, Location.Y - 1), BackColor)
        View.DrawString(GameFonts(Size), Text, New Vector2(Location.X + 1, Location.Y + 1), BackColor)

        ' Draw our foreground text.
        View.DrawString(GameFonts(Size), Text, Location, ForeColor)
    End Sub
#End Region

#Region "Logic Updates"

    Private Sub UpdateCamera()
        If Device.PreferredBackBufferWidth > Map.MaxX * PIC_X Then
            RenderOffset.X = (Device.PreferredBackBufferWidth - (Map.MaxX * PIC_X)) / 2 - 16
        Else
            RenderOffset.X = (Device.PreferredBackBufferWidth / 2) - ((Player(MyIndex).X * PIC_X) + Player(MyIndex).XOffset)
        End If

        If Device.PreferredBackBufferHeight > Map.MaxY * PIC_Y Then
            RenderOffset.Y = (Device.PreferredBackBufferHeight - (Map.MaxY * PIC_Y)) / 2
        Else
            RenderOffset.Y = (Device.PreferredBackBufferHeight / 2) - ((Player(MyIndex).Y * PIC_Y) + Player(MyIndex).YOffset)
        End If
    End Sub
    Private Function ConvertMapX(ByVal X As Integer) As Integer
        ConvertMapX = X + RenderOffset.X
    End Function
    Private Function ConvertMapY(ByVal Y As Integer) As Integer
        ConvertMapY = Y + RenderOffset.Y
    End Function
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
