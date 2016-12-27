Imports System.IO
Imports System.Windows.Forms
Imports Microsoft.Xna.Framework
Imports Microsoft.Xna.Framework.Content
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
    Private Animations() As TextureRec
    Private Characters() As TextureRec
    Private Tilesets() As TextureRec
    Private Emotes() As TextureRec
    Private Faces() As TextureRec
    Private Fog() As TextureRec
    Private Furniture() As TextureRec
    Private Items() As TextureRec
    Private Paperdolls() As TextureRec
    Private Projectiles() As TextureRec
    Private Resources() As TextureRec
    Private Skillicons() As TextureRec

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

        ' If we have to, resize our backbuffer.
        If HasBeenResized Then
            Device.PreferredBackBufferWidth = Window.ClientBounds.Width
            Device.PreferredBackBufferHeight = Window.ClientBounds.Height
            Device.ApplyChanges()
            HasBeenResized = False
        End If
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

            ' Draw our animations that go below players.
            For I = 1 To Byte.MaxValue
                If AnimInstance(I).Used(0) Then
                    DrawAnimation(I, 0)
                End If
            Next

            ' Y based rendering, so things overlap accordingly.
            For Y = 0 To Map.MaxY

                ' Players
                For i = 1 To MAX_PLAYERS
                    If IsPlaying(i) And GetPlayerMap(i) = GetPlayerMap(MyIndex) Then
                        If Player(i).Y = Y Then
                            DrawPlayer(i)
                        End If
                        If PetAlive(i) Then
                            If Player(i).Pet.Y = Y Then
                                ' DrawPet(I)
                            End If
                        End If
                    End If
                Next

            Next

            ' Draw our top layers.
            For Layer = MapLayer.Fringe To MapLayer.Fringe2
                DrawMapLayer(Layer)
            Next

            For i = 1 To MAX_PLAYERS
                If IsPlaying(i) And GetPlayerMap(i) = GetPlayerMap(MyIndex) Then
                    DrawPlayerName(i, 10)
                End If
            Next

        End If

        ' Draw everything to the screen. Do not put anything beyond this point.
        View.End()
    End Sub

    Private Sub HandleClientSizeChanged(sender As Object, e As EventArgs)
        ' Notify our Update method that the game window has changed size.
        HasBeenResized = True
    End Sub

#Region "Init Data"
    Private Sub InitTextures()
        Dim Dir = Path.Combine(AppLocation, "Data Files", "Graphics")
        ' InitTilesets(Dir)
        InitGraphics(Path.Combine(Dir, "Tilesets"), Tilesets)
        InitGraphics(Path.Combine(Dir, "Characters"), Characters)
        InitGraphics(Path.Combine(Dir, "Animations"), Animations)
        InitGraphics(Path.Combine(Dir, "Emotes"), Emotes)
        InitGraphics(Path.Combine(Dir, "Faces"), Faces)
        InitGraphics(Path.Combine(Dir, "Fog"), Fog)
        InitGraphics(Path.Combine(Dir, "Furniture"), Furniture)
        InitGraphics(Path.Combine(Dir, "Items"), Items)
        InitGraphics(Path.Combine(Dir, "Paperdolls"), Paperdolls)
        InitGraphics(Path.Combine(Dir, "Projectiles"), Projectiles)
        InitGraphics(Path.Combine(Dir, "Resources"), Resources)
        InitGraphics(Path.Combine(Dir, "Skillicons"), Skillicons)
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
        UnloadGraphics(Tilesets)
        UnloadGraphics(Characters)
        UnloadGraphics(Animations)
        UnloadGraphics(Emotes)
        UnloadGraphics(Faces)
        UnloadGraphics(Fog)
        UnloadGraphics(Furniture)
        UnloadGraphics(Items)
        UnloadGraphics(Paperdolls)
        UnloadGraphics(Projectiles)
        UnloadGraphics(Resources)
        UnloadGraphics(Skillicons)
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
                    RenderTexture(Tilesets(Map.Tile(X, Y).Layer(Layer).Tileset), New Vector2(ConvertMapX(X * PIC_X), ConvertMapY(Y * PIC_Y)), New Rectangle(Map.Tile(X, Y).Layer(Layer).X * PIC_X, Map.Tile(X, Y).Layer(Layer).Y * PIC_Y, PIC_X, PIC_Y))
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
        RenderTexture(Tilesets(Map.Tile(X, Y).Layer(layerNum).Tileset), New Vector2(destX, destY), New Rectangle(Autotile(X, Y).Layer(layerNum).srcX(quarterNum) + XOffset, Autotile(X, Y).Layer(layerNum).srcY(quarterNum) + YOffset, 16, 16))

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
        If Tex < 1 Or Tex > Animations.Length Then Exit Sub
        If FrameCount <= 0 Then Exit Sub
        If Animations(Tex).Texture Is Nothing Then LoadTexture(Animations(Tex))

        ' Get our source frame.
        Dim Width = Animations(Tex).Texture.Width
        Dim Height = Animations(Tex).Texture.Height
        Dim Source = New Rectangle((AnimInstance(Index).FrameIndex(Layer) - 1) * (Width / FrameCount), 0, Width / FrameCount, Height)

        ' Lock our animation to a target if we have to.
        Dim X As Integer
        Dim Y As Integer
        Dim LockIndex = AnimInstance(Index).lockindex
        Select Case AnimInstance(Index).LockType
            Case TargetType.None
                X = (AnimInstance(Index).X * 32) + 16 - (width / 2)
                Y = (AnimInstance(Index).Y * 32) + 16 - (height / 2)

            Case TargetType.Npc
                ' Make sure it's a valid Npc.
                If MapNpc(lockindex).Num > 0 AndAlso MapNpc(lockindex).Vital(Vitals.HP) > 0 Then
                    X = (MapNpc(lockindex).X * PIC_X) + 16 - (width / 2) + MapNpc(lockindex).XOffset
                    Y = (MapNpc(lockindex).Y * PIC_Y) + 16 - (height / 2) + MapNpc(lockindex).YOffset
                Else
                    ' This animation is not valid. Clear it.
                    ClearAnimInstance(Index)
                    Exit Sub
                End If

            Case TargetType.Player
                ' Is this player still on our map?
                If GetPlayerMap(lockindex) = GetPlayerMap(MyIndex) Then
                    X = (GetPlayerX(lockindex) * PIC_X) + 16 - (width / 2) + Player(lockindex).XOffset
                    Y = (GetPlayerY(lockindex) * PIC_Y) + 16 - (height / 2) + Player(lockindex).YOffset
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

        RenderTexture(Animations(Tex), New Vector2(ConvertMapX(X), ConvertMapY(Y)), Source)
    End Sub
    Private Sub DrawPlayer(ByVal Index As Integer)
        ' Make sure our sprite is valid.
        Dim Spritenum = GetPlayerSprite(Index)
        If Spritenum < 1 Or Spritenum > NumCharacters Then Exit Sub

        ' Make sure our sprite exists.
        If Characters(Spritenum).Texture Is Nothing Then LoadTexture(Characters(Spritenum))

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
        Dim Source = New Rectangle((Frame) * (Characters(Spritenum).Texture.Width / 4), FrameRow * (Characters(Spritenum).Texture.Height / 4), (Characters(Spritenum).Texture.Width / 4), (Characters(Spritenum).Texture.Height / 4))

        Dim X = GetPlayerX(Index) * PIC_X + Player(Index).XOffset - ((Characters(Spritenum).Texture.Width / 4 - 32) / 2)
        Dim Y As Integer
        If Characters(Spritenum).Texture.Height > 32 Then
            Y = GetPlayerY(Index) * PIC_Y + Player(Index).YOffset - ((Characters(Spritenum).Texture.Height / 4) - 32)
        Else
            Y = GetPlayerY(Index) * PIC_Y + Player(Index).YOffset
        End If

        ' render the actual sprite
        RenderTexture(Characters(Spritenum), New Vector2(ConvertMapX(X), ConvertMapY(Y)), Source)
    End Sub
    Private Sub DrawMapNpc(ByVal Index As Integer)

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
        If GetPlayerSprite(Index) < 1 Or GetPlayerSprite(Index) > NumCharacters Then
            TextY = (GetPlayerY(Index) * PIC_Y) + Player(Index).YOffset - 16
        Else
            TextY = (GetPlayerY(Index) * PIC_Y) + Player(Index).YOffset - (CharacterGFXInfo(GetPlayerSprite(Index)).Height / 4) + 16
        End If

        ' Draw name
        Call DrawText(Name, Size, New Vector2(ConvertMapX(TextX), ConvertMapY(TextY)), Color, BackColor)
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

        If Device.PreferredBackBufferHeight > Map.MaxY * PIC_X Then
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

End Class
