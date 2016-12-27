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

        ' Allow the mouse to be visible.
        IsMouseVisible = True

        ' Allow our window to be resized.
        Window.AllowUserResizing = True
        AddHandler Window.ClientSizeChanged, AddressOf HandleClientSizeChanged

        ' Set our application location.
        AppLocation = Path.GetDirectoryName(Application.ExecutablePath)

        ' Initialize all our rendering arrays and variables.
        RenderOffset = New Vector2(0, 0)
        InitTextures()

        MyBase.Initialize()     ' Do not touch
    End Sub

    Protected Overrides Sub LoadContent()
        ' Create our MonoGame objects.
        View = New SpriteBatch(GraphicsDevice)

        ' TODO: Load our resources into memory. (Content.Load)
    End Sub

    Protected Overrides Sub UnloadContent()

    End Sub

    Protected Overrides Sub Update(Time As GameTime)

        ' Update our camera position.
        UpdateCameraOffset()

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
                For I = 1 To MAX_PLAYERS
                    If IsPlaying(I) And GetPlayerMap(I) = GetPlayerMap(MyIndex) Then
                        If Player(I).Y = Y Then
                            DrawPlayer(I)
                        End If
                        If PetAlive(I) Then
                            If Player(I).Pet.Y = Y Then
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
#End Region

#Region "Unload Data"
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
    Private Sub UnloadCharacters()
        For Each T In Characters
            If Not T Is Nothing AndAlso Not T.Texture Is Nothing AndAlso T.LastAccess > DateTime.MinValue AndAlso DateTime.Now.Subtract(T.LastAccess).Minutes > 5 Then
                T.Texture = Nothing
                T.LastAccess = DateTime.MinValue
            End If
        Next
    End Sub
    Private Sub UnloadAnimations()
        For Each T In Animations
            If Not T Is Nothing AndAlso Not T.Texture Is Nothing AndAlso T.LastAccess > DateTime.MinValue AndAlso DateTime.Now.Subtract(T.LastAccess).Minutes > 5 Then
                T.Texture = Nothing
                T.LastAccess = DateTime.MinValue
            End If
        Next
    End Sub
#End Region

#Region "Render Data"
    Private Sub DrawMapLayer(ByVal Layer As Integer)
        For X = 0 To Map.MaxX
            For Y = 0 To Map.MaxY
                Dim Tile = Map.Tile(X, Y).Layer(Layer)
                RenderTexture(Tilesets(Tile.Tileset), New Vector2(X * PIC_X, Y * PIC_Y), New Rectangle(New Point(Tile.X, Tile.Y), New Point(Tile.X + PIC_X, Tile.Y + PIC_Y)))
            Next
        Next
    End Sub

    Private Sub DrawAnimation(ByVal Index As Integer, ByVal Layer As Integer)

        Dim Sprite As Integer
        Dim sRECT As Rectangle
        Dim width As Integer, height As Integer
        Dim FrameCount As Integer
        Dim X As Integer, Y As Integer
        Dim lockindex As Integer

        If AnimInstance(Index).Animation = 0 Then
            ClearAnimInstance(Index)
            Exit Sub
        End If

        Sprite = Animation(AnimInstance(Index).Animation).Sprite(Layer)

        If Sprite < 1 Or Sprite > NumAnimations Then Exit Sub

        FrameCount = Animation(AnimInstance(Index).Animation).Frames(Layer)

        If FrameCount <= 0 Then Exit Sub

        ' MAke sure our animation is loaded.
        If Animations(Sprite).Texture Is Nothing Then LoadTexture(Animations(Sprite))

        ' total width divided by frame count
        width = Animations(Sprite).Texture.Width / FrameCount
        height = Animations(Sprite).Texture.Height

        sRECT.Y = 0
        sRECT.Height = height
        sRECT.X = (AnimInstance(Index).FrameIndex(Layer) - 1) * width
        sRECT.Width = width

        ' change x or y if locked
        If AnimInstance(Index).LockType > TargetType.None Then ' if <> none
            ' is a player
            If AnimInstance(Index).LockType = TargetType.Player Then
                ' quick save the index
                lockindex = AnimInstance(Index).lockindex
                ' check if is ingame
                If IsPlaying(lockindex) Then
                    ' check if on same map
                    If GetPlayerMap(lockindex) = GetPlayerMap(MyIndex) Then
                        ' is on map, is playing, set x & y
                        X = (GetPlayerX(lockindex) * PIC_X) + 16 - (width / 2) + Player(lockindex).XOffset
                        Y = (GetPlayerY(lockindex) * PIC_Y) + 16 - (height / 2) + Player(lockindex).YOffset
                    End If
                End If
            ElseIf AnimInstance(Index).LockType = TargetType.Npc Then
                ' quick save the index
                lockindex = AnimInstance(Index).lockindex
                ' check if NPC exists
                If MapNpc(lockindex).Num > 0 Then
                    ' check if alive
                    If MapNpc(lockindex).Vital(Vitals.HP) > 0 Then
                        ' exists, is alive, set x & y
                        X = (MapNpc(lockindex).X * PIC_X) + 16 - (width / 2) + MapNpc(lockindex).XOffset
                        Y = (MapNpc(lockindex).Y * PIC_Y) + 16 - (height / 2) + MapNpc(lockindex).YOffset
                    Else
                        ' npc not alive anymore, kill the animation
                        ClearAnimInstance(Index)
                        Exit Sub
                    End If
                Else
                    ' npc not alive anymore, kill the animation
                    ClearAnimInstance(Index)
                    Exit Sub
                End If
            End If
        Else
            ' no lock, default x + y
            X = (AnimInstance(Index).X * 32) + 16 - (width / 2)
            Y = (AnimInstance(Index).Y * 32) + 16 - (height / 2)
        End If

        X = ConvertMapX(X)
        Y = ConvertMapY(Y)

        ' Clip to screen
        If Y < 0 Then

            With sRECT
                .Y = .Y - Y
                .Height = .Height - (Y * (-1))
            End With

            Y = 0
        End If

        If X < 0 Then

            With sRECT
                .X = .X - X
                .Width = .Width - (Y * (-1))
            End With

            X = 0
        End If

        If sRECT.Width < 0 Or sRECT.Height < 0 Then Exit Sub

        RenderTexture(Animations(Sprite), New Vector2(X, Y), New Rectangle(sRECT.X, sRECT.Y, sRECT.Width, sRECT.Height))
    End Sub
    Private Sub DrawCharacter(ByVal Sprite As Integer, ByVal x2 As Integer, ByVal y2 As Integer, ByVal rec As Rectangle)
        Dim X As Integer
        Dim y As Integer

        If Sprite < 1 Or Sprite > NumCharacters Then Exit Sub

        RenderTexture(Characters(Sprite), New Vector2(X, y), New Rectangle(rec.X, rec.Y, rec.Width, rec.Height))
    End Sub
    Private Sub DrawPlayer(ByVal Index As Integer)
        Dim Anim As Byte, X As Integer, Y As Integer
        Dim Spritenum As Integer, spriteleft As Integer
        Dim attackspeed As Integer, AttackSprite As Byte
        Dim srcrec As Rectangle

        Spritenum = GetPlayerSprite(Index)

        AttackSprite = 0

        If Spritenum < 1 Or Spritenum > NumCharacters Then Exit Sub

        ' Make sure our sprite exists.
        If Characters(Spritenum).Texture Is Nothing Then LoadTexture(Characters(Spritenum))

        ' speed from weapon
        If GetPlayerEquipment(Index, EquipmentType.Weapon) > 0 Then
            attackspeed = Item(GetPlayerEquipment(Index, EquipmentType.Weapon)).Speed
        Else
            attackspeed = 1000
        End If

        ' Reset frame
        Anim = 0

        ' Check for attacking animation
        If Player(Index).AttackTimer + (attackspeed / 2) > GetTickCount() Then
            If Player(Index).Attacking = 1 Then
                If AttackSprite = 1 Then
                    Anim = 4
                Else
                    Anim = 3
                End If
            End If
        Else
            ' If not attacking, walk normally
            Select Case GetPlayerDir(Index)
                Case Direction.Up

                    If (Player(Index).YOffset > 8) Then Anim = Player(Index).Steps
                Case Direction.Down

                    If (Player(Index).YOffset < -8) Then Anim = Player(Index).Steps
                Case Direction.Left

                    If (Player(Index).XOffset > 8) Then Anim = Player(Index).Steps
                Case Direction.Right

                    If (Player(Index).XOffset < -8) Then Anim = Player(Index).Steps
            End Select

        End If

        ' Check to see if we want to stop making him attack
        With Player(Index)
            If .AttackTimer + attackspeed < GetTickCount() Then
                .Attacking = 0
                .AttackTimer = 0
            End If

        End With

        ' Set the left
        Select Case GetPlayerDir(Index)
            Case Direction.Up
                spriteleft = 3
            Case Direction.Right
                spriteleft = 2
            Case Direction.Down
                spriteleft = 0
            Case Direction.Left
                spriteleft = 1
        End Select

        If AttackSprite = 1 Then
            srcrec = New Rectangle((Anim) * (Characters(Spritenum).Texture.Width / 5), spriteleft * (Characters(Spritenum).Texture.Height / 4), (Characters(Spritenum).Texture.Width / 5), (Characters(Spritenum).Texture.Height / 4))
        Else
            srcrec = New Rectangle((Anim) * (Characters(Spritenum).Texture.Width / 4), spriteleft * (Characters(Spritenum).Texture.Height / 4), (Characters(Spritenum).Texture.Width / 4), (Characters(Spritenum).Texture.Height / 4))
        End If

        ' Calculate the X
        If AttackSprite = 1 Then
            X = GetPlayerX(Index) * PIC_X + Player(Index).XOffset - ((Characters(Spritenum).Texture.Width / 5 - 32) / 2)
        Else
            X = GetPlayerX(Index) * PIC_X + Player(Index).XOffset - ((Characters(Spritenum).Texture.Width / 4 - 32) / 2)
        End If

        ' Is the player's height more than 32..?
        If Characters(Spritenum).Texture.Height > 32 Then
            ' Create a 32 pixel offset for larger sprites
            Y = GetPlayerY(Index) * PIC_Y + Player(Index).YOffset - ((Characters(Spritenum).Texture.Height / 4) - 32)
        Else
            ' Proceed as normal
            Y = GetPlayerY(Index) * PIC_Y + Player(Index).YOffset
        End If

        ' render the actual sprite
        DrawCharacter(Spritenum, X, Y, srcrec)

        'check for paperdolling
        For i = 1 To EquipmentType.Count - 1
            If GetPlayerEquipment(Index, i) > 0 Then
                If Item(GetPlayerEquipment(Index, i)).Paperdoll > 0 Then
                    'DrawPaperdoll(X, Y, Item(GetPlayerEquipment(Index, i)).Paperdoll, Anim, spriteleft)
                End If
            End If
        Next

        ' Check to see if we want to stop showing emote
        With Player(Index)
            If .EmoteTimer < GetTickCount() Then
                .Emote = 0
                .EmoteTimer = 0
            End If
        End With

        'check for emotes
        'Player(Index).Emote = 4
        If Player(Index).Emote > 0 Then
            'DrawEmotes(X, Y, Player(Index).Emote)
        End If
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
#End Region

#Region "Logic Updates"

    Private Sub UpdateCameraOffset()

    End Sub

#End Region

End Class
