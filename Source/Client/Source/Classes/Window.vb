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

    ' Location Variables
    Private AppLocation As String

    ' Textures
    Private Tilesets() As TextureRec

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

        ' Initialize all our rendering arrays.
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

        ' TODO: Handle logic here.

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
        RenderTexture(Tilesets(1), New Vector2(0, 0), New Rectangle(0, 0, 32, 32))

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
        InitTilesets(Dir)
    End Sub

    Private Sub InitTilesets(ByVal Dir As String)
        Dim IsLooking As Boolean = True
        Dim Files As New List(Of String)
        Dim Id = 1

        ' Look for our files.
        While IsLooking
            Dim Fname = Path.Combine(Dir, "Tilesets", String.Format("{0}{1}", Id, GFX_EXT))
            If (File.Exists(Fname)) Then
                Files.Add(Fname)
                Id += 1
            Else
                IsLooking = False
            End If
        End While

        ' Redim our array and add filenames to it.
        ReDim Tilesets(0 To Files.Count)
        For i = 1 To Files.Count
            Tilesets(i) = New TextureRec()
            Tilesets(i).FileName = Files(i - 1)
            Tilesets(i).LastAccess = DateTime.MinValue
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
        UnloadTilesets()
    End Sub

    Private Sub UnloadTilesets()
        For Each T In Tilesets
            If Not T Is Nothing AndAlso Not T.Texture Is Nothing AndAlso T.LastAccess > DateTime.MinValue AndAlso DateTime.Now.Subtract(T.LastAccess).Minutes > 5 Then
                T.Texture = Nothing
                T.LastAccess = DateTime.MinValue
            End If
        Next
    End Sub
#End Region

#Region "Render Data"
    Private Sub RenderTexture(ByVal Texture As TextureRec, ByVal Destination As Vector2, Source As Rectangle)
        ' First make sure our texture exists.
        LoadTexture(Texture)

        ' Draw to screen
        View.Draw(Texture.Texture, Destination, Source, New Color(255, 255, 255, 255))
    End Sub
#End Region

End Class
