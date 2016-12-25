Imports System.IO
Imports Microsoft.Xna.Framework
Imports Microsoft.Xna.Framework.Graphics
Imports Microsoft.Xna.Framework.Input

Public Class Window : Inherits Game

    ' MonoGame Rendering Variables.
    Private Device As GraphicsDeviceManager
    Private View As SpriteBatch

    Private Fullscreen As Boolean
    Private ResolutionX As Integer
    Private ResolutionY As Integer

    ' Logic Variables.
    Private HasBeenResized As Boolean

    Public Sub New(ByVal ResX As Integer, ByVal ResY As Integer, ByVal IsFullscreen As Boolean)
        ' Create a brand new graphics device.
        Device = New GraphicsDeviceManager(Me)

        ' Set our resolution variables.
        Fullscreen = IsFullscreen
        ResolutionX = ResX
        ResolutionY = ResY

        ' Set the root directory of the application so we don't need to use full paths everywhere.
        Content.RootDirectory = Path.GetDirectoryName(Application.ExecutablePath)
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

        MyBase.Initialize()     ' Do not touch.
    End Sub

    Protected Overrides Sub LoadContent()
        ' Create our rendering object.
        View = New SpriteBatch(GraphicsDevice)

        ' TODO: Load our resources into memory. (Content.Load)

        MyBase.LoadContent()    ' Do not touch.
    End Sub

    Protected Overrides Sub UnloadContent()
        MyBase.UnloadContent()  ' Do not touch.
    End Sub

    Protected Overrides Sub Update(Time As GameTime)

        ' TODO: Handle logic here.

        ' If we have to, resize our backbuffer.
        If HasBeenResized Then
            Device.PreferredBackBufferWidth = Window.ClientBounds.Width
            Device.PreferredBackBufferHeight = Window.ClientBounds.Height
            Device.ApplyChanges()
            HasBeenResized = False
        End If

        MyBase.Update(Time)     ' Do not touch.
    End Sub

    Protected Overrides Sub Draw(Time As GameTime)
        ' Clear our screen and give it a lovely black background colour.
        GraphicsDevice.Clear(Color.Black)

        ' TODO: Render Graphics

        MyBase.Draw(Time)       ' Do not touch.
    End Sub

    Private Sub HandleClientSizeChanged(sender As Object, e As EventArgs)
        ' Notify our Update method that the game window has changed size.
        HasBeenResized = True
    End Sub
End Class
