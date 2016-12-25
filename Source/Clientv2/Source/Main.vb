Imports System.IO

Module Main
    Public Sub Main(ByVal cmdArgs() As String)

        ' Initialize our client
        InitClient()

        ' Create a brand new game Window and run it.
        Dim game = New Window(GameOptions.Graphics.ResolutionX, GameOptions.Graphics.ResolutionY, GameOptions.Graphics.Fullscreen)
        game.Run()

    End Sub

    Private Sub InitClient()

        ' Set our required globals.
        AppLocation = Path.GetDirectoryName(Application.ExecutablePath)

        ' Check to see if all our directories are in place.
        CheckDirectories()

        ' Load our options.
        LoadOptions()

    End Sub

    Private Sub CheckDirectories()
        CheckDir(Path.Combine(AppLocation, DIR_DATA))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_ANIMATIONS))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_CHARACTERS))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_EMOTES))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_FACES))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_FOGS))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_FURNITURE))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_GUI))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_ITEMS))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_MISC))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_MUSIC))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_PAPERDOLLS))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_PROJECTILES))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_RESOURCES))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_SKILLICONS))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_SOUND))
        CheckDir(Path.Combine(AppLocation, DIR_DATA, DIR_TILESETS))
    End Sub

    Private Sub CheckDir(ByVal Dir As String)
        If Not Directory.Exists(Dir) Then Directory.CreateDirectory(Dir)
    End Sub

End Module
