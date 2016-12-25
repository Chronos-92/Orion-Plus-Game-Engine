Imports System.IO
Imports Newtonsoft.Json

Module Options

    Public Sub LoadOptions()
        ' Get the file we'll be looking for.
        Dim fname = Path.Combine(AppLocation, DIR_DATA, FIL_OPTIONS)

        ' Make sure our options file exists.
        CheckOptions(fname)

        ' Load our options.
        GameOptions = JsonConvert.DeserializeObject(Of OptionsRec)(File.ReadAllText(fname))
    End Sub

    Public Sub SaveOptions()
        ' Get the file we'll be saving to.
        Dim fname = Path.Combine(AppLocation, DIR_DATA, FIL_OPTIONS)

        File.WriteAllText(fname, JsonConvert.SerializeObject(GameOptions))
    End Sub

    Private Sub CheckOptions(ByVal FileName As String)

        If Not File.Exists(FileName) Then
            ' Create our default options.
            GameOptions = New OptionsRec()
            With GameOptions
                .General.GameName = "Orion+ ORPG Engine"
                .General.SavedUser = String.Empty

                .Network.Address = "localhost"
                .Network.Address = 4001

                .Graphics.Fullscreen = False
                .Graphics.ResolutionX = 1280
                .Graphics.ResolutionY = 720
            End With

            ' Save our newly created Options
            SaveOptions()
        End If
    End Sub

End Module
