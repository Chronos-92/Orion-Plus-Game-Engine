Public Class OptionsRec
    Public General As GeneralOptions
    Public Network As NetworkOptions
    Public Graphics As GraphicsOptions

    Public Sub New()
        General = New GeneralOptions()
        Network = New NetworkOptions()
        Graphics = New GraphicsOptions()
    End Sub

End Class

Public Class GeneralOptions
    Public GameName As String
    Public SavedUser As String
End Class

Public Class NetworkOptions
    Public Address As String
    Public Port As Integer
End Class

Public Class GraphicsOptions
    Public ResolutionX As Integer
    Public ResolutionY As Integer
    Public Fullscreen As Boolean
End Class
