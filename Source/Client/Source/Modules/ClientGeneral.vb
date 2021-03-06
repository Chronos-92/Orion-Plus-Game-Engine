﻿Imports System.Threading
Imports System.Windows.Forms

Module ClientGeneral

    Public started As Boolean

    Public Function GetTickCount() As Integer
        Return Environment.TickCount
    End Function

    Sub startup()
        SFML.CSFML.Activate()

        frmMenu.Visible = True

        ReDim CharSelection(3)

        ReDim Player(0 To MAX_PLAYERS)

        For i = 0 To MAX_PLAYERS
            ClearPlayer(i)
        Next

        ReDim Autotile(0 To Map.MaxX, 0 To Map.MaxY)

        For X = 0 To Map.MaxX
            For Y = 0 To Map.MaxY
                ReDim Autotile(X, Y).Layer(0 To MapLayer.Count - 1)
                For i = 0 To MapLayer.Count - 1
                    ReDim Autotile(X, Y).Layer(i).srcX(0 To 4)
                    ReDim Autotile(X, Y).Layer(i).srcY(0 To 4)
                    ReDim Autotile(X, Y).Layer(i).QuarterTile(0 To 4)
                Next
            Next
        Next

        'Housing
        ReDim House(MAX_HOUSES)
        ReDim HouseConfig(MAX_HOUSES)

        'quests
        ReDim Quest(MAX_QUESTS)
        ClearQuests()

        ReDim Map.Npc(0 To MAX_MAP_NPCS)

        ReDim Item(MAX_ITEMS)
        For i = 0 To MAX_ITEMS
            For x = 0 To Stats.Count - 1
                ReDim Item(i).Add_Stat(x)
            Next
            For x = 0 To Stats.Count - 1
                ReDim Item(i).Stat_Req(x)
            Next

            ReDim Item(i).FurnitureBlocks(0 To 3, 0 To 3)
            ReDim Item(i).FurnitureFringe(0 To 3, 0 To 3)
        Next

        ReDim Npc(MAX_NPCS)
        For i = 0 To MAX_NPCS
            For x = 0 To Stats.Count - 1
                ReDim Npc(i).Stat(x)
            Next

            ReDim Npc(i).DropChance(5)
            ReDim Npc(i).DropItem(5)
            ReDim Npc(i).DropItemValue(5)

            ReDim Npc(i).Skill(6)
        Next

        ReDim MapNpc(MAX_MAP_NPCS)
        For i = 0 To MAX_MAP_NPCS
            For x = 0 To Vitals.Count - 1
                ReDim MapNpc(i).Vital(x)
            Next
        Next

        ReDim Shop(MAX_SHOPS)
        For i = 0 To MAX_SHOPS
            For x = 0 To MAX_TRADES
                ReDim Shop(i).TradeItem(x)
            Next
        Next

        ReDim Animation(MAX_ANIMATIONS)
        For i = 0 To MAX_ANIMATIONS
            For x = 0 To 1
                ReDim Animation(i).Sprite(x)
            Next
            For x = 0 To 1
                ReDim Animation(i).Frames(x)
            Next
            For x = 0 To 1
                ReDim Animation(i).LoopCount(x)
            Next
            For x = 0 To 1
                ReDim Animation(i).looptime(x)
            Next
        Next

        ReDim AnimInstance(MAX_ANIMATIONS)
        For i = 0 To MAX_ANIMATIONS
            For x = 0 To 1
                ReDim AnimInstance(i).Timer(x)
            Next
            For x = 0 To 1
                ReDim AnimInstance(i).Used(x)
            Next
            For x = 0 To 1
                ReDim AnimInstance(i).LoopIndex(x)
            Next
            For x = 0 To 1
                ReDim AnimInstance(i).FrameIndex(x)
            Next
        Next

        ReDim Bank.Item(MAX_BANK)

        ReDim MapProjectiles(MAX_PROJECTILES)
        ReDim Projectiles(MAX_PROJECTILES)

        ClearItems()

        'craft
        ClearRecipes()

        'party
        ClearPartie()

        'pets
        ClearPets()

        SetStatus("Loading...")
        GettingMap = True
        vbQuote = Chr(34) ' "

        ' Update the form with the game's name before it's loaded
        frmMainGame.Text = GAME_NAME

        SetStatus("Loading Options...")

        ' load options
        LoadOptions()

        ' randomize rnd's seed
        Randomize()

        SetStatus("Initializing Network...")

        frmMenu.Text = GAME_NAME

        ' DX7 Master Object is already created, early binding
        SetStatus("Initializing Graphics...")
        CheckTilesets()
        CheckCharacters()
        CheckPaperdolls()
        CheckAnimations()
        CheckItems()
        CheckResources()
        CheckSkillIcons()
        CheckFaces()
        CheckFog()
        CacheMusic()
        CacheSound()
        CheckEmotes()

        CheckFurniture()
        CheckProjectiles()

        InitGraphics()
        InitMessages()

        ' check if we have main-menu music
        If Options.Music = 1 Then
            If Len(Trim$(Options.MenuMusic)) > 0 Then
                PlayMusic(Trim$(Options.MenuMusic))
                MusicPlayer.Volume() = 100
            End If
        End If

        ' Reset values
        Ping = -1

        ' set values for directional blocking arrows
        DirArrowX(1) = 12 ' up
        DirArrowY(1) = 0
        DirArrowX(2) = 12 ' down
        DirArrowY(2) = 23
        DirArrowX(3) = 0 ' left
        DirArrowY(3) = 12
        DirArrowX(4) = 23 ' right
        DirArrowY(4) = 12

        'set gui switches
        HUDVisible = True

        SetStatus("Starting Game...")
        started = True
        frmmenuvisible = True
        pnlloadvisible = False

        pnlInventoryVisible = True

        GameLoop()
    End Sub

    Public Function isLoginLegal(ByVal Username As String, ByVal Password As String) As Boolean
        If Len(Trim$(Username)) >= 3 Then
            If Len(Trim$(Password)) >= 3 Then
                isLoginLegal = True
            Else
                isLoginLegal = False
            End If
        Else
            isLoginLegal = False
        End If

    End Function

    Public Function isStringLegal(ByVal sInput As String) As Boolean
        Dim i As Integer

        ' Prevent high ascii chars
        For i = 1 To Len(sInput)

            If (Asc(Mid$(sInput, i, 1))) < 32 Or Asc(Mid$(sInput, i, 1)) > 126 Then
                MsgBox("You cannot use high ASCII characters in your name, please re-enter.", vbOKOnly, GAME_NAME)
                isStringLegal = False
                Exit Function
            End If

        Next

        isStringLegal = True
    End Function

    Sub GameInit()
        pnlloadvisible = False

        ' Set the focus
        frmMainGame.picscreen.Focus()

        'stop the song playing
        StopMusic()
    End Sub

    Public Sub SetStatus(ByVal Caption As String)
        frmMenu.lblStatus.Text = Caption
    End Sub

    Public Sub MenuState(ByVal State As Integer)
        pnlloadvisible = True
        frmmenuvisible = False
        Select Case State
            Case MENU_STATE_ADDCHAR
                pnlCharCreateVisible = False
                pnlLoginVisible = False
                pnlRegisterVisible = False
                pnlCreditsVisible = False

                If ConnectToServer(1) Then
                    Call SetStatus("Connected, sending character addition data...")

                    If frmMenu.rdoMale.Checked = True Then
                        SendAddChar(SelectedChar, frmMenu.txtCharName.Text, Sex.Male, frmMenu.cmbClass.SelectedIndex + 1, newCharSprite)
                    Else
                        SendAddChar(SelectedChar, frmMenu.txtCharName.Text, Sex.Female, frmMenu.cmbClass.SelectedIndex + 1, newCharSprite)
                    End If
                End If

            Case MENU_STATE_NEWACCOUNT
                pnlLoginVisible = False
                pnlCharCreateVisible = False
                pnlRegisterVisible = False
                pnlCreditsVisible = False

                If ConnectToServer(1) Then
                    SetStatus("Connected, sending new account information...")
                    SendNewAccount(frmMenu.txtRuser.Text, frmMenu.txtRPass.Text)
                End If

            Case MENU_STATE_LOGIN
                pnlLoginVisible = False
                pnlCharCreateVisible = False
                pnlRegisterVisible = False
                pnlCreditsVisible = False
                tempUserName = frmMenu.txtLogin.Text
                tempPassword = frmMenu.txtPassword.Text

                If ConnectToServer(1) Then
                    SetStatus("Connected, sending login information...")
                    SendLogin(frmMenu.txtLogin.Text, frmMenu.txtPassword.Text)
                    Exit Sub
                End If
        End Select

    End Sub

    Public Function ConnectToServer(ByVal i As Integer) As Boolean
        Dim until As Integer
        ConnectToServer = False

        ' Check to see if we are already connected, if so just exit
        If IsConnected() Then
            ConnectToServer = True
            Exit Function
        End If

        If i = 4 Then Exit Function
        until = GetTickCount() + 3500

        Connect()

        SetStatus("Connecting to server...(" & i & ")")

        ' Wait until connected or a few seconds have passed and report the server being down
        Do While (Not IsConnected()) And (GetTickCount() <= until)
            DoEvents()
            Thread.Sleep(10)
        Loop

        ' return value
        If IsConnected() Then
            ConnectToServer = True
        End If

        If Not ConnectToServer Then
            ConnectToServer(i + 1)
        End If

    End Function

    Public Sub DoEvents()
        Application.DoEvents()
    End Sub

    Public Function FileExist(ByVal file_path) As Boolean
        FileExist = IO.File.Exists(file_path)
    End Function

    Public Sub RePositionGUI()

        'first change the tiles
        If Options.ScreenSize = 0 Then ' 800x600
            SCREEN_MAPX = 25
            SCREEN_MAPY = 19
        ElseIf Options.ScreenSize = 1 Then '1024x768
            SCREEN_MAPX = 31
            SCREEN_MAPY = 24
        ElseIf Options.ScreenSize = 2 Then
            SCREEN_MAPX = 35
            SCREEN_MAPY = 26
        End If

        'then the window
        frmMainGame.ClientSize = New Drawing.Size((SCREEN_MAPX) * PIC_X + PIC_X, (SCREEN_MAPY) * PIC_Y + PIC_Y)
        frmMainGame.picscreen.Width = (SCREEN_MAPX) * PIC_X + PIC_X
        frmMainGame.picscreen.Height = (SCREEN_MAPY) * PIC_Y + PIC_Y

        HalfX = ((SCREEN_MAPX) \ 2) * PIC_X
        HalfY = ((SCREEN_MAPY) \ 2) * PIC_Y
        ScreenX = (SCREEN_MAPX) * PIC_X
        ScreenY = (SCREEN_MAPY) * PIC_Y

        GameWindow.SetView(New SFML.Graphics.View(New SFML.Graphics.FloatRect(0, 0, (SCREEN_MAPX) * PIC_X + PIC_X, (SCREEN_MAPY) * PIC_Y + PIC_Y)))

        'Then we can recalculate the positions

        'chatwindow
        ChatWindowX = 1
        ChatWindowY = frmMainGame.Height - ChatWindowGFXInfo.Height - 65

        MyChatX = 1
        MyChatY = frmMainGame.Height - 60

        'hotbar
        If Options.ScreenSize = 0 Then
            HotbarX = HUDWindowX + HUDPanelGFXInfo.Width + 20
            HotbarY = 5

            'petbar
            PetbarX = HotbarX
            PetbarY = HotbarY + 34
        Else
            HotbarX = ChatWindowX + MyChatWindowGFXInfo.Width + 50
            HotbarY = frmMainGame.Height - HotBarGFXInfo.Height - 45

            'petbar
            PetbarX = HotbarX
            PetbarY = HotbarY - 34
        End If

        'action panel
        ActionPanelX = frmMainGame.Width - ActionPanelGFXInfo.Width - 25
        ActionPanelY = frmMainGame.Height - ActionPanelGFXInfo.Height - 45

        'Char Window
        CharWindowX = frmMainGame.Width - CharPanelGFXInfo.Width - 26
        CharWindowY = frmMainGame.Height - CharPanelGFXInfo.Height - ActionPanelGFXInfo.Height - 50

        'inv Window
        InvWindowX = frmMainGame.Width - InvPanelGFXInfo.Width - 26
        InvWindowY = frmMainGame.Height - InvPanelGFXInfo.Height - ActionPanelGFXInfo.Height - 50

        'skill window
        SkillWindowX = frmMainGame.Width - SkillPanelGFXInfo.Width - 26
        SkillWindowY = frmMainGame.Height - SkillPanelGFXInfo.Height - ActionPanelGFXInfo.Height - 50

    End Sub

    Public Sub DestroyGame()
        SendLeaveGame()

        ' break out of GameLoop
        InGame = False

        DestroyGraphics()
        GameDestroyed = True
        If Not PlayerSocket Is Nothing Then PlayerSocket.Close()
        PlayerSocket = Nothing
        Application.Exit()
        End
    End Sub

    Public Sub CheckDir(ByVal DirPath As String)

        If Not IO.Directory.Exists(DirPath) Then
            IO.Directory.CreateDirectory(DirPath)
        End If

    End Sub

    Public Function GetExceptionInfo(ex As Exception) As String
        Dim Result As String
        Dim hr As Integer = Runtime.InteropServices.Marshal.GetHRForException(ex)
        Result = ex.GetType.ToString & "(0x" & hr.ToString("X8") & "): " & ex.Message & Environment.NewLine & ex.StackTrace & Environment.NewLine
        Dim st As StackTrace = New StackTrace(ex, True)
        For Each sf As StackFrame In st.GetFrames
            If sf.GetFileLineNumber() > 0 Then
                Result &= "Line:" & sf.GetFileLineNumber() & " Filename: " & IO.Path.GetFileName(sf.GetFileName) & Environment.NewLine
            End If
        Next
        Return Result
    End Function
End Module