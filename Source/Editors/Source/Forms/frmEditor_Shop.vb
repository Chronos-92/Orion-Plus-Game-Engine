﻿Imports System.Windows.Forms

Public Class frmEditor_Shop

    Private Sub txtName_TextChanged(ByVal sender As Object, ByVal e As EventArgs) Handles txtName.TextChanged
        Dim tmpIndex As Integer

        If EditorIndex = 0 Then Exit Sub
        tmpIndex = lstIndex.SelectedIndex
        Shop(EditorIndex).Name = Trim$(txtName.Text)
        lstIndex.Items.RemoveAt(EditorIndex - 1)
        lstIndex.Items.Insert(EditorIndex - 1, EditorIndex & ": " & Shop(EditorIndex).Name)
        lstIndex.SelectedIndex = tmpIndex
    End Sub

    Private Sub scrlBuy_Scroll(ByVal sender As Object, ByVal e As ScrollEventArgs) Handles scrlBuy.Scroll
        lblBuy.Text = "Buy Rate: " & scrlBuy.Value & "%"
        Shop(EditorIndex).BuyRate = scrlBuy.Value
    End Sub

    Private Sub btnUpdate_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnUpdate.Click
        Dim Index As Integer
        Index = lstTradeItem.SelectedIndex + 1
        If Index = 0 Then Exit Sub
        With Shop(EditorIndex).TradeItem(Index)
            .Item = cmbItem.SelectedIndex
            .ItemValue = Val(txtItemValue.Text)
            .CostItem = cmbCostItem.SelectedIndex
            .CostValue = Val(txtCostValue.Text)
        End With
        Call UpdateShopTrade()
    End Sub

    Private Sub btnDeleteTrade_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnDeleteTrade.Click
        Dim Index As Integer
        Index = lstTradeItem.SelectedIndex + 1
        If Index = 0 Then Exit Sub
        With Shop(EditorIndex).TradeItem(Index)
            .Item = 0
            .ItemValue = 0
            .CostItem = 0
            .CostValue = 0
        End With
        Call UpdateShopTrade()
    End Sub

    Private Sub lstIndex_Click(ByVal sender As Object, ByVal e As EventArgs) Handles lstIndex.Click
        ShopEditorInit()
    End Sub

    Private Sub btnSave_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnSave.Click
        If Len(Trim$(txtName.Text)) = 0 Then
            Call MsgBox("Name required.")
        Else
            Call ShopEditorOk()
        End If
    End Sub

    Private Sub btnCancel_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnCancel.Click
        ShopEditorCancel()
    End Sub

    Private Sub btnDelete_Click(ByVal sender As Object, ByVal e As EventArgs) Handles btnDelete.Click
        Dim tmpIndex As Integer

        ClearShop(EditorIndex)

        tmpIndex = lstIndex.SelectedIndex
        lstIndex.Items.RemoveAt(EditorIndex - 1)
        lstIndex.Items.Insert(EditorIndex - 1, EditorIndex & ": " & Shop(EditorIndex).Name)
        lstIndex.SelectedIndex = tmpIndex

        ShopEditorInit()
    End Sub

    Private Sub scrlFace_Scroll(sender As Object, e As ScrollEventArgs) Handles scrlFace.Scroll
        lblFace.Text = "Face: " & scrlFace.Value

        If FileExist(Application.StartupPath & GFX_PATH & "Faces\" & scrlFace.Value & GFX_EXT) Then
            Me.picFace.BackgroundImage = Drawing.Image.FromFile(Application.StartupPath & GFX_PATH & "Faces\" & scrlFace.Value & GFX_EXT)
        End If

        Shop(EditorIndex).Face = scrlFace.Value
    End Sub
End Class