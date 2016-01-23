'Harman Singh
'Ch8 Ex1 - Student Names
'December 16, 2014

'Displays 5 names inputted by the user in a list box. The names are presented in the reverse order they were inputted.


Public Class Form1



    Structure Block
        Dim count As Integer
        Dim posRow As Integer
        Dim posCol As Integer
        Dim btn As Button

        'Animation Information
        Dim timer As Timer  'Each structure will have its own timer so that each block can be animated without interference from another block's animation 
        Dim NumOfEmpSpacesAvailable As Integer
        Dim directionOfMotion As String
        Dim counter As Integer
    End Structure

    Dim Blocks(,) As Block
    Dim GameInitialized As Boolean = False
    Dim Total As Integer
    Dim LargestTile As Integer
    Dim Length As Integer = 0
    Dim animationComplete As Boolean = False
   

    'The method ouputs names inputted by the user in reverse order
    'Pre: The method is initiated when btnAddNames is clicked
    'Post: Names inputted by user are added to listbox in reverse order
    '
    Private Sub NewGame(sender As System.Object, e As System.EventArgs) Handles NewGame4x4ToolStripMenuItem.Click, NewGame5x5ToolStripMenuItem.Click

        Length = Val(sender.Tag()) 'The length of the board that the game will be played on 
        ReDim Blocks(Length, Length) 'Array Indexes are meaningful and will refer to the position of each block

        Total = 0
        LargestTile = 2

        If (Length = 4) Then
            Blocks(1, 1).btn = Me.btn11
            Blocks(1, 1).timer = Me.tim11
            Blocks(1, 2).btn = Me.btn12
            Blocks(1, 2).timer = Me.tim12
            Blocks(1, 3).btn = Me.btn13
            Blocks(1, 3).timer = Me.tim13
            Blocks(1, 4).btn = Me.btn14
            Blocks(1, 4).timer = Me.tim14
            Blocks(2, 1).btn = Me.btn21
            Blocks(2, 1).timer = Me.tim21
            Blocks(2, 2).btn = Me.btn22
            Blocks(2, 2).timer = Me.tim22
            Blocks(2, 3).btn = Me.btn23
            Blocks(2, 3).timer = Me.tim23
            Blocks(2, 4).btn = Me.btn24
            Blocks(2, 4).timer = Me.tim24
            Blocks(3, 1).btn = Me.btn31
            Blocks(3, 1).timer = Me.tim31
            Blocks(3, 2).btn = Me.btn32
            Blocks(3, 2).timer = Me.tim32
            Blocks(3, 3).btn = Me.btn33
            Blocks(3, 3).timer = Me.tim33
            Blocks(3, 4).btn = Me.btn34
            Blocks(3, 4).timer = Me.tim34
            Blocks(4, 1).btn = Me.btn41
            Blocks(4, 1).timer = Me.tim41
            Blocks(4, 2).btn = Me.btn42
            Blocks(4, 2).timer = Me.tim42
            Blocks(4, 3).btn = Me.btn43
            Blocks(4, 3).timer = Me.tim43
            Blocks(4, 4).btn = Me.btn44
            Blocks(4, 4).timer = Me.tim44
        End If

                If (Length = 5) Then
                End If

        For row As Integer = 1 To Length
            For col As Integer = 1 To Length
                Blocks(row, col).count = 0
                Blocks(row, col).posRow = row
                Blocks(row, col).posCol = col
                Blocks(row, col).counter = 1
                Blocks(row, col).timer.Interval = 50
                LoadValueOnBlock(Blocks(row, col)) 'If game was played previously, removes image on button, and turns button invisible (because block has count = 0, which means its image will be removed, and will be turned invisible )
            Next
        Next

        AssignRandomValues(Blocks, 2)

        GameInitialized = True
    End Sub


    Sub ControlBlocks(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown
        Me.KeyPreview = True


        If (e.KeyCode = Keys.W And GameInitialized = True) Then 'Up

            AnimationInformationUp(Blocks)

            For col As Integer = 1 To Length
                Dim row As Integer = 1
                Dim searchForNextBlock As Boolean = True
                Do While (searchForNextBlock And row <= Length)
                    If (Blocks(row, col).count >= 2) Then
                        Blocks(row, col).timer.Enabled = True
                        searchForNextBlock = False
                    End If
                    row += 1
                Loop
            Next

        ElseIf (e.KeyCode = Keys.A And GameInitialized = True) Then 'Left

            AnimationInformationToLeft(Blocks)

            For row As Integer = 1 To Length
                Dim col As Integer = 1
                Dim searchForNextBlock As Boolean = True
                Do While (searchForNextBlock And col <= Length)
                    If (Blocks(row, col).count >= 2) Then
                        Blocks(row, col).timer.Enabled = True
                        searchForNextBlock = False
                    End If
                    col += 1
                Loop
            Next
        ElseIf (e.KeyCode = Keys.S And GameInitialized = True) Then 'Down

            AnimationInformationDown(Blocks)

            For col As Integer = 1 To Length
                Dim row As Integer = Length
                Dim searchForNextBlock As Boolean = True
                Do While (searchForNextBlock And row >= 1)
                    If (Blocks(row, col).count >= 2) Then
                        Blocks(row, col).timer.Enabled = True
                        searchForNextBlock = False
                    End If
                    row -= 1
                Loop
            Next



        ElseIf (e.KeyCode = Keys.D And GameInitialized = True) Then 'Right

            AnimationInformationToRight(Blocks) 'Gathers Information (and saves in block structure) on how the blocks will reorient themselves when player presses button. Information includes num of Empty spaces available for button to move, and direction

            For row As Integer = 1 To Length
                Dim col As Integer = Length
                Dim searchForNextBlock As Boolean = True
                Do While (searchForNextBlock And col >= 1)
                    If (Blocks(row, col).count >= 2) Then
                        Blocks(row, col).timer.Enabled = True
                        searchForNextBlock = False
                    End If
                    col -= 1
                Loop
            Next

        End If

    End Sub


    Private Sub AddTiles(ByRef blockset(,) As Block)


        Select Case RandomInteger(1, 100)

            Case Is > 50
                AssignRandomValues(blockset, 1)
        End Select

    End Sub





    Private Sub AnimateMotion(sender As System.Object, e As System.EventArgs) Handles tim11.Tick, tim12.Tick, tim13.Tick, tim14.Tick, tim21.Tick, tim22.Tick, tim23.Tick, tim24.Tick, tim31.Tick, tim32.Tick, tim33.Tick, tim34.Tick, tim41.Tick, tim42.Tick, tim43.Tick, tim44.Tick

        Dim row As Integer = Val(sender.Tag().Substring(0, 1))
        Dim col As Integer = Val(sender.Tag().Substring(2, 1))
        Dim direction As String = Blocks(row, col).directionOfMotion
        Dim counter As Integer = Blocks(row, col).counter

        Select Case direction
            Case "right"
                If (Blocks(row, col).NumOfEmpSpacesAvailable <> 0) Then
                    moveBlockToSpace(Blocks, Blocks(row, col + (counter - 1)), Blocks(row, col + counter))
                    Blocks(row, col).counter += 1
                End If
                If (Blocks(row, col).counter > Blocks(row, col).NumOfEmpSpacesAvailable) Then 'Once Animation is complete for one block, turn off the animation for that block and proceed to next block 
                    Blocks(row, col).timer.Enabled = False
                    Blocks(row, col).counter = 1
                    Combine(Blocks, Blocks(row, col + Blocks(row, col).NumOfEmpSpacesAvailable), direction) 'After block has moved, It is checked if the block can combine with another block that has the same count, and if they can, they combine and share the location of the block further to the right
                    Dim colNumberForNextBlock As Integer = col - 1
                    Dim searchForNextBlock = True
                    Do While (searchForNextBlock And colNumberForNextBlock >= 1)
                        If (Blocks(row, colNumberForNextBlock).count >= 2) Then
                            Blocks(row, colNumberForNextBlock).timer.Enabled = True
                            searchForNextBlock = False
                        End If
                        colNumberForNextBlock -= 1
                
                    Loop
                End If

            Case "left"
                If (Blocks(row, col).NumOfEmpSpacesAvailable <> 0) Then
                    moveBlockToSpace(Blocks, Blocks(row, col - (counter - 1)), Blocks(row, col - counter))
                    Blocks(row, col).counter += 1
                End If
                If (Blocks(row, col).counter > Blocks(row, col).NumOfEmpSpacesAvailable) Then 'Once Animation is complete for one block, turn off the animation for that block and proceed to next block 
                    Blocks(row, col).timer.Enabled = False
                    Blocks(row, col).counter = 1
                    Combine(Blocks, Blocks(row, col - Blocks(row, col).NumOfEmpSpacesAvailable), direction) 'After block has moved, It is checked if the block can combine with another block that has the same count, and if they can, they combine and share the location of the block further to the right
                    Dim colNumberForNextBlock As Integer = col + 1
                    Dim searchForNextBlock = True
                    Do While (searchForNextBlock And colNumberForNextBlock <= Length)
                        If (Blocks(row, colNumberForNextBlock).count >= 2) Then
                            Blocks(row, colNumberForNextBlock).timer.Enabled = True
                            searchForNextBlock = False
                        End If
                        colNumberForNextBlock += 1
                    Loop
                End If

            Case "up"
                If (Blocks(row, col).NumOfEmpSpacesAvailable <> 0) Then
                    moveBlockToSpace(Blocks, Blocks(row - (counter - 1), col), Blocks(row - counter, col))
                    Blocks(row, col).counter += 1
                End If
                If (Blocks(row, col).counter > Blocks(row, col).NumOfEmpSpacesAvailable) Then 'Once Animation is complete for one block, turn off the animation for that block and proceed to next block 
                    Blocks(row, col).timer.Enabled = False
                    Blocks(row, col).counter = 1
                    Combine(Blocks, Blocks(row - Blocks(row, col).NumOfEmpSpacesAvailable, col), direction)
                    Dim rowNumberForNextBlock As Integer = row + 1
                    Dim searchForNextBlock = True
                    Do While (searchForNextBlock And rowNumberForNextBlock <= Length)
                        If (Blocks(rowNumberForNextBlock, col).count >= 2) Then
                            Blocks(rowNumberForNextBlock, col).timer.Enabled = True
                            searchForNextBlock = False
                        End If
                        rowNumberForNextBlock += 1
                    Loop
                End If

            Case "down"
                If (Blocks(row, col).NumOfEmpSpacesAvailable <> 0) Then
                    moveBlockToSpace(Blocks, Blocks(row + (counter - 1), col), Blocks(row + counter, col))
                    Blocks(row, col).counter += 1
                End If
                If (Blocks(row, col).counter > Blocks(row, col).NumOfEmpSpacesAvailable) Then 'Once Animation is complete for one block, turn off the animation for that block and proceed to next block 
                    Blocks(row, col).timer.Enabled = False
                    Blocks(row, col).counter = 1
                    Combine(Blocks, Blocks(row + Blocks(row, col).NumOfEmpSpacesAvailable, col), direction)
                    Dim rowNumberForNextBlock As Integer = row - 1
                    Dim searchForNextBlock = True
                    Do While (searchForNextBlock And rowNumberForNextBlock >= 1)
                        If (Blocks(rowNumberForNextBlock, col).count >= 2) Then
                            Blocks(rowNumberForNextBlock, col).timer.Enabled = True
                            searchForNextBlock = False
                        End If
                        rowNumberForNextBlock -= 1
                    Loop
                End If
        End Select
    End Sub


    Private Sub Combine(ByRef BlockSet(,) As Block, ByVal block As Block, ByVal direction As String)

        Dim movedblockrow As Integer = block.posRow
        Dim movedblockcol As Integer = block.posCol


        Select Case direction
            Case "right"
                If ((movedblockcol + 1) <= Length) Then
                    If (Blocks(movedblockrow, movedblockcol + 1).count = Blocks(movedblockrow, movedblockcol).count) Then
                        Blocks(movedblockrow, movedblockcol + 1).count = Blocks(movedblockrow, movedblockcol + 1).count + Blocks(movedblockrow, movedblockcol).count
                        Blocks(movedblockrow, movedblockcol).count = 0
                        LoadValueOnBlock(Blocks(movedblockrow, movedblockcol))
                        LoadValueOnBlock(Blocks(movedblockrow, movedblockcol + 1))
                    End If
                End If
            Case "left"
                If ((movedblockcol - 1) >= 1) Then
                    If (Blocks(movedblockrow, movedblockcol - 1).count = Blocks(movedblockrow, movedblockcol).count) Then
                        Blocks(movedblockrow, movedblockcol - 1).count = Blocks(movedblockrow, movedblockcol - 1).count + Blocks(movedblockrow, movedblockcol).count
                        Blocks(movedblockrow, movedblockcol).count = 0
                        LoadValueOnBlock(Blocks(movedblockrow, movedblockcol))
                        LoadValueOnBlock(Blocks(movedblockrow, movedblockcol - 1))
                    End If
                End If
            Case "up"
                If ((movedblockrow - 1) >= 1) Then
                    If (Blocks(movedblockrow - 1, movedblockcol).count = Blocks(movedblockrow, movedblockcol).count) Then
                        Blocks(movedblockrow - 1, movedblockcol).count = Blocks(movedblockrow - 1, movedblockcol).count + Blocks(movedblockrow, movedblockcol).count
                        Blocks(movedblockrow, movedblockcol).count = 0
                        LoadValueOnBlock(Blocks(movedblockrow, movedblockcol))
                        LoadValueOnBlock(Blocks(movedblockrow - 1, movedblockcol))
                    End If
                End If

            Case "down"
                If ((movedblockrow + 1) <= Length) Then
                    If (Blocks(movedblockrow + 1, movedblockcol).count = Blocks(movedblockrow, movedblockcol).count) Then
                        Blocks(movedblockrow + 1, movedblockcol).count = Blocks(movedblockrow + 1, movedblockcol).count + Blocks(movedblockrow, movedblockcol).count
                        Blocks(movedblockrow, movedblockcol).count = 0
                        LoadValueOnBlock(Blocks(movedblockrow, movedblockcol))
                        LoadValueOnBlock(Blocks(movedblockrow + 1, movedblockcol))
                    End If
                End If
        End Select

        Dim animationComplete As Boolean = True
        Dim didNothingMove As Boolean = True
        For row As Integer = 1 To Length
            For col As Integer = 1 To Length
                If (Blocks(row, col).timer.Enabled = True) Then
                    animationComplete = False

                End If
                If (Blocks(row, col).NumOfEmpSpacesAvailable > 0) Then
                    didNothingMove = False

                End If


            Next
        Next

        If (animationComplete) Then
            AddTiles(Blocks)
        End If


    End Sub


    Private Sub AnimationInformationToLeft(ByRef blocks(,) As Block)
        For row As Integer = 1 To Length
            For i As Integer = 1 To Length
                If (blocks(row, i).count >= 2) Then
                    blocks(row, i).NumOfEmpSpacesAvailable = numEmptySpaces(blocks, blocks(row, i), "left")
                    blocks(row, i).directionOfMotion = "left"
                End If
            Next
        Next
    End Sub

    Private Sub AnimationInformationUp(ByRef blocks(,) As Block)
        For col As Integer = 1 To Length
            For i As Integer = 1 To Length
                If (blocks(i, col).count >= 2) Then
                    blocks(i, col).NumOfEmpSpacesAvailable = numEmptySpaces(blocks, blocks(i, col), "up")
                    blocks(i, col).directionOfMotion = "up"
                End If
            Next
        Next
    End Sub

    Private Sub AnimationInformationDown(ByRef blocks(,) As Block)
        For col As Integer = 1 To Length
            For i As Integer = Length To 1 Step -1
                If (blocks(i, col).count >= 2) Then
                    blocks(i, col).NumOfEmpSpacesAvailable = numEmptySpaces(blocks, blocks(i, col), "down")
                    blocks(i, col).directionOfMotion = "down"
                End If
            Next
        Next
    End Sub



    Private Sub AnimationInformationToRight(ByRef blocks(,) As Block)
        For row As Integer = 1 To Length
            For i As Integer = Length To 1 Step -1
                If (blocks(row, i).count >= 2) Then
                    blocks(row, i).NumOfEmpSpacesAvailable = numEmptySpaces(blocks, blocks(row, i), "right")
                    blocks(row, i).directionOfMotion = "right"
                End If
            Next
        Next
    End Sub



    Sub moveBlockToSpace(ByRef blocks(,) As Block, ByVal block As Block, ByVal space As Block)

        Dim spaceRow As Integer = space.posRow
        Dim spaceCol As Integer = space.posCol
        Dim blockRow As Integer = block.posRow
        Dim blockCol As Integer = block.posCol

        If (spaceRow <> blockRow Or spaceCol <> blockCol) Then 'If they are the same block, they should not do anything
            blocks(spaceRow, spaceCol).count = blocks(blockRow, blockCol).count
            blocks(blockRow, blockCol).count = 0
            LoadValueOnBlock(blocks(spaceRow, spaceCol))
            LoadValueOnBlock(blocks(blockRow, blockCol))
        End If
    End Sub



    Function numEmptySpaces(ByRef blocks(,) As Block, ByVal block As Block, ByVal direction As String) As Integer
        Dim row As Integer = block.posRow
        Dim col As Integer = block.posCol
        Dim numOfEmptySpaces = 0

        Select Case direction
            Case "right"
                If ((col + 1) <= Length) Then
                    For i As Integer = col + 1 To Length
                        If (blocks(row, i).count = 0) Then
                            numOfEmptySpaces += 1
                        End If
                    Next
                End If

            Case "left"
                If ((col - 1) >= 1) Then
                    For i As Integer = col - 1 To 1 Step -1
                        If (blocks(row, i).count = 0) Then
                            numOfEmptySpaces += 1
                        End If
                    Next
                End If

            Case "up"
                If ((row - 1) >= 1) Then
                    For i As Integer = row - 1 To 1 Step -1
                        If (blocks(i, col).count = 0) Then
                            numOfEmptySpaces += 1
                        End If
                    Next
                End If

            Case "down"
                If ((row + 1) <= Length) Then
                    For i As Integer = row + 1 To Length
                        If (blocks(i, col).count = 0) Then
                            numOfEmptySpaces += 1
                        End If
                    Next
                End If
        End Select
        Return numOfEmptySpaces
    End Function


    Private Function NumBlocksInRow(ByRef blocks(,), ByVal rowNumber) As Integer
        Dim count As Integer = 0
        For i As Integer = 1 To Length
            If (blocks(rowNumber, i).count >= 2) Then
                count += 1
            End If
        Next
        Return count
    End Function


    Private Function NumBlocksInCol(ByRef blocks(,), ByVal colNumber) As Integer
        Dim count As Integer = 0
        For i As Integer = 1 To Length
            If (blocks(i, colNumber).count >= 2) Then
                count += 1
            End If
        Next
        Return count
    End Function



    Private Sub UpdateScoreAndMaxTile()

    End Sub


    'The method ouputs names inputted by the user in reverse order
    'Pre: The method is initiated when btnAddNames is clicked
    'Post: Names inputted by user are added to listbox in reverse order
    '
    Private Sub AssignRandomValues(ByRef blocks(,) As Block, ByVal num As Integer)

        Dim rndVal As Integer

        Select Case RandomInteger(1, 100)
            Case Is < 70
                rndVal = 2
            Case Is < 90
                rndVal = 4
            Case Is < 99
                rndVal = 8
            Case Else
                rndVal = 64
        End Select

        For i As Integer = 1 To num
            Dim a As Block = FindRandomUnoccupiedLocation(blocks)
            blocks(a.posRow, a.posCol).count = rndVal
            blocks(a.posRow, a.posCol).btn.Visible = True
            LoadValueOnBlock(blocks(a.posRow, a.posCol))
        Next

    End Sub


    'The method finds a random unused location
    'Pre: One location in the button array must be empty or runtime error will result
    'Post: Names inputted by user are added to listbox in reverse order
    '
    Private Function FindRandomUnoccupiedLocation(ByRef blocks(,) As Block) As Block

        Do While True 'Infinite loop to find if there is an unused block

            Dim randRow As Integer = RandomInteger(1, 4)
            Dim randCol As Integer = RandomInteger(1, 4)


            If blocks(randRow, randCol).count = 0 Then
                Return blocks(randRow, randCol) 'Stops Loop
            End If
        Loop

    End Function


    'The method ouputs names inputted by the user in reverse order
    'Pre: The method is initiated when btnAddNames is clicked
    'Post: Names inputted by user are added to listbox in reverse order
    '
    Private Function RandomInteger(ByVal low, ByVal high) As Integer
        Randomize()
        Return Int((high - low + 1) * Rnd()) + low
    End Function


    'The method ouputs names inputted by the user in reverse order
    'Pre: The method is initiated when btnAddNames is clicked
    'Post: Names inputted by user are added to listbox in reverse order
    '
    Private Sub LoadValueOnBlock(ByRef block As Block)
        Select Case block.count
            Case 0
                block.btn.Image = Nothing
                block.btn.Visible = False
            Case 2
                block.btn.Visible = True
                block.btn.Image = My.Resources._2
            Case 4
                block.btn.Visible = True
                block.btn.Image = My.Resources._4
            Case 8
                block.btn.Visible = True
                block.btn.Image = My.Resources._8
            Case 16
                block.btn.Visible = True
                block.btn.Image = My.Resources._16
            Case 32
                block.btn.Visible = True
                block.btn.Image = My.Resources._32
            Case 64
                block.btn.Visible = True
                block.btn.Image = My.Resources._64
            Case 128
                block.btn.Visible = True
                block.btn.Image = My.Resources._128
            Case 256
                block.btn.Visible = True
                block.btn.Image = My.Resources._256
            Case 512
                block.btn.Visible = True
                block.btn.Image = My.Resources._512
            Case 1024
                block.btn.Visible = True
                block.btn.Image = My.Resources._1024
            Case 2048
                block.btn.Visible = True
                block.btn.Image = My.Resources._2048
        End Select

    End Sub









    Private Sub ExitToolStripMenuItem2_Click(sender As System.Object, e As System.EventArgs) Handles ExitToolStripMenuItem2.Click
        Application.Exit()

    End Sub

    Private Sub MoveBlocksInRowRight(ByRef blocks(,) As Block, ByVal row As Integer)
        Dim spacesEmpty As Integer = 0
        For i As Integer = Length To 1 Step -1
            If (blocks(row, i).count >= 2) Then
                spacesEmpty = numEmptySpaces(blocks, blocks(row, i), "right")
                If (spacesEmpty <> 0) Then
                    moveBlockToSpace(blocks, blocks(row, i), blocks(row, i + spacesEmpty))
                End If
            End If
        Next
    End Sub

  
End Class


















'Function EquateBlockSets(ByVal OriginalBlock(,), ByVal ResultantBlock(,))
'    For row As Integer = 1 To Length - 1
'        For col As Integer = 1 To Length - 1
'            OriginalBlock(row, col).count = ResultantBlock(row, col).count
'            OriginalBlock(row, col).posRow = ResultantBlock(row, col).posRow
'            OriginalBlock(row, col).posCol = ResultantBlock(row, col).posCol
'            OriginalBlock(row, col).counter = ResultantBlock(row, col).counter
'            OriginalBlock(row, col).animationComplete = ResultantBlock(row, col).animationComplete
'        Next
'    Next
'End Function


'  Private Sub MoveBlocksInRowRight(ByRef blocks(,) As Block, ByVal row As Integer)

'    Dim spacesEmpty As Integer = 0
'   Dim counter As Integer


'    For i As Integer = blocks.GetLength(1) - 1 To 1 Step -1
'       If (blocks(row, i).count >= 2) Then
'           spacesEmpty = numEmptySpaces(blocks, blocks(row, i), "right")

'    MessageBox.Show("Num of Empty Spaces " & spacesEmpty & " Row: " & row & "Col: " & i)
'            If (spacesEmpty <> 0) Then ' blocks should only move if there is a space for them to move into
'                moveBlockToSpace(blocks, blocks(row, i), blocks(row, i + spacesEmpty))

'Animation Code

'  Do While (counter <= SpacesEmpty)  'This is for animations
' moveBlockToSpace(blocks, blocks(row, i + (counter - 1)), blocks(row, i + counter))
' counter += 1
' Loop
'  animatedBlock = blocks(row, i)
'  directionOfMotion = "Right"
' animateNumOfEmptySpaces = SpacesEmpty
' counter = 1
'      Timer1.Start()

'           End If

'         End If
'    Next

'End Sub

