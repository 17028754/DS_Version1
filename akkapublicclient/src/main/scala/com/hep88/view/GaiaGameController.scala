package com.hep88.view

import akka.actor.typed.ActorRef
import com.hep88.{ChatClient, Client, ClientRef}
import com.hep88.model.{GaiaGame, ScalaFXSound}
import scalafx.Includes._
import scalafx.animation._
import scalafx.application.Platform
import scalafx.event.ActionEvent
import scalafx.scene.control.Alert.AlertType
import scalafx.scene.control.{Alert, Label, TextField}
import scalafx.scene.input.{KeyCode, KeyEvent}
import scalafx.scene.layout.{AnchorPane, GridPane}
import scalafx.scene.shape.Rectangle
import scalafxml.core.macros.sfxml

@sfxml
class GaiaGameController(
                          tetris: AnchorPane,
                          tetrisBoard: GridPane,
                          enemytetrisBoard: GridPane,
                          nextPieceBoard: GridPane,
                          enemyNextPieceBoard: GridPane,
                          score: Label,
                          showPaused: Label,
                          enemyScore: Label
                        ) extends ScalaFXSound {

  var game = new GaiaGame
  var gameOver = false
  var rectangles: List[List[Rectangle]] = List()
  var nextPieceRectangles: List[List[Rectangle]] = List()
  var enemyNextPieceRectangles: List[List[Rectangle]] = List()
  var enemyNextPiece: List[List[Array[Int]]] = List()
  var enemyRectangles: List[List[Rectangle]] = List()
  var test: Unit = None
  // control
  var pause = false
  var leftPressed = false
  var rightPressed = false
  var upPressed = false
  var downPressed = false
  var enterPressed = false
  // for animationTimer
  var time = 0L

  var clientRef: ActorRef[ChatClient.Command] = ClientRef.clientRef.get
  var clientName: String = ClientRef.clientName
  var ownRef: ActorRef[ChatClient.Command] = ClientRef.ownRef.get
  var ownName: String = ClientRef.ownName
  var serverRef: Option[ActorRef[ChatClient.Command]] = ClientRef.serverRef
  var gameOverOpponent = false
  var opponentScore = 0

  //  // if pressed the rules, pause the game and show the rules
  //  def handleGaiaRules(action: ActionEvent): Unit = {
  //    if (!pause && !gameOver) {
  //      timer.stop
  //      showPaused.setText("Game Paused")
  //      pause = true
  //      MainApp.showGaiaRules()
  //      timer.start
  //      showPaused.setText("")
  //      pause = false
  //    }
  //    else {
  //      // else create a new game straight
  //      if (!pause && gameOver) {
  //        timer.stop
  //        MainApp.showGaiaRules()
  //        MainApp.showMain()
  //      }
  //    }
  //  }



  // if press back button, go back to Main
  // Things to do here:
  // 1. Cannot leave in the middle of the game
  // 2. Can only leave and go back to main lobby when both player ends
  def handleBack(action: ActionEvent): Unit = {
    timer.stop
    pause = true
    Client
  }

  def omissionOccured(): Unit = {
    timer.stop
    pause = true
    val alert = new Alert(AlertType.Information){
      title = "Game over"
      headerText = "Enemy left the game."
      contentText = "Your score: " + game.scores.toString + "\nOpponent score: " + opponentScore.toString
    }
    Platform.runLater(alert.showAndWait())
    gameOverSoundEffect()
  }

  // create every single rectangles for OWN tetrisBoard
  for (row <- 0 until game.gameBoard.rows) {
    var tmpRec: List[Rectangle] = List()
    for (col <- 0 until game.gameBoard.columns) {
      if (col == 0 || col == game.gameBoard.columns - 1) {
        //sides pink
        tmpRec = tmpRec :+ new Rectangle {
          width = 27
          height = 27
          fill = "pink"
        }
      }
      else if (row >= game.gameBoard.rows - 3 && row <= game.gameBoard.rows - 1) {
        //bottom 3 rows
        if (col >= game.gameBoard.columns - 7 && col <= game.gameBoard.columns - 4) {
          //tree branch
          tmpRec = tmpRec :+ new Rectangle {
            width = 27
            height = 27
            fill= "brown"
          }
        }
        else if (row == game.gameBoard.rows - 1) {
          if (col < 3 || col > 6) {
            //bottom pink
            tmpRec = tmpRec :+ new Rectangle {
              width = 27
              height = 27
              fill = "pink"
            }
          }
        }
        else {
          tmpRec = tmpRec :+ new Rectangle {
            //white for last 3 bottom rows
            width = 27
            height = 27
            fill ="white"
          }
        }
      }
      else if (row == 0) {
        //at the top most pink
        tmpRec = tmpRec :+ new Rectangle {
          width = 27
          height = 27
          fill = "pink"
        }
      }
      else {
        tmpRec = tmpRec :+ new Rectangle {
          //white
          width = 27
          height = 27
          fill = "white"
        }
      }
      tetrisBoard.add(tmpRec(col), col, row)
    }
    rectangles = rectangles ++: List(tmpRec)
  }

  // create every single rectangles for ENEMY tetrisBoard
  for (row <- 0 until game.gameBoard.rows) {
    var tmpRec: List[Rectangle] = List()
    for (col <- 0 until game.gameBoard.columns) {
      if (col == 0 || col == game.gameBoard.columns - 1) {
        //sides pink
        tmpRec = tmpRec :+ new Rectangle {
          width = 27
          height = 27
          fill = "pink"
        }
      }
      else if (row >= game.gameBoard.rows - 3 && row <= game.gameBoard.rows - 1) {
        //bottom 3 rows
        if (col >= game.gameBoard.columns - 7 && col <= game.gameBoard.columns - 4) {
          //tree branch
          tmpRec = tmpRec :+ new Rectangle {
            width = 27
            height = 27
            fill= "brown"
          }
        }
        else if (row == game.gameBoard.rows - 1) {
          if (col < 3 || col > 6) {
            //bottom pink
            tmpRec = tmpRec :+ new Rectangle {
              width = 27
              height = 27
              fill = "pink"
            }
          }
        }
        else {
          tmpRec = tmpRec :+ new Rectangle {
            //white for last 3 bottom rows
            width = 27
            height = 27
            fill ="white"
          }
        }
      }
      else if (row == 0) {
        //at the top most pink
        tmpRec = tmpRec :+ new Rectangle {
          width = 27
          height = 27
          fill = "pink"
        }
      }
      else {
        tmpRec = tmpRec :+ new Rectangle {
          //white
          width = 27
          height = 27
          fill = "white"
        }
      }
      enemytetrisBoard.add(tmpRec(col), col, row)
    }
    enemyRectangles = enemyRectangles ++: List(tmpRec)
  }


  // create every single rectangles for OWN nextPieceBoard
  for (row <- 0 until 4) {
    var tmpRec: List[Rectangle] = List()
    for (col <- 0 until 4) {
      tmpRec = tmpRec :+ new Rectangle {
        width = 27
        height = 27
        fill = "white"
        opacity = 0.5
      }
      nextPieceBoard.add(tmpRec(col), col, row)
    }
    nextPieceRectangles = nextPieceRectangles ++: List(tmpRec)
  }

  // create every single rectangles for ENEMY nextPieceBoard
  for (row <- 0 until 4) {
    var tmpRec: List[Rectangle] = List()
    for (col <- 0 until 4) {
      tmpRec = tmpRec :+ new Rectangle {
        width = 27
        height = 27
        fill = "white"
        opacity = 0.5
      }
      enemyNextPieceBoard.add(tmpRec(col), col, row)
    }
    enemyNextPieceRectangles = enemyNextPieceRectangles ++: List(tmpRec)
  }

  def receiveEnemyNextPiece(nextPieceRectangles: List[List[Array[Int]]]): Unit ={
    enemyNextPiece = nextPieceRectangles
    if (enemyNextPiece != Nil){
      for (a <- enemyNextPiece.head.indices) {
        enemyNextPieceRectangles(enemyNextPiece.head(a)(1))(enemyNextPiece.head(a)(0) + 1).fill = "red"
      }
    }
  }

  def clearEnemyNextPiece(): Unit = {
    if (enemyNextPiece != Nil) {
      for (a <- enemyNextPiece.head.indices) {
        enemyNextPieceRectangles(enemyNextPiece.head(a)(1))(enemyNextPiece.head(a)(0)+1).fill = "white"
      }
    }
  }

  //  Event Handler
  //  Controls
  tetris.onKeyPressed = (e: KeyEvent) => {
    if (e.code == KeyCode.Left) leftPressed = true
    if (e.code == KeyCode.Right) rightPressed = true
    if (e.code == KeyCode.Up) upPressed = true
    if (e.code == KeyCode.Down) downPressed = true
    if (e.code == KeyCode.Enter) enterPressed = true
    if (e.code == KeyCode.P) {
      // Pause game and tell opponent to pause also
      if (!pause && !gameOver) {
        Client.userRef ! ChatClient.OwnPause(clientRef)
        pauseSoundEffect()
        timer.stop
        showPaused.setText("Game Paused!")
        pause = true
      }
      // Unpause game and tell opponent to unpause also
      else {
        if (pause && !gameOver) {
          Client.userRef ! ChatClient.OwnUnpause(clientRef)
          pauseSoundEffect()
          timer.start
          showPaused.setText("")
          pause = false
        }
      }
    }
  }

  // pause invoked by opponent
  def pauseFromOther(): Unit = {
    pauseSoundEffect()
    timer.stop
    showPaused.setText("Your opponent paused the game.")
  }

  // unpause invoked by opponent
  def unpauseFromOther(): Unit = {
    pauseSoundEffect()
    timer.start
    showPaused.setText("")
  }

  def refreshBoard(): Unit = {
    for (row <- 0 until game.gameBoard.rows) {
      for (col <- 0 until game.gameBoard.columns) {
        // if it occupy, fill the right colour, otherwise make it white
        if (game.gameBoard.board(row)(col) == 1) {
          if (row >= game.gameBoard.rows - 3 && row <= game.gameBoard.rows - 1) {
            if (col >= game.gameBoard.columns - 7 && col <= game.gameBoard.columns - 4)
              rectangles(row)(col).fill = "brown"
            else
              rectangles(row)(col).fill = "lightgreen"
          }
          else if (row > 0)
            rectangles(row)(col).fill = "lightgreen"
        }
        // for branches
        else if (game.gameBoard.board(row)(col) == 2)
          rectangles(row)(col).fill = "brown"
        else {
          if (row == 0) {
            // at the very top filled with pink
            rectangles(row)(col).fill = "pink"
          }
          else if (col == 0 || col == game.gameBoard.columns - 1) {
            // at both leftmost and rightmost filled with pink
            rectangles(row)(col).fill = "pink"
          }
          else if (row == game.gameBoard.rows - 1) {
            // at the very bottom fill the row with pink except tree branch
            if (col < game.gameBoard.columns - 7 || col > game.gameBoard.columns - 4)
              rectangles(row)(col).fill = "pink"
          }
          else
            rectangles(row)(col).fill = "white"
        }
      }
    }
  }

  def paintPieceToBoard(piece: List[Array[Int]], currentX: Int, currentY: Int): Unit = {
    //paint board black blue according to pieces
    for (a <- piece.indices) {
      rectangles(piece(a)(1)+currentY)(piece(a)(0)+currentX).fill = "blue"
    }
  }

  def clearPieceFromBoard(piece: List[Array[Int]], currentX: Int, currentY: Int): Unit = {
    // paint the board back to white according to the piece
    for (a <- piece.indices) {
      rectangles(piece(a)(1) + currentY)(piece(a)(0) + currentX).fill = "white"
    }
    refreshBoard()
  }

  def printEnemyCurrentPiece1(nextPieceRectangles: List[Array[Int]], currentX: Int, currentY: Int): Unit = {
    for (a <- nextPieceRectangles.head.indices) {
      enemyRectangles(nextPieceRectangles(a)(1)+currentY)(nextPieceRectangles(a)(0)+currentX).fill = "blue"
    }
  }

  def paintEnemyPiece(nextPieceRectangles: List[Array[Int]], currentX: Int, currentY: Int): Unit = {
    for (a <- nextPieceRectangles.indices) {
      enemyRectangles(nextPieceRectangles(a)(1)+currentY)(nextPieceRectangles(a)(0)+currentX).fill = "blue"
    }
  }

  def clearEnemyPiece(nextPieceRectangles: List[Array[Int]], currentX: Int, currentY: Int, enemy: Array[Array[Int]]): Unit ={
    for (a <- nextPieceRectangles.indices) {
      enemyRectangles(nextPieceRectangles(a)(1) + currentY)(nextPieceRectangles(a)(0) + currentX).fill = "white"
    }
    refreshEnemyBoard(enemy)
  }

  def refreshEnemyBoard(enemy: Array[Array[Int]]): Unit = {
    for (row <- 0 until game.gameBoard.rows) {
      for (col <- 0 until game.gameBoard.columns) {
        // if it occupy, fill the right colour, otherwise make it white
        if (enemy(row)(col) == 1) {
          if (row >= game.gameBoard.rows - 3 && row <= game.gameBoard.rows - 1) {
            if (col >= game.gameBoard.columns - 7 && col <= game.gameBoard.columns - 4)
              enemyRectangles(row)(col).fill = "brown"
            else
              enemyRectangles(row)(col).fill = "lightgreen"
          }
          else if (row > 0)
            enemyRectangles(row)(col).fill = "lightgreen"
        }
        // for branches
        else if (enemy(row)(col) == 2)
          enemyRectangles(row)(col).fill = "brown"
        else {
          if (row == 0) {
            // at the very top filled with pink
            enemyRectangles(row)(col).fill = "pink"
          }
          else if (col == 0 || col == game.gameBoard.columns - 1) {
            // at both leftmost and rightmost filled with pink
            enemyRectangles(row)(col).fill = "pink"
          }
          else if (row == game.gameBoard.rows - 1) {
            // at the very bottom fill the row with pink except tree branch
            if (col < game.gameBoard.columns - 7 || col > game.gameBoard.columns - 4)
              enemyRectangles(row)(col).fill = "pink"
          }
          else
            enemyRectangles(row)(col).fill = "white"
        }
      }
    }
  }


  // update own score and notify enemy current's score
  def updateScore(): Unit = {
    var scores = game.scores
    score.setText(scores.toString)
    Client.userRef ! ChatClient.PutScore(clientRef, scores.toString)
  }

  // Update enemy's score
  def addScore(text: String): Unit = {
    enemyScore.setText(text)
  }

  // Update enemy's game over status
  def updateGameStatus(score: Int, status: Boolean): Unit = {
    gameOverOpponent = status
    opponentScore = score
  }

  def gameOverFinal(score: Int): Unit = {
    opponentScore = score
    serverRef map (_ ! ChatClient.GameCompleted(clientName, clientRef, ownName, ownRef))
    if (game.scores > opponentScore){
      val alert = new Alert(AlertType.Information){
        title = "Game over"
        headerText = "You Won!"
        contentText = "Your score: " + game.scores.toString + "\nOpponent score: " + opponentScore.toString
      }
      Platform.runLater(alert.showAndWait())
      gameOverSoundEffect()
    }else if (game.scores < opponentScore){
      val alert = new Alert(AlertType.Information){
        title = "Game over"
        headerText = "You Lost!"
        contentText = "Your score: " + game.scores.toString + "\nOpponent score: " + opponentScore.toString
      }
      Platform.runLater(alert.showAndWait())
      gameOverSoundEffect()
    }
  }

  //animationTimer
  val timer: AnimationTimer = AnimationTimer(t => {

    // if nextPiece is empty, get new piece from randomPiece
    if (game.nextPiece.isEmpty) {
      game.nextPiece = game.randomPiece()
      Client.userRef ! ChatClient.TellNextPiece(clientRef, game.nextPiece)
      for (a <- game.nextPiece.head.indices) {
        nextPieceRectangles(game.nextPiece.head(a)(1))(game.nextPiece.head(a)(0)+1).fill = "red"
      }
    }

    // if currentPiece is empty, get new one
    if (game.currentPiece.isEmpty) {
      var tmpPiece = game.nextPiece
      game.currentTetrad = tmpPiece
      for (a <- game.nextPiece.head.indices) {
        nextPieceRectangles(game.nextPiece.head(a)(1))(game.nextPiece.head(a)(0)+1).fill = "white"
      }
      Client.userRef ! ChatClient.ClearNextPiece(clientRef)
      game.nextPiece = List()
      game.tetraminoes.currentZ = 0
      game.tetraminoes.currentX = 4
      game.tetraminoes.currentY = 0
      game.currentPiece = game.currentTetrad.head
      if (game.checkGameOver()) {
        gameOver = true
        timer.stop
        showPaused.setText("Your game has ended,\nplease wait for your opponent to finish.")
        Client.userRef ! ChatClient.SendGameOver(clientRef, gameOver, game.scores)
      }

      // End game after both players reach the termination criteria
      if (gameOver == true && gameOverOpponent == true){
        if (game.scores > opponentScore){
          timer.stop
          val alert = new Alert(AlertType.Information){
            title = "Game over"
            headerText = "You Won!"
            contentText = "Your score: " + game.scores.toString + "\nOpponent score: " + opponentScore.toString
          }
          Platform.runLater(alert.showAndWait())
          gameOverSoundEffect()
        }else if (game.scores < opponentScore){
          timer.stop
          val alert = new Alert(AlertType.Information){
            title = "Game over"
            headerText = "You Lost!"
            contentText = "Your score: " + game.scores.toString + "\nOpponent score: " + opponentScore.toString
          }
          Platform.runLater(alert.showAndWait())
          gameOverSoundEffect()
        }
        showPaused.setText("Game Over!")
        Client.userRef ! ChatClient.SendGameOverLast(clientRef, game.scores)
      }



      for (a <- game.currentPiece.indices) {
        rectangles(game.currentPiece(a)(1)+game.tetraminoes.currentY)(game.currentPiece(a)(0)+game.tetraminoes.currentX).fill = "blue"
      }
      Client.userRef ! ChatClient.CurrentBoardPiece(clientRef, game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
    }

    if (leftPressed) {
      game.moveSet(-1,0)
      refreshBoard()
      paintPieceToBoard(game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
      leftPressed = false
      Client.userRef ! ChatClient.RefreshEnemyBoard(clientRef, game.gameBoard.board)
      Client.userRef ! ChatClient.PaintEnemyBoardPiece(clientRef, game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
    }
    if (rightPressed) {
      // go right
      game.moveSet(1,0)
      refreshBoard()
      paintPieceToBoard(game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
      rightPressed = false
      Client.userRef ! ChatClient.RefreshEnemyBoard(clientRef, game.gameBoard.board)
      Client.userRef ! ChatClient.PaintEnemyBoardPiece(clientRef, game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
    }
    if (upPressed) {
      // rotate
      if (game.isRotatable()) {
        rotateSoundEffect()
        clearPieceFromBoard(game.currentPiece,game.tetraminoes.currentX,game.tetraminoes.currentY)
        game.currentPiece = game.tetraminoes.rotate(game.currentTetrad, game.tetraminoes.currentZ)
        paintPieceToBoard(game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
        Client.userRef ! ChatClient.ClearEnemyBoardPiece(clientRef, game.currentPiece,game.tetraminoes.currentX,game.tetraminoes.currentY, game.gameBoard.board)
        Client.userRef ! ChatClient.PaintEnemyBoardPiece(clientRef, game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
      }
      else
        collisionSoundEffect()
      upPressed = false
    }
    if (downPressed) {
      game.moveSet(0,1)
      refreshBoard()
      paintPieceToBoard(game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
      downPressed = false
      Client.userRef ! ChatClient.RefreshEnemyBoard(clientRef, game.gameBoard.board)
      Client.userRef ! ChatClient.PaintEnemyBoardPiece(clientRef, game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
    }
    if (enterPressed) {
      do {
        game.moveSet(0,1)
        refreshBoard()
        paintPieceToBoard(game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
        Client.userRef ! ChatClient.RefreshEnemyBoard(clientRef, game.gameBoard.board)
        Client.userRef ! ChatClient.PaintEnemyBoardPiece(clientRef, game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
      } while (game.currentPiece.nonEmpty)
      enterPressed = false
    }

    // make the body of this if statement to run every second
    if ((t - time) > 1e+9) {
      game.moveSet(0,1)
      updateScore()
      refreshBoard()
      paintPieceToBoard(game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
      time = t
      Client.userRef ! ChatClient.RefreshEnemyBoard(clientRef, game.gameBoard.board)
      Client.userRef ! ChatClient.PaintEnemyBoardPiece(clientRef, game.currentPiece, game.tetraminoes.currentX, game.tetraminoes.currentY)
    }
  })

  timer.start



}
