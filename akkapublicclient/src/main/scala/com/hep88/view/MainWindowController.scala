package com.hep88.view
import akka.actor.Actor
import akka.actor.typed.ActorRef
import com.hep88.Client.stage
import com.hep88.ClientRef
import scalafxml.core.macros.sfxml
import scalafx.event.ActionEvent
import scalafx.scene.control.{Alert, ButtonType, Label, ListView, TextField}
import com.hep88.{ChatClient, Client, Client1, User}
import scalafx.collections.ObservableBuffer
import scalafx.Includes._
import scalafx.scene.control.Alert.AlertType

@sfxml
class MainWindowController(private val txtName: TextField, private var clientRef: ActorRef[ChatClient.Command], private var clientName: String,
private val lblStatus: Label, private val listUser: ListView[User], private val listRules: ListView[User],
private val listGameRoom: ListView[User]) {

    var invGame: Boolean = true

    var startGame: Boolean = false

    var chatClientRef: Option[ActorRef[ChatClient.Command]] = None

    val receivedText: ObservableBuffer[String] =  new ObservableBuffer[String]()

    val membersInGame = new ObservableBuffer[User]()

    def handleJoin(action: ActionEvent): Unit = {
        if(txtName != null) {
          chatClientRef map (_ ! ChatClient.StartJoin(txtName.text()))
        }
    }

    def displayStatus(text: String): Unit = {
        lblStatus.text = text
    }
  def updateList(x: Iterable[User]): Unit ={
    listUser.items = new ObservableBuffer[User]() ++= x
  }

  def displayLeaveGameRoom(): Unit = {
    invGame = true
    startGame = false
    new Alert(AlertType.Information) {
      initOwner(stage)
      title = "Information Dialog"
      headerText = "A player has left the game room!"
      contentText = "Please invite a player to start the game after your invitation is accepted."
    }.showAndWait()
  }

  def handleLeaveGameRoom(action: ActionEvent): Unit = {
    if (invGame == true){
      new Alert(AlertType.Warning) {
        initOwner(stage)
        title = "Warning Dialog"
        headerText = "You are not in a game room!"
        contentText = "Leave button only allows you to leave the game room\nthat you have joined after accepting the invitation."
      }.showAndWait()
    }
    else{
      invGame = true
      startGame = false
      membersInGame.clear()
      Client.userRef ! ChatClient.LeaveGameRoomList(clientRef, membersInGame.toList)
    }
  }

  // Special case to take note:
  // 2. Implement a leave game feature, and update the list
  // Create game
  def handleCreateGame(actionEvent: ActionEvent): Unit = {
    // Don't let user create/invite another player when user is already in a game room with another player
    if (invGame == false){
      new Alert(AlertType.Warning) {
        initOwner(stage)
        title = "Warning Dialog"
        headerText = "You are already in a game room!"
        contentText = "Please leave current game room to invite the player you want to play with!\nMax participant per game room: 2"
      }.showAndWait()
    }
    else{
      // In case user selected own username or that the list was empty
      if (listUser.selectionModel().selectedIndex.value < 0
        || listUser.selectionModel().selectedItem.value.ref == Client.userRef){

        // warning dialog
        new Alert(AlertType.Warning) {
          initOwner(stage)
          title = "Warning Dialog"
          headerText = "Unable to Invite to Game!"
          contentText = "Please choose other player's username!"
        }.showAndWait()
      }
      else {
        // Store information for omission during game, and to notify client in cluster about game invitation
        clientRef = listUser.selectionModel().selectedItem.value.ref
        clientName = listUser.selectionModel().selectedItem.value.name
        // Send invitation to client in cluster
        Client.userRef ! ChatClient.SendInvitation(clientRef, txtName.text.value)
        // Need to notify all the clients, call the function in ChatClient object that updates all the clients about a new game created
        // At the moment, notify one selected client

        // notification dialog
        new Alert(AlertType.Information){
          initOwner(stage)
          title = "Information Dialog"
          headerText = "Invitation sent!"
          contentText = "Please wait for " + clientName + " to accept or reject your invite, you will be notified accordingly."
        }.showAndWait()
      }
    }
  }

  // Receive invitation
  def receiveInvitation(name: String, actorRef: ActorRef[ChatClient.Command]): Unit = {

    val alert = new Alert(AlertType.Confirmation){
      initOwner(stage)
      title = "Confirmation Dialog"
      headerText = "Invitation received from " + name + "!"
      contentText = "Do you want to join the game?"
    }

    // display the confirmation dialog box
    val result = alert.showAndWait()

    // Match the receiver's choice
    result match {
        // Accept invitation
      case Some(ButtonType.OK) => ClientRef.clientRef = ClientRef.toOption(actorRef)
                                  Client.userRef ! ChatClient.AcceptInvitation(actorRef)
                                  clientRef = actorRef
                                  clientName = name
                                  invGame = false

        // Reject invitation
      case _ => Client.userRef ! ChatClient.RejectInvitation(actorRef)
    }
  }

  def displayInvitationResult(result: Boolean): Unit = {
    if(result == true){
//      chatClientRef map (_ ! ChatClient.GameOmission(clientName, clientRef, txtName.text.value, Client.userRef))
      invGame = false
      startGame = true
      membersInGame += User(clientName, clientRef)
      membersInGame += User(txtName.text.value, Client.userRef)
      Client.userRef ! ChatClient.UpdateGameRoomList(clientRef, membersInGame.toList)
      new Alert(AlertType.Information){
        initOwner(stage)
        title = "Information Dialog"
        headerText = "Invitation accepted!"
        contentText = "Press Start Game to begin the game."
      }.showAndWait()
    }else{
      new Alert(AlertType.Information){
        initOwner(stage)
        title = "Information Dialog"
        headerText = "Invitation rejected."
        contentText = "Please invite another player to start the game."
      }.showAndWait()
    }
  }

  def updateGameRoom(x: Iterable[User]): Unit ={
    listGameRoom.items = new ObservableBuffer[User]() ++= x
  }


  // Initiate game to launch for both clients
  def startGame(actionEvent: ActionEvent): Unit = {
    if (startGame == false) {
      new Alert(AlertType.Warning) {
        initOwner(stage)
        title = "Warning Dialog"
        headerText = "Unable to Start Game!"
        contentText = "Please invite other player to be eligible to start game!\nNote: Only people who send invitation can start the game."
      }.showAndWait()
    }else{
      chatClientRef map (_ ! ChatClient.GameOmission(clientName, clientRef, txtName.text.value, Client.userRef))
      Client.userRef ! ChatClient.StartGame(clientRef)
    }
  }

  // Load the game fxml and store the appropriate information for game omission/communication
  def loadGame(): Unit = {
    // Store opponent client ref in object for game controller to reference
    // Store additional component for game omission in the server
    ClientRef.clientRef = ClientRef.toOption(clientRef)
    ClientRef.clientName = clientName
    ClientRef.ownRef = ClientRef.toOption(Client.userRef)
    ClientRef.ownName = txtName.text.value
    ClientRef.serverRef = chatClientRef
    Client1
  }

}