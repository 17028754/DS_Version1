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
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafx.scene.control.Alert.AlertType

@sfxml
class MainWindowController(private val txtName: TextField, private var clientRef: ActorRef[ChatClient.Command], private var clientName: String,
private val lblStatus: Label, private val listUser: ListView[User], private val listRules: ListView[User],
private val listMessage: ListView[String],
private val txtMessage: TextField) {


    var chatClientRef: Option[ActorRef[ChatClient.Command]] = None

    val receivedText: ObservableBuffer[String] =  new ObservableBuffer[String]()

    listMessage.items = receivedText

    def handleJoin(action: ActionEvent): Unit = {
        if(txtName != null)
          chatClientRef map (_ ! ChatClient.StartJoin(txtName.text()))
    }

    def displayStatus(text: String): Unit = {
        lblStatus.text = text
    }
  def updateList(x: Iterable[User]): Unit ={
    listUser.items = new ObservableBuffer[User]() ++= x
  }

//  // Join Game (NOT LOBBY) BUTTON
//  def handleSend(actionEvent: ActionEvent): Unit ={
//
//    // Join Game (NOT LOBBY) function
//    if (listUser.selectionModel().selectedIndex.value >= 0){
//      Client.userRef ! ChatClient.SendMessageL(listUser.selectionModel().selectedItem.value.ref,
//        txtMessage.text())
//    }
//  }


  // Create game
  def handleCreateGame(actionEvent: ActionEvent): Unit = {
    // In case user selected own username or that the list was empty
    if (listUser.selectionModel().selectedIndex.value < 0
      || listUser.selectionModel().selectedItem.value.ref == Client.userRef){

      // warning dialog
      new Alert(AlertType.Warning) {
        initOwner(stage)
        title = "Warning Dialog"
        headerText = "Unable to Start Game!"
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

        // Reject invitation
      case _ => Client.userRef ! ChatClient.RejectInvitation(actorRef)
    }
  }

  def displayInvitationResult(result: Boolean): Unit = {
    if(result == true){
      chatClientRef map (_ ! ChatClient.GameOmission(clientName, clientRef, txtName.text.value, Client.userRef))
//      // Store opponent client ref in object for game controller to reference
//      // Store additional component for game omission in the server
//      ClientRef.clientRef = ClientRef.toOption(clientRef)
//      ClientRef.clientName = clientName
//      ClientRef.ownRef = ClientRef.toOption(Client.userRef)
//      ClientRef.ownName = txtName.text.value
//      ClientRef.serverRef = chatClientRef
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



//  // Testing for client ref
//  def debug(actionEvent: ActionEvent): Unit = {
//    if (listUser.selectionModel().selectedIndex.value >= 0){
//      Client.userRef ! ChatClient.SendMessageL(clientRef, "Debug part")
//    }
//  }

  // Testing to start game, launch fxml on both clients
  def startGame(actionEvent: ActionEvent): Unit = {
    Client.userRef ! ChatClient.StartGame(clientRef)
  }

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


  def addText(text: String): Unit = {
      receivedText += text
  }

}