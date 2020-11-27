package com.hep88.view

import akka.actor.typed.ActorRef
import com.hep88.{ChatClient, User}
import scalafx.collections.ObservableBuffer
import scalafx.event.ActionEvent
import scalafx.scene.control.{Label, ListView, TextField}
import scalafxml.core.macros.sfxml

@sfxml
class LandingPageController(private val txtName: TextField){

  var chatClientRef: Option[ActorRef[ChatClient.Command]] = None


  def handleJoin(action: ActionEvent): Unit = {
    if(txtName != null)
      chatClientRef map (_ ! ChatClient.StartJoin(txtName.text()))
  }

}
