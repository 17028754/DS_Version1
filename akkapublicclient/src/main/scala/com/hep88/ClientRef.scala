package com.hep88
import akka.actor.typed.ActorRef

object ClientRef {

  def toOption(value: ActorRef[ChatClient.Command]): Option[ActorRef[ChatClient.Command]] = {
    Option(value)
  }

  var clientRef: Option[ActorRef[ChatClient.Command]] = None
  var clientName: String = ""

  var ownRef: Option[ActorRef[ChatClient.Command]] = None
  var ownName: String = ""

  var serverRef: Option[ActorRef[ChatClient.Command]] = None

}
