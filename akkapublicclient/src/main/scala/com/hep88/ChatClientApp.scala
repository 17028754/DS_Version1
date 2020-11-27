package com.hep88
import java.net.URL

import akka.cluster.typed._
import akka.{actor => classic}
import akka.discovery.{Discovery, Lookup, ServiceDiscovery}
import akka.discovery.ServiceDiscovery.Resolved
import akka.actor.typed.{ActorRef, ActorSystem}
import akka.actor.typed.scaladsl.adapter._
import akka.stream.Client
import com.hep88.ChatClient.getClass
import com.hep88.Client.{getClass, loader, mainSystem, stage}
import com.typesafe.config.ConfigFactory
import scalafx.application.JFXApp
import scalafx.application.JFXApp.PrimaryStage
import scalafx.scene.Scene
import scalafxml.core.{FXMLLoader, NoDependencyResolver}
import scalafx.Includes._
import scalafx.scene.layout.BorderPane

import scala.concurrent.Future
import scala.concurrent.duration._


object Client extends JFXApp {
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global
    val config = ConfigFactory.load()
   val mainSystem = akka.actor.ActorSystem("HelloSystem", MyConfiguration.askDevConfig().withFallback(config))
  val greeterMain: ActorSystem[Nothing] = mainSystem.toTyped

  val cluster = Cluster(greeterMain)

  val discovery: ServiceDiscovery = Discovery(mainSystem).discovery

 val userRef = mainSystem.spawn(ChatClient(), "ChatClient")

  // To join internet
 def joinPublicSeedNode(): Unit = {
    val lookup: Future[Resolved] =
     discovery.lookup(Lookup("wm.hep88.com").withPortName("hellosystem").withProtocol("tcp"), 1.second)

    lookup.foreach (x => {
        val result = x.addresses
        result map { x =>
            val address = akka.actor.Address("akka", "HelloSystem", x.host, x.port.get)
            cluster.manager ! JoinSeedNodes(List(address))
        }
    })
 }

  // To join local network
 def joinLocalSeedNode(): Unit = {
    val address = akka.actor.Address("akka", "HelloSystem", MyConfiguration.localAddress.get.getHostAddress, 2222)
    cluster.manager ! JoinSeedNodes(List(address))
 }
  joinLocalSeedNode()

  // fxml loader (UI)
  val loader = new FXMLLoader(null, NoDependencyResolver)
  loader.load(getClass.getResourceAsStream("view/MainWindowTest.fxml"))
  val border: scalafx.scene.layout.BorderPane = loader.getRoot[javafx.scene.layout.BorderPane]()
  val control = loader.getController[com.hep88.view.MainWindowController#Controller]()
  control.chatClientRef = Option(userRef)
  stage = new PrimaryStage() {
    scene = new Scene(){
      root = border
    }
  }

  stage.onCloseRequest = handle( {
    mainSystem.terminate
  })


}