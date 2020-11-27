package com.hep88
import scalafxml.core.{FXMLLoader, NoDependencyResolver}

object Client1 {

  val resource = getClass.getResource("view/Board.fxml")
  val loader = new FXMLLoader(resource, NoDependencyResolver)
  loader.load()
  val roots = loader.getRoot[javafx.scene.layout.AnchorPane]
  Client.border.setCenter(roots)
  val control = loader.getController[com.hep88.view.GaiaGameController#Controller]()

}