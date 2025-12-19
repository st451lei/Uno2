package de.htwg.se.uno2

import de.htwg.se.uno2.controller.api.ControllerInterface
import de.htwg.se.uno2.controller.impl.Controller
import de.htwg.se.uno2.aview.{Tui, GUI}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@main def run(): Unit =
  val controller: ControllerInterface = new Controller()
  val tui = Tui(controller)
  val gui = new GUI(controller)
  
  controller.addObserver(tui)
  controller.addObserver(gui)
  
  Future {
    tui.run()
  }