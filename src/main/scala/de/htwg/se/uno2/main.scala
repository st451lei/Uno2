package de.htwg.se.uno2

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.controller.Controller
import de.htwg.se.uno2.aview.{Tui, GUI}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

@main def run(): Unit =
  val controller = Controller()
  val tui = Tui(controller)
  val gui = new GUI(controller)
  
  controller.addObserver(tui)
  controller.addObserver(gui)
  
  Future {
    tui.run()
  }