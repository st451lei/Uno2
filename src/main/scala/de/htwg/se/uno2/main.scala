package de.htwg.se.uno2

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.controller.Controller
import de.htwg.se.uno2.aview.Tui

@main def runUnoTui(): Unit =
  val controller = Controller(ColorOnlyGameStateFactory)
  val tui = Tui(controller)
  tui.run()