package de.htwg.se.uno2

@main def runUnoTui(): Unit =
  val controller = Controller()
  val tui = Tui(controller)
  tui.run()