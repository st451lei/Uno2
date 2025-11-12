package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.Controller
import de.htwg.se.uno2.model._
import scala.io.StdIn.readLine
import scala.util.Try

class Tui(controller: Controller):

  def run(): Unit =
    controller.startGame()
    loop()

  private def loop(): Unit =
    println(controller.gameStateToString)
    print("> ")
    val input = readLine().trim

    input match
      case "quit" =>
        println("Spiel beendet.")
        return
      case "draw" =>
        controller.drawCard()
      case s if s.startsWith("play") =>
        val parts = s.split(" ")
        if parts.length == 2 then
          Try(parts(1).toInt).toOption match
            case Some(index) => controller.playCard(index)
            case None => println("Ungültiger Index.")
        else
          println("Verwendung: play <index>")
      case _ =>
        println("Unbekannter Befehl. (play <index>, draw, quit)")

    loop()