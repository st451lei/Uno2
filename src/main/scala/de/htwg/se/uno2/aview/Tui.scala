package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.Controller
import de.htwg.se.uno2.util.Observer
import scala.io.StdIn.readLine
import scala.util.Try

class Tui(controller: Controller) extends Observer:
  // подписываемся на обновления контроллера
  controller.addObserver(this)

  // вызывается при каждом notifyObservers()
  def update: Unit =
    println(controller.gameStateToString)

  // точка запуска TUI
  def run(): Unit =
    val names = askPlayers()
    controller.startGame(names)
    loop()

  // основной REPL-цикл
  private def loop(): Unit =
    print("> ")
    val input = Option(readLine()).getOrElse("").trim
    if controller.isAwaitingColorChoise then
      input.toLowerCase match
        case s if s.startsWith("color") =>
          val parts = s.split("\\s+")
          if parts.length >= 2 then  controller.chooseColor(parts(1))
          else println("Verwendung: color r|y|g|b")
          loop()
        case "quit" =>
          println("Spiel beendet."); ()
        case _ =>
          controller.chooseColor("")
          loop()
    else
      input match
        case "quit" =>
          println("Spiel beendet."); ()// завершаем
        case "draw" =>
          controller.drawCard(); loop()
        case s if s.startsWith("play") =>
          val parts = s.split("\\s+")
          if parts.length == 2 then
            Try(parts(1).toInt).toOption match
              case Some(index) => controller.playCard(index)
              case None        => println("Ungültiger Index.")
          else
            println("Verwendung: play <index>")
          loop()
        case s if s.startsWith("color") =>
          println("Jetzt keine Farbauswahl nötig.")
          loop()
        case _ =>
          println("Unbekannter Befehl. (play <index>, draw, quit)")
          loop()

  private def askPlayers(): Seq[String] =
    println("Spieler eingeben, z.B. 'Ann Ben':")
    val line  = Option(readLine()).getOrElse("")
    val names = line.split("[ ,]+").toVector.filter(_.nonEmpty)
    if names.nonEmpty then names else Vector("Player1", "Player2")
