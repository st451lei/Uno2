package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.ControllerInterface
import scala.util.Try

class Tui(controller: ControllerInterface) extends BaseTui(controller):
  private var exit = false
  
  override def update: Unit =
    println()
    println(controller.gameStateToString)
    println()
    if controller.isGameOver then
      println("Spiel beendet.")
      exit = true
    else
     println("Befehle: play <index>, draw, color <r|y|g|b>, save, load, undo, redo, quit")
    
  protected def shouldExit: Boolean = exit

  protected def handleQuit(): Unit =
    println("Spiel beendet")
    exit = true

  protected def handleAwaitingColorInput(input: String): Unit =
    input.toLowerCase match
      case s if s.startsWith("color") =>
        val parts = s.split("\\s+")
        if parts.length >= 2 then
          controller.chooseColor(parts(1))
        else
          println("Verwendung: color r|y|g|b")

      case "save" => 
        controller.save()
      case "load" => 
        controller.load()
      case "undo" =>
        controller.undo()
      case "redo" =>
        controller.redo()
      case _ =>
        controller.chooseColor("")
        
  protected def handleNormalInput(input: String): Unit =
    input match
      case "draw" =>
        controller.drawCard

      case s if s.startsWith("play") =>
        val parts = s.split("\\s+")
        if parts.length == 2 then
          Try(parts(1).toInt).toOption match
            case Some(index) => controller.playCard(index)
            case None => println("Ungültiger Index.")
        else
          println("Verwendung: play <index>")

      case "save" =>
        controller.save()
      case "load" =>
        controller.load()
      case "undo" =>
        controller.undo()
      case "redo" =>
        controller.redo()

      case s if s.startsWith("color") =>
        print("Jetzt keine Farbauswahl nötig")

      case _ =>
        println("Unbekannter Befehl. (play <index>, draw, save, load, undo, redo, quit)")
  

  protected def askPlayers(): Seq[String] =
    println("Spieler eingeben, z.B. 'Ann Ben':")
    val line  = Option(scala.io.StdIn.readLine()).getOrElse("")
    val names = line.split("[ ,]+").toVector.filter(_.nonEmpty)
    if names.nonEmpty then names else Vector("Player1", "Player2")
