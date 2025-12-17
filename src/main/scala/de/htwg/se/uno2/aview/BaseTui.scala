package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.api.ControllerInterface
import de.htwg.se.uno2.util.Observer
import scala.io.StdIn.readLine

abstract class BaseTui(val controller: ControllerInterface) extends Observer:
  controller.addObserver(this)

  def update: Unit

  final def run(): Unit =
    val names = askPlayers()
    controller.startGame(names)
    mainLoop()

  private final def mainLoop(): Unit =
    print("> ")
    val input = Option(readLine()).getOrElse("").trim

    if input == "quit" then
      handleQuit()
    else if controller.isAwaitingColorChoise then
      handleAwaitingColorInput(input)
    else
      handleNormalInput(input)

    if !shouldExit then
      mainLoop()

  protected def shouldExit: Boolean

  protected def handleQuit(): Unit

  protected def handleAwaitingColorInput(input: String): Unit

  protected def handleNormalInput(input: String): Unit

  protected def askPlayers(): Seq[String]

