package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.util._
import scala.util.{Try, Success, Failure}

class DrawCardCommand(controller: Controller) extends Command:
  private var backup: Option[GameState] = None

  override def doStep(): Unit =
    backup = controller.currentState
    Try(controller.drawCardInternal()) match
      case Success(_) => ()
      case Failure(_) =>
        backup.foreach(controller.restoreState)

  override def undoStep(): Unit =
    backup.foreach(controller.restoreState)

  override def redoStep(): Unit =
    Try(controller.drawCardInternal()).getOrElse(())

class PlayCardCommand(controller: Controller, index: Int) extends Command:
  private var backup: Option[GameState] = None

  override def doStep(): Unit =
    backup = controller.currentState
    Try(controller.playCardInternal(index)) match
      case Success(_) => ()
      case Failure(_) =>
        backup.foreach(controller.restoreState)

  override def undoStep(): Unit =
    backup.foreach(controller.restoreState)

  override def redoStep(): Unit =
    Try(controller.playCardInternal(index)).getOrElse(())

class ChooseColorCommand(controller:Controller, token: String) extends Command:
  private var backup: Option[GameState] = None
  
  override def doStep(): Unit =
    backup = controller.currentState
    Try(controller.chooseColorInternal(token)) match
      case Success(_) => ()
      case Failure(_) =>
        backup.foreach(controller.restoreState)

  override def undoStep(): Unit =
    backup.foreach(controller.restoreState)
    
  override def redoStep(): Unit =
    Try(controller.chooseColorInternal(token)).getOrElse(())

class Controller(factory: GameStateFactory = DefaultGameStateFactory) extends Observable:
  
  private var state: Option[GameState] = None
  private var mode: ControllerState = NormalState

  private val undoManager = new UndoManager

  def undo(): Unit =
    undoManager.undoStep()

  def redo(): Unit =
    undoManager.redoStep()

  def startGame(names: Seq[String]): Unit =
    state = Some(factory.create(names))
    mode = NormalState
    notifyObservers

  def currentState: Option[GameState] = state

  def restoreState(oldState: GameState): Unit =
    state = Some(oldState)

    if oldState.isAwaitingColorChoise then
      mode = AwaitingColorState
    else
      mode = NormalState
    notifyObservers

  def currentPlayer: Player =
    state.get.currentPlayer
    
  def isAwaitingColorChoise: Boolean =
    state.exists(_.isAwaitingColorChoise)
    
  def drawCard: Unit =
    undoManager.doStep(new DrawCardCommand(this))
    
  def playCard(index: Int): Unit =
    undoManager.doStep(new PlayCardCommand(this, index))
    
  def chooseColor(token: String): Unit =
    undoManager.doStep(new ChooseColorCommand(this, token))
    
  def gameStateToString: String =
    state.map(_.toDisplayString).getOrElse("Noch kein Spiel gestartet")

  def currentGameState: Option[GameState] = state

  def currentHand: Vector[Card] =
    state.map(_.currentPlayer.hand).getOrElse(Vector.empty)

  def topDiscard: Option[Card] =
    state.flatMap(_.discard.lastOption) // hängt von deinem GameState ab

  def deckSize: Int =
    state.map(_.deck.size).getOrElse(0)

  def currentPlayerName: String =
    state.map(_.currentPlayer.name).getOrElse("-")

  private[controller] def setMode(newMode: ControllerState): Unit =
    mode = newMode
    
  private[controller] def playCardInternal(index: Int): Unit =
    state = state.map(s => s.playCard(index))

    if state.exists(_.isAwaitingColorChoise) then
      mode = AwaitingColorState
    else
      mode = NormalState

    notifyObservers
    
  private[controller] def chooseColorInternal(token: String): Unit =
    state = state.map(_.chooseColor(token))
    
    mode = NormalState
    
    notifyObservers
    
  private[controller] def drawCardInternal(): Unit =
    state = state.map(_.drawCard)
    
    mode = NormalState
    
    notifyObservers
