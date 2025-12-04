package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.util._

class DrawCardCommand(controller: Controller) extends Command:
  private var backup: Option[GameState] = None

  override def doStep(): Unit =
    backup = controller.currentState
    controller.drawCardInternal()

  override def undoStep(): Unit =
    backup.foreach(controller.restoreState)

  override def redoStep(): Unit =
    controller.drawCardInternal()

class PlayCardCommand(controller: Controller, index: Int) extends Command:
  private var backup: Option[GameState] = None

  override def doStep(): Unit =
    backup = controller.currentState
    controller.playCardInternal(index)

  override def undoStep(): Unit =
    backup.foreach(controller.restoreState)

  override def redoStep(): Unit =
    controller.playCardInternal(index)

class ChooseColorCommand(controller:Controller, token: String) extends Command:
  private var backup: Option[GameState] = None
  
  override def doStep(): Unit =
    backup = controller.currentState
    controller.chooseColorInternal(token)
    
  override def undoStep(): Unit =
    backup.foreach(controller.restoreState)
    
  override def redoStep(): Unit =
    controller.chooseColorInternal(token)
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
