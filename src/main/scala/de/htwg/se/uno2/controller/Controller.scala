package de.htwg.se.uno2.controller

import de.htwg.se.uno2.core.api.*
import de.htwg.se.uno2.core.impl.DefaultGameFactory
import de.htwg.se.uno2.util._
import de.htwg.se.uno2.controller.api.ControllerInterface
import de.htwg.se.uno2.controller.view.GameSnapshotRenderer
import scala.util.{Try, Success, Failure}

private[controller] class DrawCardCommand(controller: Controller) extends Command:
  private var backup: Option[Game] = None

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

private[controller] class PlayCardCommand(controller: Controller, index: Int) extends Command:
  private var backup: Option[Game] = None

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

private[controller] class ChooseColorCommand(controller:Controller, token: String) extends Command:
  private var backup: Option[Game] = None
  
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

class Controller(factory: GameFactory = DefaultGameFactory) extends Observable with ControllerInterface:
  
  private var state: Option[Game] = None
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

  private[controller] def currentState: Option[Game] = state

  private[controller] def restoreState(oldState: Game): Unit =
    state = Some(oldState)

    if oldState.isAwaitingColorChoise then
      mode = AwaitingColorState
    else
      mode = NormalState
    notifyObservers
  
  def isAwaitingColorChoise: Boolean =
    state.exists(_.isAwaitingColorChoise)
    
  def drawCard: Unit =
    undoManager.doStep(new DrawCardCommand(this))
    
  def playCard(index: Int): Unit =
    undoManager.doStep(new PlayCardCommand(this, index))
    
  def chooseColor(token: String): Unit =
    undoManager.doStep(new ChooseColorCommand(this, token))
    
  def gameStateToString: String =
    state.map(g => GameSnapshotRenderer.render(g.snapshot)).getOrElse("Noch kein Spiel gestartet")

  private[controller] def setMode(newMode: ControllerState): Unit =
    mode = newMode
    
  private[controller] def playCardInternal(index: Int): Unit =
    state = state.map(_.playCard(index))

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
