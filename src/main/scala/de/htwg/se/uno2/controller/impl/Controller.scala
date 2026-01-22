package de.htwg.se.uno2.controller.impl

import com.google.inject.Inject
import de.htwg.se.uno2.core.impl.DefaultGameFactory
import de.htwg.se.uno2.core.impl.model.*
import de.htwg.se.uno2.util.*
import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.controller.view.GameSnapshotRenderer
import de.htwg.se.uno2.core.{Game, GameFactory}
import de.htwg.se.uno2.core.impl.GameImpl
import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.fileio.xml.XmlFileIO

import scala.util.{Failure, Success, Try}

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
    
private[controller] class EndTurnCommand(controller: Controller) extends Command:
  private var backup: Option[Game] = None
  
  override def doStep(): Unit =
    backup = controller.currentState
    Try(controller.endTurnInternal()) match
      case Success(_) => ()
      case Failure(_) =>
        backup.foreach(controller.restoreState)
        
  override def undoStep(): Unit =
    backup.foreach(controller.restoreState)
    
  override def redoStep(): Unit =
    Try(controller.endTurnInternal()).getOrElse(())  

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

class Controller @Inject() (factory: GameFactory, fileIO: FileIOInterface) extends Observable with ControllerInterface:

  private var state: Option[Game] = None
  private var mode: ControllerState = NormalState

  private var undoManager = new UndoManager

  def undo(): Unit =
    undoManager.undoStep()

  def redo(): Unit =
    undoManager.redoStep()

  def startGame(names: Seq[String]): Unit =
    state = Some(factory.create(names))
    mode = NormalState
    undoManager = new UndoManager
    notifyObservers
    
  def save(): Unit =
    state.foreach(g => fileIO.save(g.gameState))
    
  def load(): Unit =
    fileIO.load() match
      case Some(gs) =>
        restoreState(GameImpl(gs))
        undoManager = new UndoManager
      case None =>
        ()
        
  

  private[controller] def currentState: Option[Game] = state

  private[controller] def restoreState(oldState: Game): Unit =
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

  def isGameOver: Boolean =
    state.exists(_.isGameOver)

  def winnerName: Option[String] =
    state.flatMap(_.winnerName)

  def drawCard: Unit =
    if !isGameOver then undoManager.doStep(new DrawCardCommand(this))

  def playCard(index: Int): Unit =
    if !isGameOver then undoManager.doStep(new PlayCardCommand(this, index))
    
  def canEndTurn: Boolean =
    state.exists(_.gameState.canEndTurn)
    
  def endTurn(): Unit =
    if !isGameOver then undoManager.doStep(new EndTurnCommand(this))  
  
  def chooseColor(token: String): Unit =
    if !isGameOver then undoManager.doStep(new ChooseColorCommand(this, token))

  def gameStateToString: String =
    state.map(g => GameSnapshotRenderer.render(g.snapshot)).getOrElse("Noch kein Spiel gestartet")

  def currentHand: Vector[Card] =
    state.map(_.currentHand).getOrElse(Vector.empty)

  def topDiscard: Option[Card] =
    state.flatMap(_.topDiscard)

  def deckSize: Int =
    state.map(_.deckSize).getOrElse(0)

  def currentPlayerName: String =
    state.map(_.currentPlayerName).getOrElse("-")

  def opponentCardCounts: Vector[(String, Int)] =
    state.map { g =>
      val gs = g.gameState
      val players = gs.players
      val n = players.size
      if n <= 1 then Vector.empty
      else
        val cur = gs.currentPlayerIndex
        (1 until n).toVector.map { step =>
          val i = (cur + step) % n
          val p = players(i)
          (p.name, p.hand.size)
        }
    }.getOrElse(Vector.empty)

  def activeColor: Option[Color] =
    state.flatMap(_.gameState.chosenColor)

  private[controller] def setMode(newMode: ControllerState): Unit =
    mode = newMode

  private[controller] def playCardInternal(index: Int): Unit =
    state = state.map(_.playCard(index))

    if state.exists(_.isAwaitingColorChoise) then
      mode = AwaitingColorState
    else
      mode = NormalState

    notifyObservers
    
  private[controller] def endTurnInternal(): Unit =
    state = state.map(_.endTurn)
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

object Controller:
  def default: Controller = new Controller(DefaultGameFactory, new XmlFileIO)