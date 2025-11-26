package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.util.Observable

class Controller(factory: GameStateFactory = DefaultGameStateFactory) extends Observable:
  
  private var state: Option[GameState] = None
  
  private var mode: ControllerState = NormalState
  
  def startGame(names: Seq[String]): Unit =
    state = Some(factory.create(names))
    mode = NormalState
    notifyObservers
    
  def currentPlayer: Player =
    state.get.currentPlayer
    
  def isAwaitingColorChoise: Boolean =
    state.exists(_.isAwaitingColorChoise)
    
  def drawCard: Unit =
    mode.drawCard(this)
    
  def playCard(index: Int): Unit =
    mode.playCard(this, index)
    
  def chooseColor(token: String): Unit =
    mode.chooseColor(this, token)
    
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