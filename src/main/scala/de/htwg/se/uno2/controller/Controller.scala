package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.model.GameState.*
import de.htwg.se.uno2.util.Observable

class Controller extends Observable:
  
  private var state: Option[GameState] = None
  
  def startGame(names: Seq[String]): Unit =
    state = Some(GameState.initial(names))
    notifyObservers
    
  def currentPlayer: Player =
    state.get.currentPlayer
    
  def isAwaitingColorChoise: Boolean =
    state.exists(_.isAwaitingColorChoise)
    
  def drawCard: Unit =
    state = state.map(s => s.drawCard)
    notifyObservers
    
  def playCard(index: Int): Unit =
    state = state.map(s => s.playCard(index))
    notifyObservers
    
  def chooseColor(token: String): Unit =
    state = state.map(s => s.chooseColor(token))
    notifyObservers
    
  def gameStateToString: String =
    state.map(_.toDisplayString).getOrElse("Noch kein Spiel gestartet")