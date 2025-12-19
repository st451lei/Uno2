package de.htwg.se.uno2.controller.api

import de.htwg.se.uno2.core.impl.model.Card
import de.htwg.se.uno2.util.Observer

trait ControllerInterface:
  def addObserver(o: Observer): Unit
  def removeObserver(o: Observer): Unit
  
  def startGame(names: Seq[String]): Unit
  
  def drawCard: Unit
  def playCard(index: Int): Unit
  def chooseColor(token: String): Unit
  
  def undo(): Unit
  def redo(): Unit
  
  def isAwaitingColorChoise: Boolean
  def gameStateToString: String

  def currentHand: Vector[Card]
  def topDiscard: Option[Card]
  def deckSize: Int
  def currentPlayerName: String