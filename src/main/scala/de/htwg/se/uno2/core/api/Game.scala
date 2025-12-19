package de.htwg.se.uno2.core.api

import de.htwg.se.uno2.core.impl.model._

trait Game():
  def drawCard: Game
  def playCard(index: Int): Game
  def chooseColor(token: String): Game

  def isAwaitingColorChoise: Boolean
  def snapshot: Game.Snapshot
  def currentPlayer: Player
  def currentHand: Vector[Card]
  def topDiscard: Option[Card]
  def deckSize: Int
  def deck: Deck
  def discard: Vector[Card]
  def currentPlayerName: String
object Game:
  final case class Snapshot(
                            currentPlayerName: String,
                            currentHand: Vector[String],
                            topDiscard: Option[String],
                            deckSize: Int,
                            awaitingColor: Boolean
                            )