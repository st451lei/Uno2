package de.htwg.se.uno2.core.api

trait Game:
  def drawCard: Game
  def playCard(index: Int): Game
  def chooseColor(token: String): Game

  def isAwaitingColorChoise: Boolean
  def snapshot: Game.Snapshot

object Game:
  final case class Snapshot(
                            currentPlayerName: String,
                            currentHand: Vector[String],
                            topDiscard: Option[String],
                            deckSize: Int,
                            awaitingColor: Boolean
                            )