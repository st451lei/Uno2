package de.htwg.se.uno2.core.impl

import de.htwg.se.uno2.core.Game
import de.htwg.se.uno2.core.impl.model.{Card, Deck, GameState, Player}

final class GameImpl private (private val state: GameState) extends Game:

  override def gameState: GameState = state
  
  override def drawCard: Game =
    GameImpl(state.drawCard)
    
  override def playCard(index: Int): Game =
    GameImpl(state.playCard(index))
    
  override def chooseColor(token: String): Game =
    GameImpl(state.chooseColor(token))

  override def isAwaitingColorChoise: Boolean =
    state.isAwaitingColorChoise
    
  override def snapshot: Game.Snapshot =
    val cur = state.currentPlayer
    Game.Snapshot(
      currentPlayerName = cur.name,
      currentHand = cur.hand.map(_.toString),
      topDiscard = state.discard.lastOption.map(_.toString),
      deckSize = state.deck.size,
      awaitingColor = state.isAwaitingColorChoise,
      winnerName = state.winnerName
    )
  override def currentPlayer: Player =
    state.currentPlayer
  
  override def isGameOver: Boolean =
    state.isGameOver

  override def winnerName: Option[String] =
    state.winnerName
    
  override def currentHand: Vector[Card] =
    state.currentHand
    
  override def topDiscard: Option[Card] =
    state.topDiscard

  override def deckSize: Int =
    state.deckSize

  override def currentPlayerName: String =
    state.currentPlayerName
    
  override def deck: Deck =
    state.deck
  override def discard: Vector[Card] =
    state.discard
    
object GameImpl:
  def apply(gs: GameState): Game = new GameImpl(gs)