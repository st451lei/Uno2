package de.htwg.se.uno2.core.impl

import de.htwg.se.uno2.core.api.*
import de.htwg.se.uno2.core.impl.model.GameState

final class GameImpl private (private val state: GameState) extends Game:

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
      awaitingColor = state.isAwaitingColorChoise
    )

object GameImpl:
  def apply(gs: GameState): Game = new GameImpl(gs)