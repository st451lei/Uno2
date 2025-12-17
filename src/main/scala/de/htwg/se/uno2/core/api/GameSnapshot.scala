package de.htwg.se.uno2.core.api

final case class GameSnapshot(
                             currentPlayerName: String,
                             currentHand: Vector[String],
                             topDiscard: Option[String],
                             deckSize: Int,
                             awaitingColor: Boolean
                             )
