package de.htwg.se.uno2.controller.view

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.Game.Snapshot

final class GameSnapshotRendererSpec extends AnyWordSpec with Matchers {

  private def snap(
                    player: String = "Alice",
                    hand: Vector[String] = Vector.empty,
                    top: Option[String] = None,
                    deck: Int = 42,
                    awaitingColor: Boolean = false,
                    winner: Option[String] = None
                  ): Snapshot =
    Snapshot(
      currentPlayerName = player,
      currentHand = hand,
      topDiscard = top,
      deckSize = deck,
      awaitingColor = awaitingColor,
      winnerName = winner
    )

  "GameSnapshotRenderer.render" should {

    "render normal command list when not awaiting color and no winner" in {
      val s = snap(awaitingColor = false, winner = None)
      val out = GameSnapshotRenderer.render(s)

      out should include("Aktueller Spieler")
      out should include("Deckgröße")
      out should include("play <i> | draw | save | load | undo | redo | quit")
    }

    "render awaiting-color command list when awaitingColor is true" in {
      val s = snap(awaitingColor = true, winner = None)
      val out = GameSnapshotRenderer.render(s)

      out should include("color r|y|g|b | save | load | undo | redo | quit")
    }

    "render game-over command list and winner line when winner exists" in {
      val s = snap(awaitingColor = false, winner = Some("Bob"))
      val out = GameSnapshotRenderer.render(s)

      out should include("Gewinner: Bob")
      out should include("Spiel ist vorbei. (quit | undo | redo)")
    }

    "render 'Keine Karte' when topDiscard is empty" in {
      val s = snap(top = None)
      val out = GameSnapshotRenderer.render(s)

      out should include("Oberste Karte: Keine Karte")
    }
  }
}
