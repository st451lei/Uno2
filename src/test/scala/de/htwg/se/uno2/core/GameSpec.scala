package de.htwg.se.uno2.core

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.impl.DefaultGameFactory

final class GameSpec extends AnyWordSpec with Matchers {

  "Game.Snapshot" should {

    "support copy and equality" in {
      val s1 = Game.Snapshot(
        currentPlayerName = "A",
        currentHand = Vector("c1", "c2"),
        topDiscard = Some("top"),
        deckSize = 10,
        awaitingColor = false,
        winnerName = None
      )

      val s2 = s1.copy(deckSize = 9)
      s2.deckSize shouldBe 9
      s2.currentPlayerName shouldBe "A"
      s2.currentHand shouldBe Vector("c1", "c2")
      s2.topDiscard shouldBe Some("top")
      s2.awaitingColor shouldBe false
      s2.winnerName shouldBe None

      s1 should not be s2
      s1 shouldBe s1.copy()
    }
  }

  "Game (via GameImpl)" should {

    "produce a snapshot consistent with its accessors" in {
      val g = DefaultGameFactory.create(Seq("A", "B"))

      val s = g.snapshot
      s.currentPlayerName shouldBe g.currentPlayerName
      s.deckSize shouldBe g.deckSize
      s.awaitingColor shouldBe g.isAwaitingColorChoise
      s.winnerName shouldBe g.winnerName

      s.topDiscard.isDefined shouldBe g.topDiscard.isDefined
      s.currentHand.size shouldBe g.currentHand.size
    }
  }
}


