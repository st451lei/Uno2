package de.htwg.se.uno2.core.impl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.impl.model.*
import de.htwg.se.uno2.core.impl.model.Color.*
import de.htwg.se.uno2.core.impl.model.Rank.*

final class GameImplSpec extends AnyWordSpec with Matchers {

  private def C(c: Color, r: Rank): Card = Card(c, r)

  "GameImpl" should {

    "delegate state accessors and build snapshot as strings" in {
      val gs = GameState(
        deck = Deck(Vector(C(Green, Number(1)))),
        discard = Vector(C(Red, Number(5))),
        players = Vector(Player("A", Vector(C(Red, Number(7))))),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false,
        ruleSet = ClassicRuleSet
      )

      val g = GameImpl(gs)

      g.currentPlayerName shouldBe "A"
      g.deckSize shouldBe 1
      g.topDiscard shouldBe Some(C(Red, Number(5)))

      val snap = g.snapshot
      snap.currentPlayerName shouldBe "A"
      snap.currentHand shouldBe gs.currentPlayer.hand.map(_.toString)
      snap.topDiscard shouldBe Some(C(Red, Number(5)).toString)
      snap.deckSize shouldBe 1
      snap.awaitingColor shouldBe false
    }

    "drawCard should return new Game with updated underlying state" in {
      val gs = GameState(
        deck = Deck(Vector(C(Green, Number(1)))),
        discard = Vector(C(Red, Number(5))),
        players = Vector(Player("A", Vector.empty)),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false,
        ruleSet = ClassicRuleSet
      )

      val g2 = GameImpl(gs).drawCard
      g2.currentHand.size shouldBe 1
      g2.deckSize shouldBe 0
    }
  }
}
