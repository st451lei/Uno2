package de.htwg.se.uno2.core.impl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.impl.model.{ColorOnlyGameStateFactory, DefaultGameStateFactory}
import de.htwg.se.uno2.core.impl.model.{Rank}
import de.htwg.se.uno2.core.impl.model.ClassicRuleSet
import de.htwg.se.uno2.core.impl.model.ColorOnlyRuleSet
import de.htwg.se.uno2.core.impl.DefaultGameFactory

final class GameFactorySpec extends AnyWordSpec with Matchers {

  import Rank.*

  "DefaultGameStateFactory" should {

    "require at least 1 player name" in {
      intercept[IllegalArgumentException] {
        DefaultGameStateFactory.create(Seq.empty)
      }
    }

    "create initial state: 7 cards each, discard is not wild, classic rules" in {
      val gs = DefaultGameStateFactory.create(Seq("A", "B", "C"))

      gs.players.size shouldBe 3
      all(gs.players.map(_.hand.size)) shouldBe 7
      gs.ruleSet shouldBe ClassicRuleSet
      gs.awaitingColor shouldBe false
      gs.chosenColor shouldBe None

      gs.discard.size shouldBe 1
      gs.discard.last.rank should not be Wild
      gs.discard.last.rank should not be WildDrawFour

      val maxDeck = 108 - 7 * 3 - 1
      val minDeck = maxDeck - 8
      gs.deck.size should be <= maxDeck
      gs.deck.size should be >= minDeck
    }
  }

  "ColorOnlyGameStateFactory" should {
    "use ColorOnlyRuleSet" in {
      val gs = ColorOnlyGameStateFactory.create(Seq("A", "B"))
      gs.ruleSet shouldBe ColorOnlyRuleSet
    }
  }

  "DefaultGameFactory" should {
    "create a GameImpl and keep first name as current player" in {
      val g = DefaultGameFactory.create(Seq("A", "B"))
      g.currentPlayerName shouldBe "A"
    }
  }
}
