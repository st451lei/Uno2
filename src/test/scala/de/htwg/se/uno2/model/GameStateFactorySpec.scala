package de.htwg.se.uno2.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class GameStateFactorySpec extends AnyWordSpec with Matchers:

  "DefaultGameStateFactory" should {

    "create a valid initial game state" in {
      val names = Seq("Alice", "Bob")
      val state = DefaultGameStateFactory.create(names)

      state.players.map(_.name) should contain theSameElementsAs names
      all(state.players.map(_.hand.size)) shouldBe 7
      state.discard.size shouldBe 1

      state.discard.head.rank match
        case Rank.Wild | Rank.WildDrawFour =>
          fail("First card must not be wild")
        case _ => ()

      val totalCards =
        state.deck.size +
          state.discard.size +
          state.players.map(_.hand.size).sum

      totalCards should not be 0
      state.ruleSet shouldBe ClassicRuleSet
    }

    "reject empty player list" in {
      an [IllegalArgumentException] shouldBe thrownBy {
        DefaultGameStateFactory.create(Seq.empty)
      }
    }
  }

  "ColorOnlyGameStateFactory" should {

    "produce a state with ColorOnlyRuleSet" in {
      val state = ColorOnlyGameStateFactory.create(Seq("A", "B"))
      state.ruleSet shouldBe ColorOnlyRuleSet
      state.players.size shouldBe 2
    }
  }
