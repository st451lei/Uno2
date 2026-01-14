package de.htwg.se.uno2.core.impl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.Game
import de.htwg.se.uno2.core.impl.model.*
import de.htwg.se.uno2.core.impl.model.Rank.*
import de.htwg.se.uno2.core.impl.model.ClassicRuleSet

final class DefaultGameFactorySpec extends AnyWordSpec with Matchers {

  "DefaultGameFactory" should {

    "create a GameImpl instance" in {
      val g: Game = DefaultGameFactory.create(Seq("A", "B"))
      g.isInstanceOf[GameImpl] shouldBe true
    }

    "create a consistent initial game state" in {
      val g: Game = DefaultGameFactory.create(Seq("A", "B", "C"))
      val gs = g.gameState

      gs.players.map(_.name) shouldBe Vector("A", "B", "C")
      all(gs.players.map(_.hand.size)) shouldBe 7
      gs.ruleSet shouldBe ClassicRuleSet
      gs.awaitingColor shouldBe false
      gs.chosenColor shouldBe None

      gs.discard.size shouldBe 1
      gs.discard.last.rank should not be Wild
      gs.discard.last.rank should not be WildDrawFour
    }
  }
}
