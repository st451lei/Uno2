package de.htwg.se.uno2.model

import de.htwg.se.uno2.core.impl.model.{Card, ClassicRuleSet, Color, ColorOnlyRuleSet, Rank}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers.*

class RuleSetSpec extends AnyWordSpec:

  "ClassicRuleSet" should {
    "delegate to Card.canPlayOn" in {
      val top = Card(Color.Red, Rank.Number(1))
      val next = Card(Color.Red, Rank.Skip)

      ClassicRuleSet.canPlayOn(top, next, None) shouldBe
        Card.canPlayOn(top, next, None)
    }
  }

  "ColorOnlyRuleSet" should {
    "allow matching color or wilds" in {
      val top = Card(Color.Green, Rank.Number(2))
      val matchColor = Card(Color.Green, Rank.DrawTwo)
      val otherColor = Card(Color.Red, Rank.Number(5))
      val wild = Card(Color.Black, Rank.Wild)
      val wild4 = Card(Color.Black, Rank.WildDrawFour)

      ColorOnlyRuleSet.canPlayOn(top, matchColor, None) shouldBe true
      ColorOnlyRuleSet.canPlayOn(top, wild, None) shouldBe true
      ColorOnlyRuleSet.canPlayOn(top, wild4, None) shouldBe true
      ColorOnlyRuleSet.canPlayOn(top, otherColor, None) shouldBe false
    }

    "respect chosenColor when defined" in {
      val top = Card(Color.Red, Rank.Number(1))
      val greenCard = Card(Color.Green, Rank.Number(3))

      ColorOnlyRuleSet.canPlayOn(top, greenCard, Some(Color.Green)) shouldBe true
      ColorOnlyRuleSet.canPlayOn(top, greenCard, Some(Color.Blue)) shouldBe false
    }
  }
