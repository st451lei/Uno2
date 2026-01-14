package de.htwg.se.uno2.core.impl.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

final class RuleSetSpec extends AnyWordSpec with Matchers {

  import Color.*
  import Rank.*

  "ClassicRuleSet" should {

    "allow wild only if no non-wild playable card exists in hand" in {
      val top = Card(Red, Number(5))
      val playableNonWild = Card(Red, Skip)
      val wild = Card(Black, Wild)

      val handHasNonWildPlayable = Vector(playableNonWild, wild)
      ClassicRuleSet.canPlayWild(top, wild, None, handHasNonWildPlayable) shouldBe false

      val handOnlyWildOrNonPlayable = Vector(Card(Green, Number(1)), wild)
      ClassicRuleSet.canPlayWild(top, wild, None, handOnlyWildOrNonPlayable) shouldBe true
    }
  }

  "ColorOnlyRuleSet" should {

    "ignore rank matching for normal cards" in {
      val top = Card(Red, Number(5))
      val sameRankDifferentColor = Card(Blue, Number(5))

      ColorOnlyRuleSet.canPlayOn(top, sameRankDifferentColor, None) shouldBe false
      ColorOnlyRuleSet.canPlayOn(top, Card(Red, Number(1)), None) shouldBe true
    }

    "apply same wild restriction logic as ClassicRuleSet" in {
      val top = Card(Yellow, Number(9))
      val playableNonWild = Card(Yellow, DrawTwo)
      val wild4 = Card(Black, WildDrawFour)

      ColorOnlyRuleSet.canPlayWild(top, wild4, None, Vector(playableNonWild, wild4)) shouldBe false
      ColorOnlyRuleSet.canPlayWild(top, wild4, None, Vector(Card(Blue, Number(1)), wild4)) shouldBe true
    }
  }
}
