package de.htwg.se.uno2.core.impl.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

final class CardSpec extends AnyWordSpec with Matchers {

  import Color.*
  import Rank.*

  "Card.canPlayOn" should {

    "allow same color" in {
      val top = Card(Red, Number(1))
      val next = Card(Red, Skip)
      Card.canPlayOn(top, next) shouldBe true
    }

    "allow same rank" in {
      val top = Card(Red, DrawTwo)
      val next = Card(Blue, DrawTwo)
      Card.canPlayOn(top, next) shouldBe true
    }

    "allow wilds regardless of color/rank" in {
      val top = Card(Green, Number(9))
      Card.canPlayOn(top, Card(Black, Wild)) shouldBe true
      Card.canPlayOn(top, Card(Black, WildDrawFour)) shouldBe true
    }

    "respect chosenColor override" in {
      val top = Card(Red, Number(5))
      val next = Card(Blue, Number(1))

      Card.canPlayOn(top, next, chosenColor = None) shouldBe false
      Card.canPlayOn(top, next, chosenColor = Some(Blue)) shouldBe true
    }

    "reject when neither color nor rank matches and not a wild" in {
      val top = Card(Red, Number(5))
      val next = Card(Blue, Number(1))
      Card.canPlayOn(top, next) shouldBe false
    }
  }
}
