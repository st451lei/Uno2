package de.htwg.se.uno2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class CardSpec extends AnyWordSpec {

  "A UNO card" should {

    "be playable if colors match" in {
      val top = Card(Color.Red, Rank.Number(5))
      val next = Card(Color.Red, Rank.Skip)

      Card.canPlayOn(top, next, None) shouldBe true
    }

    "be playable if ranks match" in {
      val top = Card(Color.Blue, Rank.Number(7))
      val next = Card(Color.Red, Rank.Number(7))
      Card.canPlayOn(top, next, None) shouldBe true
    }
  }
}