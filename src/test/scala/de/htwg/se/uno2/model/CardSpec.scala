package de.htwg.se.uno2.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class CardSpec extends AnyWordSpec {

  "A UNO card" should {

    "be playable if colors match" in {
      val top = Card(Color.Red, Rank.Number(5))
      val next = Card(Color.Red, Rank.Number(9))

      Card.canPlayOn(top, next) should
    }
  }
}