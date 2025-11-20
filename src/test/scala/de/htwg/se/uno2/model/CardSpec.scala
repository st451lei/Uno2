package de.htwg.se.uno2.model

import org.scalatest.matchers.should.Matchers.*
import org.scalatest.wordspec.AnyWordSpec

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

    "be playable if next card Wild" in {
      val top = Card(Color.Green, Rank.Reverse)
      val next = Card(Color.Black, Rank.Wild)
      Card.canPlayOn(top, next, None) shouldBe true
    }

    "be playable if next card WildDrawFour" in {
      val top = Card(Color.Yellow, Rank.DrawTwo)
      val next = Card(Color.Black, Rank.WildDrawFour)
      Card.canPlayOn(top, next, None) shouldBe true
    }
    
    "respect chosenColor if it is defined" in {
      val top = Card(Color.Red, Rank.Number(5))
      val next = Card(Color.Green, Rank.Number(9))
      
      Card.canPlayOn(top, next, Some(Color.Green)) shouldBe true
      Card.canPlayOn(top, next, Some(Color.Yellow)) shouldBe false
    }
    
    "not be playable if neither color nor rank match and no wild / chosen color" in {
      val top = Card(Color.Red, Rank.Number(5))
      val next = Card(Color.Green, Rank.Skip)
      
      Card.canPlayOn(top, next, None) shouldBe false
    }
  }  
}