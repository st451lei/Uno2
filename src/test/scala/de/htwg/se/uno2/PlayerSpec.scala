package de.htwg.se.uno2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class PlayerSpec extends AnyWordSpec {
  
  "A Player" should {
    
    "draw cards from a pile" in {
      val player = Player("Stas", Vector.empty)
      val pile = Vector(
        Card(Color.Red, Rank.Number(1)),
        Card(Color.Blue, Rank.Reverse),
        Card(Color.Green, Rank.Number(3))
      )
      
      val (p2, rest) = player.draw(2, pile)
      p2.size shouldBe 2
      rest.size shouldBe 1
    }
    
    "play a card by index and reduce hand size" in {
      val player = Player("Stas", Vector(
        Card(Color.Red, Rank.Number(5)),
        Card(Color.Blue, Rank.Skip)
      ))
      val (played, p2) = player.playAt(1)
      
      played shouldBe Card(Color.Blue, Rank.Skip)
      p2.size shouldBe 1
    }
  }
}