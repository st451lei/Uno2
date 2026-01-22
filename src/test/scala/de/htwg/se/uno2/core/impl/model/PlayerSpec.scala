package de.htwg.se.uno2.core.impl.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

final class PlayerSpec extends AnyWordSpec with Matchers {

  import Color.*
  import Rank.*

  private val c1 = Card(Red, Number(1))
  private val c2 = Card(Green, Number(2))
  private val c3 = Card(Blue, Number(3))

  "Player" should {

    "draw(n) should take from drawPile and extend hand" in {
      val p = Player("A", Vector(c1))
      val (p2, rest) = p.draw(2, Vector(c2, c3, c1))

      p2.hand shouldBe Vector(c1, c2, c3)
      rest shouldBe Vector(c1)
    }

    "playAt should return card at index and remove it from hand" in {
      val p = Player("A", Vector(c1, c2, c3))
      val (played, p2) = p.playAt(1)

      played shouldBe c2
      p2.hand shouldBe Vector(c1, c3)
    }

    "playAt on invalid index should throw" in {
      val p = Player("A", Vector(c1))
      intercept[NoSuchElementException] {
        p.playAt(10)
      }
    }
  }
}
