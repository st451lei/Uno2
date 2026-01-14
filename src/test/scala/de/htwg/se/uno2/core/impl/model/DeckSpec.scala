package de.htwg.se.uno2.core.impl.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

final class DeckSpec extends AnyWordSpec with Matchers {

  import Color.*
  import Rank.*

  private val c1 = Card(Red, Number(1))
  private val c2 = Card(Green, Number(2))
  private val c3 = Card(Blue, Number(3))
  private val c4 = Card(Yellow, Number(4))

  "Deck" should {

    "peek and draw() from non-empty deck" in {
      val d = Deck.from(Seq(c1, c2, c3))
      d.peek shouldBe Some(c1)

      val (drawn, d2) = d.draw()
      drawn shouldBe c1
      d2.cards shouldBe Vector(c2, c3)
    }

    "draw() from empty deck should throw" in {
      val d = Deck.empty
      intercept[IllegalArgumentException] {
        d.draw()
      }
    }

    "draw(n) should throw if n < 0" in {
      val d = Deck.from(Seq(c1))
      intercept[IllegalArgumentException] {
        d.draw(-1)
      }
    }

    "draw(n) with n > size should just take all and leave empty" in {
      val d = Deck.from(Seq(c1, c2))
      val (taken, rest) = d.draw(10)
      taken shouldBe Vector(c1, c2)
      rest.isEmpty shouldBe true
    }

    "add should put card on top; addToBottom should append" in {
      val d = Deck.from(Seq(c1, c2))
      d.add(c3).cards shouldBe Vector(c3, c1, c2)
      d.addToBottom(Seq(c3, c4)).cards shouldBe Vector(c1, c2, c3, c4)
    }

    "deal should validate args" in {
      val d = Deck.from(Seq(c1, c2))
      intercept[IllegalArgumentException] { d.deal(0, 7) }
      intercept[IllegalArgumentException] { d.deal(2, -1) }
    }

    "deal should distribute cards round-robin and stop when deck is empty" in {
      val d = Deck.from(Seq(c1, c2, c3, c4, c1))
      val (hands, rest) = d.deal(numPlayers = 2, cardsEach = 3)

      hands.size shouldBe 2
      hands(0).size shouldBe 3
      hands(1).size shouldBe 2
      rest.isEmpty shouldBe true
    }

    "shuffle should keep size and multiset of cards" in {
      val d = Deck.from(Seq(c1, c2, c3, c4))
      val s = d.shuffle()

      s.size shouldBe d.size
      s.cards.groupMapReduce(identity)(_ => 1)(_ + _) shouldBe d.cards.groupMapReduce(identity)(_ => 1)(_ + _)
    }
  }
}
