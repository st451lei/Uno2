package de.htwg.se.uno2.model

import de.htwg.se.uno2.core.impl.model.{Card, Color, Deck, Rank}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DeckSpec extends AnyWordSpec with Matchers {
  private val cards = Vector(
    Card(Color.Red, Rank.Number(1)),
    Card(Color.Red, Rank.Number(2)),
    Card(Color.Blue, Rank.Number(1)),
    Card(Color.Green, Rank.Skip),
    Card(Color.Black, Rank.Wild)
  )

  "A Deck" should {
    "have the correct initial size" in {
    val deck = Deck(cards)
    deck.size shouldBe cards.size
    deck.peek shouldBe Some(cards.head)
    }
    
    "draw a single card and produce a new deck" in {
      val deck = Deck(cards)
      val (c,d2) = deck.draw()
      c shouldBe cards.head
      d2.size shouldBe cards.size - 1
    }
    "draw n cards and produce a new deck" in {
      val deck = Deck(cards)
      val (drawn,d2) = deck.draw(3)
      drawn shouldBe cards.take(3)
      d2.size shouldBe cards.size -3
    }
    
    "report empty when all cards are drawn" in {
      val deck = Deck(cards)
      val (drawnAll,d2) = deck.draw(deck.size)
      d2.isEmpty shouldBe true
      an [IllegalArgumentException] shouldBe thrownBy {
        d2.draw()
      }
    }
    "shuffle without losing cards" in {
      val deck = Deck(cards)
      val d2 = deck.shuffle()
      d2.cards.toSet shouldBe deck.cards.toSet
      d2.cards.length shouldBe deck.cards.length
    }
    
    "reset to the original order using Deck.from" in {
      val deck = Deck(cards)
      val shuffled = deck.shuffle()
      val reset = Deck.from(cards)
      reset.cards shouldBe cards
    }
    
    "deal cards to players and retrun new deck" in {
      val deck = Deck(cards)
      val (hands,d2) = deck.deal(numPlayers = 2, cardsEach = 2)
      hands should have length 2
      hands.foreach(_.length shouldBe 2)
      d2.size shouldBe cards.size - 4
      
      hands(0) shouldBe Seq(cards(0), cards(2))
      hands(1) shouldBe Seq(cards(1), cards(3))
    }
    
    "add cards immutably to the top and to the bottom" in {
      val deck = Deck(cards)
      val newCard = Card(Color.Yellow, Rank.Number(3))

      val d2 = deck.add(newCard)
      d2.peek shouldBe Some(newCard)

      val newCard2 = Card(Color.Black, Rank.Wild)
      val d3 = d2.addToBottom(Seq(newCard2))
      d3.cards.last shouldBe newCard2
    }
    
    "throw on negative draw count" in {
      val deck = Deck(cards)
      an[IllegalArgumentException] shouldBe thrownBy {
        deck.draw(-1)
      }
    } 
    "throw on invalid deal arguments" in {
      val deck = Deck(cards)
      an [IllegalArgumentException] shouldBe thrownBy {
        deck.deal(numPlayers = 2, cardsEach = -1)
      }
    }
  }
}