package de.htwg.se.uno2.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers._

class DeckSpec extends AnyWordSpec with Matchers {
  private val cards = Seq(
    Card(Some("Red"), "1"),
    Card(Some("Red"), "2"),
    Card(Some("Blue"), "1"),
    Card(Some("Green"), "Skip"),
    Card(None, "Wild")
  )

  "A Deck" should {
    "have the correct initial size" in {
    val deck = new Deck(cards)
    deck.size shouldBe cards.size
    deck.peek shouldBe Some(cards.head)
    }
    
    "draw a single card from the top" in {
      val deck = new Deck(cards)
      val c = deck.draw()
      c shouldBe Some(cards.head)
      deck.size shouldBe cards.size - 1
    }
    "draw n cards and reduce size accordingly" in {
      val deck = new Deck(cards)
      val drawn = deck.draw(3)
      drawn should have size 3
      deck.size shouldBe cards.size -3
      drawn shouldBe cards.take(3)
    }
    
    "report empty when all cards drawn" in {
      val deck = new Deck(cards)
      deck.draw(cards.size)
      deck.isEmpty shouldBe true
      deck.draw() shouldBe None
    }
    "shuffle without losing cards (same multiset)" in {
      val deck = new Deck(cards)
      val before = deck.allCards
      deck.shuffle()
      val after = deck.allCards
      after.toSet shouldBe before.toSet
      after.length shouldBe before.length
    }
    
    "can reset to original order" in {
      val deck = new Deck(cards)
      deck.shuffle()
      deck.reset()
      deck.allCards shouldBe cards
    }
    
    "deal cards fairly to players and reduce deck size" in {
      val deck = new Deck(cards)
      val hands = deck.deal(numPlayers = 2, cardsEach = 2)
      hands should have length 2
      hands.foreach(_..length shouldBe 2)
      deck.size shouldBe cards.size - 4
      
      hands(0) shouldBe Seq(cards(0), cards(2))
      hands(1) shouldBe Seq(cards(1), cards(3))
    }
    
    "add a card to the top and to the bottom" in {
      val deck = new Deck(cards)
      val newCard = Card(Some("Yellow"), "9")
      deck.add(newCard)
      deck.peek shouldBe Some(newCard)
      deck.addToBottom(Seq(Card(Some("Black"), "X")))
      deck.allCards.last shouldBe Card(Some("Black"), "X")
    }
  }  
  
}