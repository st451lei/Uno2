package de.htwg.se.uno2

import scala.util.Random
class Deck(initialCards: Seq[Card]) {

  private val original: Vector[Card] = initialCards.toVector
  private var cards: Vector[Card] = original
  
  def size: Int = cards.length
  
  def isEmpty: Boolean = cards.isEmpty
  
  def peek: Option[Card] = cards.headOption
  def draw(): Option[Card] = {
    cards.headOption match {
      case Some(c) => 
        cards = cards.tail
        Some(c)
      case None => None
    }
  }
  
  def draw(n: Int): Seq[Card] = {
    val taken = cards.take(n)
    cards = cards.drop(n)
    taken
  }
  
  def add(card: Card): Unit = {
    cards = card +: cards
  }
  def shuffle(): Unit = {
    cards = Random.shuffle(cards)
  }
  
  def reset(): Unit = {
    cards = original
  }
  
  def addToBottom(cardsToAdd: Seq[Card]): Unit = {
    cards = cards ++ cardsToAdd
  }
  
  def deal(numPlayers: Int, cardsEach: Int): Seq[Seq[Card]] = {
    require(numPlayers > 0 && cardsEach >= 0, "ivalid args")
    val hands = Array.fill(numPlayers)(Vector.empty[Card])
    for (i <- 0 until cardsEach; p <- 0 until numPlayers) {
      if (cards.nonEmpty) {
        hands(p) = hands(p) :+ cards.head
        cards = cards.tail
      }
    }
    hands.map(_.toSeq).toSeq
  }
  
  def allCards: Seq[Card] = cards
}