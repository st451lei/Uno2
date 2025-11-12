package de.htwg.se.uno2.model

import scala.util.Random

final case class Deck(cards: Vector[Card]) {
  
  def size: Int = cards.length

  def isEmpty: Boolean = cards.isEmpty

  def peek: Option[Card] = cards.headOption
  
  def draw(): (Card, Deck) = 
    val c = cards.head
    (c, copy(cards = cards.tail))
  

  def draw(n: Int): (Vector[Card], Deck) = 
    val (take, rest) = cards.splitAt(n)
    (take, copy(cards = rest))
  

  def add(card: Card): Deck =
    copy(cards = card +: cards)
  
  def shuffle(): Deck =
    copy(cards = Random.shuffle(cards))
  
  def addToBottom(cardsToAdd: Seq[Card]): Deck =
    copy(cards = cards ++ cardsToAdd)
  

  def deal(numPlayers: Int, cardsEach: Int): (Vector[Vector[Card]], Deck) = 
    require(numPlayers > 0 && cardsEach >= 0, "invalid args")
    var d = this
    val hands = Vector.tabulate(numPlayers)(_ => Vector.empty[Card])
    val dealt = (0 until cardsEach).foldLeft(hands) { (acc, _) => 
      acc.indices.foldLeft(acc) { (acc2, p) =>
        if d.isEmpty then acc2
        else
          val (c, d2) = d.draw()
          d = d2
          acc2.updated(p, acc2(p) :+ c)
      }
    }      
    (dealt,d)
}