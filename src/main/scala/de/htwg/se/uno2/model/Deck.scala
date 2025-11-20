package de.htwg.se.uno2.model

import scala.util.Random

final case class Deck(cards: Vector[Card]) {
  def size: Int = cards.length
  def isEmpty: Boolean = cards.isEmpty

  def peek: Option[Card] = cards.headOption

  def draw(): (Card, Deck) = {
    require(cards.nonEmpty, "deck is empty")
    val c = cards.head
    (c, copy(cards = cards.tail))
  }

  def draw(n: Int): (Vector[Card], Deck) = {
    require(n >= 0, "n < 0")
    val (take, rest) = cards.splitAt(n)
    (take, copy(cards = rest))
  }

  def add(card: Card): Deck =
    copy(cards = card +: cards)

  def addToBottom(cardsToAdd: Seq[Card]): Deck =
    copy(cards = cards ++ cardsToAdd)

  def shuffle(): Deck =
    copy(cards = Random.shuffle(cards))

  def deal(numPlayers: Int, cardsEach: Int): (Vector[Vector[Card]], Deck) = {
    require(numPlayers > 0 && cardsEach >= 0, "invalid deal args")
    var d = this
    val hands0 = Vector.fill(numPlayers)(Vector.empty[Card])

    val hands = (0 until cardsEach).foldLeft(hands0) { (acc, _) =>
      acc.indices.foldLeft(acc) { (acc2, p) =>
        if d.isEmpty then acc2
        else {
          val (c, d2) = d.draw()
          d = d2
          acc2.updated(p, acc2(p) :+ c)
        }
      }
    }
    (hands, d)
  }
}

object Deck {
  val empty: Deck = Deck(Vector.empty)
  def from(all: Seq[Card]): Deck = Deck(all.toVector)
}
