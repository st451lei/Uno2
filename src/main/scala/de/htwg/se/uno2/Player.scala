package de.htwg.se.uno2.model

final case class Player(name: String, hand: Vector[Card]):
  def size: Int = hand.size
  
  def draw(n: Int, drawPile: Vector[Card]): (Player, Vector[Card]) =
    val (take, rest) = drawPile.splitAt(n)
    (copy(hand = hand ++ take), rest)
    
  def playAt(i: Int): (Card, Player) =
    val (l, r) = hand.splitAt(i)
    val card = r.head
    (card, copy(hand = l ++ r.tail))