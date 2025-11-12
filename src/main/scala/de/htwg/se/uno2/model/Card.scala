package de.htwg.se.uno2.model

enum Color:
  case Red, Yellow, Green, Blue, Black

enum Rank:
  case Number(n: Int)
  case Skip, Reverse, DrawTwo
  case Wild, WildDrawFour

final case class Card(color: Color, rank: Rank) {}

object Card:
  def canPlayOn(top: Card, next: Card, chosenColor: Option[Color] = None): Boolean =
    val activeColor = chosenColor.getOrElse(top.color)
    (next.color == activeColor)
      || (next.rank == top.rank)
      || (next.rank == Rank.Wild)
      || (next.rank == Rank.WildDrawFour)