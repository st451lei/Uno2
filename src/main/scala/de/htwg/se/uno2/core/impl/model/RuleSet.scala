package de.htwg.se.uno2.core.impl.model

trait RuleSet:
  def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean
  
object ClassicRuleSet extends RuleSet:
  override def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean =
    Card.canPlayOn(top, next, chosenColor)
    
object ColorOnlyRuleSet extends RuleSet:
  override def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean =
    val activeColor = chosenColor.getOrElse(top.color)
    (next.color == activeColor) || (next.rank == Rank.Wild) || (next.rank == Rank.WildDrawFour)