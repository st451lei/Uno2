package de.htwg.se.uno2.model

trait RuleSet:
  def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean
  
object ClassicRuleSet extends RuleSet:
  override def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean =
    Card.canPlayOn(top, next, chosenColor)