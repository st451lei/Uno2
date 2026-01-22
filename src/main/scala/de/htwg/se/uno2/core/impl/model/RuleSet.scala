package de.htwg.se.uno2.core.impl.model

trait RuleSet:
  def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean
  def hasNonWildPlayableCard(top: Card, chosenColor: Option[Color], hand: Vector[Card]): Boolean
  def canPlayWild(top: Card, next: Card, chosenColor: Option[Color], hand: Vector[Card]): Boolean

object ClassicRuleSet extends RuleSet:
  override def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean =
    Card.canPlayOn(top, next, chosenColor)
    
  override def hasNonWildPlayableCard(top: Card, chosenColor: Option[Color], hand: Vector[Card]): Boolean =
    hand.exists { c =>
      c.rank match
        case Rank.Wild | Rank.WildDrawFour => false
        case _ => canPlayOn(top, c, chosenColor)
    }
  override def canPlayWild(top: Card, next: Card, chosenColor: Option[Color], hand: Vector[Card]): Boolean =
    next.rank match
      case Rank.Wild | Rank.WildDrawFour =>
        !hasNonWildPlayableCard(top, chosenColor, hand) && canPlayOn(top, next, chosenColor)
      case _ =>
        canPlayOn(top, next, chosenColor)
    
object ColorOnlyRuleSet extends RuleSet:
  override def canPlayOn(top: Card, next: Card, chosenColor: Option[Color]): Boolean =
    val activeColor = chosenColor.getOrElse(top.color)
    (next.color == activeColor) || (next.rank == Rank.Wild) || (next.rank == Rank.WildDrawFour)

  override def hasNonWildPlayableCard(top: Card, chosenColor: Option[Color], hand: Vector[Card]): Boolean =
    hand.exists { c =>
      c.rank match
        case Rank.Wild | Rank.WildDrawFour => false
        case _ => canPlayOn(top, c, chosenColor)
    }

  override def canPlayWild(top: Card, next: Card, chosenColor: Option[Color], hand: Vector[Card]): Boolean =
    next.rank match
      case Rank.Wild | Rank.WildDrawFour =>
        !hasNonWildPlayableCard(top, chosenColor, hand) && canPlayOn(top, next, chosenColor)
      case _ =>
        canPlayOn(top, next, chosenColor)  