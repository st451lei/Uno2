package de.htwg.se.uno2.fileio.json

import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.core.impl.model.*

import play.api.libs.json.*
import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.Try

class JsonFileIO extends FileIOInterface:
  private val filename = "uno2.json"

  private case class RankData(kind: String, n: Option[Int] = None)
  private case class CardData(color: String, rank: RankData)
  private case class PlayerData(name: String, hand: Vector[CardData])
  private case class GameStateData(
                                    deck: Vector[CardData],
                                    discard: Vector[CardData],
                                    players: Vector[PlayerData],
                                    currentPlayerIndex: Int,
                                    chosenColor: Option[String],
                                    awaitingColor: Boolean,
                                    ruleSet: String,
                                    direction: Int,
                                    pendingWild: Option[String],
                                    winnerName: Option[String]
                                  )

  private given OFormat[RankData] = Json.format[RankData]
  private given OFormat[CardData] = Json.format[CardData]
  private given OFormat[PlayerData] = Json.format[PlayerData]
  private given OFormat[GameStateData] = Json.format[GameStateData]

  override def save(state: GameState): Unit =
    val data = toData(state)
    val json = Json.prettyPrint(Json.toJson(data))
    val pw = new PrintWriter(new File(filename))
    try pw.write(json)
    finally pw.close()

  override def load(): Option[GameState] =
    Try {
      val src = Source.fromFile(filename)
      val str = try src.mkString finally src.close()

      Json.parse(str).asOpt[GameStateData].map(fromData).get
    }.toOption

  private def toData(gs: GameState): GameStateData =
    GameStateData(
      deck = gs.deck.cards.map(cardToData),
      discard = gs.discard.map(cardToData),
      players = gs.players.map(p => PlayerData(p.name, p.hand.map(cardToData))),
      currentPlayerIndex = gs.currentPlayerIndex,
      chosenColor = gs.chosenColor.map(_.toString),
      awaitingColor = gs.awaitingColor,
      ruleSet = ruleSetToString(gs.ruleSet),
      direction = gs.direction,
      pendingWild = gs.pendingWild.map(rankToString),
      winnerName = gs.winnerName
    )

  private def fromData(d: GameStateData): GameState =
    GameState(
      deck = Deck(d.deck.map(cardFromData)),
      discard = d.discard.map(cardFromData),
      players = d.players.map(p => Player(p.name, p.hand.map(cardFromData))),
      currentPlayerIndex = d.currentPlayerIndex,
      chosenColor = d.chosenColor.map(colorFromString),
      awaitingColor = d.awaitingColor,
      ruleSet = ruleSetFromString(d.ruleSet),
      direction = d.direction,
      pendingWild = d.pendingWild.map(rankFromString),
      winnerName = d.winnerName
    )

  private def cardToData(c: Card): CardData =
    CardData(c.color.toString, rankToData(c.rank))

  private def cardFromData(d: CardData): Card =
    Card(colorFromString(d.color), rankFromData(d.rank))

  private def rankToData(r: Rank): RankData =
    r match
      case Rank.Number(n)    => RankData("Number", Some(n))
      case Rank.Skip         => RankData("Skip")
      case Rank.Reverse      => RankData("Reverse")
      case Rank.DrawTwo      => RankData("DrawTwo")
      case Rank.Wild         => RankData("Wild")
      case Rank.WildDrawFour => RankData("WildDrawFour")

  private def rankFromData(d: RankData): Rank =
    d.kind match
      case "Number"       => Rank.Number(d.n.getOrElse(0))
      case "Skip"         => Rank.Skip
      case "Reverse"      => Rank.Reverse
      case "DrawTwo"      => Rank.DrawTwo
      case "Wild"         => Rank.Wild
      case "WildDrawFour" => Rank.WildDrawFour
      case other          => throw new IllegalArgumentException(s"Unknown rank: $other")

  private def ruleSetToString(rs: RuleSet): String =
    rs match
      case ClassicRuleSet   => "classic"
      case ColorOnlyRuleSet => "colorOnly"
      case _                => "classic"

  private def ruleSetFromString(s: String): RuleSet =
    s.toLowerCase match
      case "coloronly" => ColorOnlyRuleSet
      case _           => ClassicRuleSet

  private def colorFromString(s: String): Color =
    s match
      case "Red"    => Color.Red
      case "Yellow" => Color.Yellow
      case "Green"  => Color.Green
      case "Blue"   => Color.Blue
      case "Black"  => Color.Black
      case other    => throw new IllegalArgumentException(s"Unknown color: $other")

  private def rankToString(r: Rank): String =
    r match
      case Rank.Number(_)    => "Number"
      case Rank.Skip         => "Skip"
      case Rank.Reverse      => "Reverse"
      case Rank.DrawTwo      => "DrawTwo"
      case Rank.Wild         => "Wild"
      case Rank.WildDrawFour => "WildDrawFour"

  private def rankFromString(s: String): Rank =
    s match
      case "Wild"         => Rank.Wild
      case "WildDrawFour" => Rank.WildDrawFour
      case "Skip"         => Rank.Skip
      case "Reverse"      => Rank.Reverse
      case "DrawTwo"      => Rank.DrawTwo
      case other          => throw new IllegalArgumentException(s"Unknown rank: $other")
