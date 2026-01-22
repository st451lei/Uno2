package de.htwg.se.uno2.fileio.xml

import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.core.impl.model.*

import java.io.{File, PrintWriter}
import scala.util.Try
import scala.xml.{Elem, Node, PrettyPrinter, XML}

class XmlFileIO extends FileIOInterface:
  private val filename = "uno2.xml"

  override def save(state: GameState): Unit =
    val xml = gameStateToXml(state)
    val pp = new PrettyPrinter(120, 2)
    val pw = new PrintWriter(new File(filename))
    try pw.write(pp.format(xml))
    finally pw.close()

  override def load(): Option[GameState] =
    Try {
      val xml = XML.loadFile(filename)
      gameStateFromXml(xml)
    }.toOption

  private def gameStateToXml(gs: GameState): Elem =
    <uno2>
      <meta>
        <ruleSet>{ruleSetToString(gs.ruleSet)}</ruleSet>
        <currentPlayerIndex>{gs.currentPlayerIndex}</currentPlayerIndex>
        <direction>{gs.direction}</direction>
        <awaitingColor>{gs.awaitingColor}</awaitingColor>
        <chosenColor>{gs.chosenColor.map(_.toString).getOrElse("")}</chosenColor>
        <pendingWild>{gs.pendingWild.map(rankToString).getOrElse("")}</pendingWild>
        <winnerName>{gs.winnerName.getOrElse("")}</winnerName>
        <pendingNumber>{gs.pendingNumber.map(_.toString).getOrElse("")}</pendingNumber>
      </meta>

      <deck>
        { gs.deck.cards.map(cardToXml) }
      </deck>

      <discard>
        { gs.discard.map(cardToXml) }
      </discard>

      <players>
        { gs.players.map(playerToXml) }
      </players>
    </uno2>

  private def gameStateFromXml(root: Node): GameState =
    val meta = (root \ "meta").head

    def txt(tag: String): String = (meta \ tag).text.trim

    val rs = ruleSetFromString(txt("ruleSet"))
    val cpi = txt("currentPlayerIndex").toInt
    val dir = txt("direction").toInt
    val awaiting = txt("awaitingColor").toBoolean

    val chosenColor =
      txt("chosenColor") match
        case "" => None
        case s  => Some(colorFromString(s))

    val pendingWild =
      txt("pendingWild") match
        case "" => None
        case s  => Some(rankFromString(s))

    val winnerName =
      txt("winnerName") match
        case "" => None
        case s  => Some(s)

    val deckCards = (root \ "deck" \ "card").map(cardFromXml).toVector
    val discard   = (root \ "discard" \ "card").map(cardFromXml).toVector
    val players   = (root \ "players" \ "player").map(playerFromXml).toVector
    
    val pendingNumber =
      txt("pendingNumber") match
        case "" => None
        case s => Some(s.toInt)

    GameState(
      deck = Deck(deckCards),
      discard = discard,
      players = players,
      currentPlayerIndex = cpi,
      chosenColor = chosenColor,
      awaitingColor = awaiting,
      ruleSet = rs,
      direction = dir,
      pendingWild = pendingWild,
      winnerName = winnerName,
      pendingNumber = pendingNumber
    )

  private def cardToXml(c: Card): Elem =
    c.rank match
      case Rank.Number(n) =>
          <card color={c.color.toString} kind="Number" n={n.toString}/>
      case other =>
          <card color={c.color.toString} kind={rankToString(other)} n=""/>

  private def cardFromXml(n: Node): Card =
    val color = colorFromString(n \@ "color")
    val kind  = (n \@ "kind").trim
    val numS  = (n \@ "n").trim

    val rank: Rank =
      kind match
        case "Number" => Rank.Number(numS.toInt)
        case "Skip" => Rank.Skip
        case "Reverse" => Rank.Reverse
        case "DrawTwo" => Rank.DrawTwo
        case "Wild" => Rank.Wild
        case "WildDrawFour" => Rank.WildDrawFour
        case other => throw new IllegalArgumentException(s"Unknown rank: $other")

    Card(color, rank)

  private def playerToXml(p: Player): Elem =
    <player name={p.name}>
      <hand>
        { p.hand.map(cardToXml) }
      </hand>
    </player>

  private def playerFromXml(n: Node): Player =
    val name = (n \@ "name").trim
    val hand = (n \ "hand" \ "card").map(cardFromXml).toVector
    Player(name, hand)

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
