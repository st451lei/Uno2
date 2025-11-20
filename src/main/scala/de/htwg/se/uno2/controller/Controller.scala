package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.model.Color.*
import de.htwg.se.uno2.model.Rank.*
import de.htwg.se.uno2.util.Observable

class Controller extends Observable:

  private var deck: Deck = Deck(Vector.empty)
  private var discard: Vector[Card] = Vector.empty
  private var players: Vector[Player] = Vector.empty
  private var currentPlayerIndex: Int = 0
  private var chosenColor: Option[Color] = None
  private var awaitingColor: Boolean = false
  
  def startGame(names: Seq[String]): Unit =
    require(names.nonEmpty, "Soll mindestens 1 Spieler sein")
    
    deck = Deck.from(fullUnoDeck).shuffle()
    
    players = names.map(n => Player(n, Vector.empty)).toVector
    val (hands, deckAfterDeal) = deck.deal(players.size, 7)
    players = players.indices.map(i => players(i).copy(hand = hands(i))).toVector
    deck = deckAfterDeal
    
    val (first, d2) = drawFirstNonWild(deck)
    discard = Vector(first)
    deck = d2
    
    currentPlayerIndex = 0
    chosenColor = None
    awaitingColor = false

    notifyObservers

  def currentPlayer: Player = players(currentPlayerIndex)

  def drawCard(): Unit =
    if awaitingColor then
      notifyObservers
    else
      val (c, d2) = deck.draw()
      deck = d2
      val cur = currentPlayer
      val updated = cur.copy(hand = cur.hand :+ c)
      players = players.updated(currentPlayerIndex, updated)
      notifyObservers

  def playCard(index: Int): Unit =
    if awaitingColor then
      notifyObservers
    else if index < 0 || index >= currentPlayer.hand.size then
      notifyObservers
    else
      val (card, newPl) = currentPlayer.playAt(index)
      val top = discard.lastOption
      val canPlay = top.forall(t => Card.canPlayOn(t, card, chosenColor))
      if !canPlay then
        notifyObservers
      else
        discard = discard :+ card
        players = players.updated(currentPlayerIndex, newPl)

        card.rank match
          case Rank.Wild | Rank.WildDrawFour =>
            chosenColor = None
            awaitingColor = true
            notifyObservers
          case _ =>
            chosenColor = None
            nextPlayer()
            notifyObservers
  def chooseColor(token: String): Unit =
    if !awaitingColor then
      notifyObservers
    else
      parseColor(token) match
        case Some(col) =>
          chosenColor = Some(col)
          awaitingColor = false
          nextPlayer()
          notifyObservers
        case None =>
          notifyObservers

  def isAwaitingColorChoise: Boolean = awaitingColor

  def nextPlayer(): Unit =
    if players.nonEmpty then
      currentPlayerIndex = (currentPlayerIndex + 1) % players.size

  def gameStateToString: String =
    val current = currentPlayer
    val handStr = current.hand.zipWithIndex.map { case (c, i) => s"[$i] $c" }.mkString(", ")
    val topCard = discard.lastOption.map(_.toString).getOrElse("Keine Karte")
    val colorInfo =
      chosenColor.map(c => s"Gewählte Farbe: $c")
        .orElse(if awaitingColor then Some("Gewählte Farbe: -(choose with: color r|y|g|b)") else Some("Gewählte Farbe: -"))
        .getOrElse("")

    s"""
       |Aktueller Spieler: ${current.name}
       |Oberste Karte: $topCard
       |Hand: $handStr
       |Deckgröße: ${deck.size}
       |Befehl: ${if awaitingColor then "color r|y|g|b | quit" else "play <i> | draw | quit"}
       |""".stripMargin
  
  private def fullUnoDeck: Seq[Card] =
    val colors  = List(Red, Yellow, Green, Blue)
    val numbers = for c <- colors; n <- 0 to 9; count <- if n == 0 then 1 to 1 else 1 to 2 yield Card(c, Number(n))
    val actions = for c <- colors; r <- List(Skip, Reverse, DrawTwo); _ <- 1 to 2 yield Card(c, r)
    val wilds   = List.fill(4)(Card(Black, Wild)) ++ List.fill(4)(Card(Black, WildDrawFour))
    (numbers ++ actions ++ wilds).toVector

  private def drawFirstNonWild(d: Deck): (Card, Deck) =
    val (c, d2) = d.draw()
    c.rank match
      case Rank.Wild | Rank.WildDrawFour => drawFirstNonWild(d2)
      case _ => (c, d2)
      
  private def parseColor(token: String): Option[Color] =
    token.trim.toLowerCase match
      case "r" | "red" | "rot" => Some(Color.Red)
      case "y" | "yellow" | "gelb" => Some(Color.Yellow)
      case "g" | "green" | "grün" | "gruen" => Some(Color.Green)
      case "b" | "blue" | "blau" => Some(Color.Blue)
      case _ => None
