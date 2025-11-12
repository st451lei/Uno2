package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._

import scala.util.Random

class Controller:

  private val allCards: Seq[Card] = for
    color <- Seq(Color.Red, Color.Yellow, Color.Green, Color.Blue)
    number <- 0 to 9
  yield Card(color, Rank.Number(number))

  private var deck = new Deck(Random.shuffle(allCards))
  private var discardPile: Vector[Card] = Vector.empty
  private var players: Vector[Player] = Vector(
    Player("Alice", Vector.empty),
    Player("Bob", Vector.empty)
  )
  private var currentPlayerIndex: Int = 0

  def startGame(): Unit =
    val hands = deck.deal(players.size, 7)
    players = players.zip(hands).map { case (p,h) => p.copy(hand = h.toVector)}
    discardPile = Vector(deck.draw().get)
    println(s"Spiel gestartet! Erste Karte: ${discardPile.head}")

  def currentPlayer: Player = players(currentPlayerIndex)

  def drawCard(): Unit =
    val (updatedPlayer, remainingDeck) = currentPlayer.draw(1, deck.allCards.toVector)
    players = players.updated(currentPlayerIndex, updatedPlayer)
    deck = new Deck(remainingDeck)
    println(s"${currentPlayer.name} zieht eine Karte.")

  def playCard(index: Int): Unit =
    if index < 0 || index >= currentPlayer.hand.size then
      println("Ungültiger Kartenindex!")
      return

    val (card, newPlayer) = currentPlayer.playAt(index)
    val topCard = discardPile.last
    if Card.canPlayOn(topCard, card) then
      discardPile = discardPile :+ card
      players = players.updated(currentPlayerIndex, newPlayer)
      println(s"${currentPlayer.name} spielt: $card")
      nextPlayer()
    else
      println(s"Diese Karte kann nicht auf $topCard gespielt werden!")

  def nextPlayer(): Unit =
    currentPlayerIndex = (currentPlayerIndex +1) % players.size

  def gameStateToString: String =
    val current = currentPlayer
    val handStr = current.hand.zipWithIndex.map { case (c,i) => s"[$i] $c" }.mkString(", ")
    val topCard = discardPile.lastOption.map(_.toString).getOrElse("Keine Karte")
    s"""
       |Aktueller Spieler: ${current.name}
       |Oberste Karte: $topCard
       |Hand: $handStr
       |Deckgröße: ${deck.size}
       |""".stripMargin