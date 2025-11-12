package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.util.Observable

import scala.util.Random

class Controller extends Observable:
  
  private var deck: Deck = Deck.empty
  private var discard: Vector[Card] = Vector.empty
  private var players: Vector[Player] = Vector.empty
  private var currentPlayerIndex: Int = 0
  private var chosenColor: Option[Color] = None

  def startGame(names: Seq[String]): Unit =
    require(names.nonEmpty, "Soll mindestens 2 Spieler sein")
    players = names.map(n => Player(n, Vector.empty)).toVector
    
    val deck0 = fullUnoDeck.shuffle
    
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