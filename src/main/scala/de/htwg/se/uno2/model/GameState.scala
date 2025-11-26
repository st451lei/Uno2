package de.htwg.se.uno2.model


final case class GameState(
  deck: Deck,
  discard: Vector[Card],
  players: Vector[Player],
  currentPlayerIndex: Int,
  chosenColor: Option[Color],
  awaitingColor: Boolean,
  ruleSet: RuleSet
  ):

  def currentPlayer: Player = players(currentPlayerIndex)

  def isAwaitingColorChoise: Boolean = awaitingColor

  def nextPlayer: GameState =
    if players.nonEmpty then
      copy(currentPlayerIndex = (currentPlayerIndex + 1) % players.size)
    else this

  def drawCard: GameState =
    if awaitingColor then this
    else
      val (c, d2) = deck.draw()
      val cur = currentPlayer
      val updated = cur.copy(hand = cur.hand :+ c)
      copy(
        deck = d2,
        players = players.updated(currentPlayerIndex, updated)
      )

  def playCard(index: Int): GameState =
    if awaitingColor then this
    else if index < 0 || index >= currentPlayer.hand.size then this
    else
      val (card, newPl) = currentPlayer.playAt(index)
      val top = discard.lastOption
      val canPlay = top.forall(t => ruleSet.canPlayOn(t, card, chosenColor))
      if !canPlay then this
      else
        val newDiscard = discard :+ card
        val newPlayers = players.updated(currentPlayerIndex, newPl)

        card.rank match
          case Rank.Wild | Rank.WildDrawFour =>
            copy(
              discard = newDiscard,
              players = newPlayers,
              chosenColor = None,
              awaitingColor = true
            )
          case _ =>
            nextPlayer.copy(
              discard = newDiscard,
              players = newPlayers,
              chosenColor = None
            )

  def chooseColor(token: String): GameState =
    if !awaitingColor then this
    else
      GameState.parseColor(token) match
        case Some(col) =>
          nextPlayer.copy(
            chosenColor = Some(col),
            awaitingColor = false
          )
        case None =>
          this

  def toDisplayString: String =
    val current = currentPlayer
    val handStr = current.hand.zipWithIndex
      .map { case (c, i) => s"[$i] $c"}
      .mkString(", ")
    val topCard = discard.lastOption.map(_.toString).getOrElse("Keine Karte")
    s"""
       |Aktueller Spieler: ${current.name}
       |Oberste Karte: $topCard
       |Hand: $handStr
       |Deckgröße: ${deck.size}
       |Befehl: ${if awaitingColor then "color r|y|g|b | quit" else "play <i> | draw | quit"}
       |""".stripMargin
    
object GameState:

  def parseColor(token: String): Option[Color] =
    token.trim.toLowerCase match
      case "r" | "red" | "rot" => Some(Color.Red)
      case "y" | "yellow" | "gelb" => Some(Color.Yellow)
      case "g" | "green" | "grün" | "gruen" => Some(Color.Green)
      case "b" | "blue" | "blau" => Some(Color.Blue)
      case _ => None