package de.htwg.se.uno2.model


final case class GameState(
  deck: Deck,
  discard: Vector[Card],
  players: Vector[Player],
  currentPlayerIndex: Int,
  chosenColor: Option[Color],
  awaitingColor: Boolean
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
      val canPlay = top.forall(t => Card.canPlayOn(t, card, chosenColor))
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
      
  def initial(names: Seq[String]): GameState =
    require(names.nonEmpty, "Soll mindestens 1 Spieler sein")
        
    var deck = Deck.from(fullUnoDeck).shuffle()
        
    val basePlayers = names.map(n => Player(n, Vector.empty)).toVector
    val (hands, deckAfterDeal) = deck.deal(basePlayers.size, 7)
    val players = basePlayers.indices
      .map(i => basePlayers(i).copy(hand = hands(i)))
      .toVector
        
    val (first, deck2) = drawFirstNonWild(deckAfterDeal)
        
    GameState(
      deck = deck2,
      discard = Vector(first),
      players = players,
      currentPlayerIndex = 0,
      chosenColor = None,
      awaitingColor = false
    )
    
  private def fullUnoDeck: Seq[Card] =
    import de.htwg.se.uno2.model.Color.*
    import de.htwg.se.uno2.model.Rank.*
    val colors = List(Red, Yellow, Green, Blue)
    val numbers = for c <- colors; n <- 0 to 9 yield Card(c, Number(n))
    val actions = for c <- colors; r <- List(Skip, Reverse, DrawTwo) yield Card(c, r)
    val wilds = List.fill(4)(Card(Black, Wild)) ++ List.fill(4)(Card(Black, WildDrawFour))
    (numbers ++ actions ++ wilds).toVector
    
  private def drawFirstNonWild(d: Deck): (Card, Deck) =
    val (c, d2) = d.draw()
    c.rank match
      case Rank.Wild | Rank.WildDrawFour => drawFirstNonWild(d2)
      case _ => (c, d2)
      
  def parseColor(token: String): Option[Color] =
    token.trim.toLowerCase match
      case "r" | "red" | "rot" => Some(Color.Red)
      case "y" | "yellow" | "gelb" => Some(Color.Yellow)
      case "g" | "green" | "grün" | "gruen" => Some(Color.Green)
      case "b" | "blue" | "blau" => Some(Color.Blue)
      case _ => None