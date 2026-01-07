package de.htwg.se.uno2.core.impl.model

final case class GameState(
  deck: Deck,
  discard: Vector[Card],
  players: Vector[Player],
  currentPlayerIndex: Int,
  chosenColor: Option[Color],
  awaitingColor: Boolean,
  ruleSet: RuleSet,
  direction: Int = 1,
  pendingWild: Option[Rank] = None,
  winnerName: Option[String] = None,
  pendingNumber: Option[Int] = None
  ):

  def isGameOver: Boolean = winnerName.isDefined

  def currentPlayer: Player = players(currentPlayerIndex)
  
  def isAwaitingColorChoise: Boolean = awaitingColor

  private def normIndex(i: Int): Int =
    if players.isEmpty then 0
    else ((i % players.size) + players.size) % players.size

  private def nextIndex(steps: Int = 1, dir: Int = direction): Int =
    if players.isEmpty then 0
    else normIndex(currentPlayerIndex + steps * dir)

  private def advance(steps: Int, dir: Int = direction): GameState =
    if players.isEmpty then this
    else copy(currentPlayerIndex = nextIndex(steps, dir))

  private def giveCards(playerIdx: Int, n: Int): GameState =
    if players.isEmpty || n <= 0 then this
    else
      val (cardsDrawn, d2) = deck.draw(n)
      val p = players(playerIdx)
      copy(
        deck = d2,
        players = players.updated(playerIdx, p.copy(hand = p.hand ++ cardsDrawn))
      )

  def nextPlayer: GameState =
    if players.nonEmpty then
      advance(1)
    else this

  def drawCard: GameState =
    if isAwaitingColorChoise || isGameOver then this
    else
      val (c, d2) = deck.draw()
      val cur = currentPlayer
      val updated = cur.copy(hand = cur.hand :+ c)
      copy(
        deck = d2,
        players = players.updated(currentPlayerIndex, updated)
      )

  def playCard(index: Int): GameState =
    if isAwaitingColorChoise || isGameOver then this
    else if index < 0 || index >= currentPlayer.hand.size then this
    else
      val handBefore = currentPlayer.hand
      val (card, newPl) = currentPlayer.playAt(index)
      
      val allowedByPendingNumber: Boolean =
        pendingNumber match
          case None => true
          case Some(expected) =>
            card.rank match
              case Rank.Number(n) => n == expected
              case _ => false
              
      if !allowedByPendingNumber then this
      else
        val top = discard.lastOption
        val canPlay = top.forall(t => ruleSet.canPlayWild(t, card, chosenColor, handBefore))
        if !canPlay then this
        else
          val newDiscard = discard :+ card
          val newPlayers = players.updated(currentPlayerIndex, newPl)

          val base = copy(
            discard = newDiscard,
            players = newPlayers,
            chosenColor = None,
            awaitingColor = false,
            pendingWild = None
          )

          if newPl.hand.isEmpty then
            base.copy(
              winnerName = Some(newPl.name),
              awaitingColor = false,
              chosenColor = None,
              pendingWild = None,
              pendingNumber = None
            )
  
          else 
            card.rank match
          
              case Rank.Wild | Rank.WildDrawFour =>
                base.copy(
                  awaitingColor = true,
                  pendingWild = Some(card.rank),
                  chosenColor = None,
                  pendingNumber = None
                )
          
              case Rank.Skip =>
                base.copy(pendingNumber = None).advance(2)
                
              case Rank.DrawTwo =>
                val target = base.nextIndex(1)
                base.copy(pendingNumber = None).giveCards(target, 2).advance(2)
                
              case Rank.Reverse =>
                val newDir = direction * -1
                val afterFlip = base.copy(direction = newDir, pendingNumber = None)
                if players.size <= 1 then afterFlip
                else if players.size == 2 then 
                  afterFlip.advance(2, dir = newDir)
                else
                  afterFlip.advance(1, dir = newDir)
                  
              case Rank.Number(n) =>
                val keepNumber =
                  pendingNumber match
                    case Some(expected) => expected
                    case None => n
                base.copy(pendingNumber = Some(keepNumber))

              case _ =>
                base.copy(pendingNumber = None).advance(1)

  def endTurn: GameState =
    if !canEndTurn then this
    else
      copy(pendingNumber = None).advance(1)

  def chooseColor(token: String): GameState =
    if !awaitingColor || isGameOver then this
    else
      GameState.parseColor(token) match
        case Some(col) =>
          val base = copy(
            chosenColor = Some(col),
            awaitingColor = false,
            pendingNumber = None
          )
          pendingWild match
            case Some(Rank.WildDrawFour) => 
              val target = base.nextIndex(1)
              base
                .giveCards(target, 4)
                .copy(pendingWild = None)
                .advance(2)

            case Some(Rank.Wild) | None =>
              base.copy(pendingWild = None).advance(1)
            case Some(_) =>
              base.copy(pendingWild = None).advance(1)
        case None =>
          this
  
  def currentHand: Vector[Card] =
    currentPlayer.hand

  def topDiscard: Option[Card] =
    discard.lastOption

  def deckSize: Int =
    deck.size

  def currentPlayerName: String =
    currentPlayer.name

  def canEndTurn: Boolean =
    pendingNumber.isDefined && !isAwaitingColorChoise && !isGameOver
    
object GameState:

  def parseColor(token: String): Option[Color] =
    token.trim.toLowerCase match
      case "r" | "red" | "rot" => Some(Color.Red)
      case "y" | "yellow" | "gelb" => Some(Color.Yellow)
      case "g" | "green" | "grün" | "gruen" => Some(Color.Green)
      case "b" | "blue" | "blau" => Some(Color.Blue)
      case _ => None