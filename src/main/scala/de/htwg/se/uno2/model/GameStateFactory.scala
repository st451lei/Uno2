package de.htwg.se.uno2.model

trait GameStateFactory:
  def create(names: Seq[String]): GameState
  
object DefaultGameStateFactory extends GameStateFactory:
  
  def create(names: Seq[String]): GameState =
    require(names.nonEmpty, "Soll mindestens 1 Spieler sein")
    
    val deck = Deck.from(fullUnoDeck).shuffle()

    val basePlayers = names.map(n => Player(n, Vector.empty)).toVector
    val (hands, deckAfterDeal) = deck.deal(basePlayers.size, 7)
    val players = basePlayers.indices.map(i => basePlayers(i).copy(hand = hands(i))).toVector

    val (first, deck2) = drawFirstNonWild(deckAfterDeal)
    
    GameState(
      deck = deck2,
      discard = Vector(first),
      players = players,
      currentPlayerIndex = 0,
      chosenColor = None,
      awaitingColor = false,
      ruleSet = ClassicRuleSet
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
      val (c,d2) = d.draw()
      c.rank match
        case Rank.Wild | Rank.WildDrawFour => drawFirstNonWild(d2)
        case _ => (c,d2)
        
object ColorOnlyGameStateFactory extends GameStateFactory:
  override def create(names: Seq[String]): GameState =
    val ClassicState = DefaultGameStateFactory.create(names)
    ClassicState.copy(ruleSet = ColorOnlyRuleSet)
    