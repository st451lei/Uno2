package de.htwg.se.uno2.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class GameStateSpec extends AnyWordSpec with Matchers:

  "GameState.initial" should {

    "create players with 7 cards each and a non-wild top card" in {
      val names = Seq("Alice", "Bob")
      val state = GameState.initial(names)
      
      state.players.map(_.name) should contain theSameElementsAs names
      
      all(state.players.map(_.hand.size)) shouldBe 7
      
      state.discard.size shouldBe 1
      
      val top = state.discard.head
      top.rank should not be Rank.Wild
      top.rank should not be Rank.WildDrawFour
      
      val totalCards =
        state.deck.size +
          state.discard.size +
          state.players.map(_.hand.size).sum

      totalCards shouldBe 60
    }
  }

  "GameState.drawCard" should {

    "let current player draw one card when not awaiting color" in {
      val deck = Deck(Vector(
        Card(Color.Red, Rank.Number(1)),
        Card(Color.Green, Rank.Number(2))
      ))
      val p = Player("P1", Vector.empty)
      val state = GameState(
        deck = deck,
        discard = Vector.empty,
        players = Vector(p),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false
      )

      val s2 = state.drawCard

      s2.players.head.hand.size shouldBe 1
      s2.deck.size shouldBe 1
    }

    "do nothing if awaitingColor is true" in {
      val deck = Deck(Vector(Card(Color.Red, Rank.Number(1))))
      val p = Player("P1", Vector.empty)
      val state = GameState(
        deck = deck,
        discard = Vector.empty,
        players = Vector(p),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = true
      )

      val s2 = state.drawCard
      s2 shouldBe state
    }
  }

  "GameState.playCard" should {

    "ignore invalid index" in {
      val p = Player("P1", Vector(
        Card(Color.Red, Rank.Number(5))
      ))
      val state = GameState(
        deck = Deck.empty,
        discard = Vector.empty,
        players = Vector(p),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false
      )

      val s2 = state.playCard(5)
      s2 shouldBe state
    }

    "not play an unplayable card" in {
      val top  = Card(Color.Red,   Rank.Number(5))
      val card = Card(Color.Green, Rank.Skip) // нельзя играть
      val p    = Player("P1", Vector(card))

      val state = GameState(
        deck = Deck.empty,
        discard = Vector(top),
        players = Vector(p),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false
      )

      val s2 = state.playCard(0)
      s2 shouldBe state
    }

    "play a normal card and move to next player" in {
      val top  = Card(Color.Red, Rank.Number(5))
      val card = Card(Color.Red, Rank.Skip)  // совпадает цвет
      val p1   = Player("P1", Vector(card))
      val p2   = Player("P2", Vector.empty)

      val state = GameState(
        deck = Deck.empty,
        discard = Vector(top),
        players = Vector(p1, p2),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false
      )

      val s2 = state.playCard(0)

      s2.discard.last shouldBe card
      s2.players(0).hand shouldBe empty
      s2.currentPlayerIndex shouldBe 1
      s2.awaitingColor shouldBe false
      s2.chosenColor shouldBe None
    }

    "enter awaiting color mode when a Wild is played" in {
      val top  = Card(Color.Red, Rank.Number(5))
      val wild = Card(Color.Black, Rank.Wild)
      val p1   = Player("P1", Vector(wild))
      val p2   = Player("P2", Vector.empty)

      val state = GameState(
        deck = Deck.empty,
        discard = Vector(top),
        players = Vector(p1, p2),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false
      )

      val s2 = state.playCard(0)

      s2.discard.last shouldBe wild
      s2.players(0).hand shouldBe empty
      s2.awaitingColor shouldBe true
      s2.chosenColor shouldBe None
      s2.currentPlayerIndex shouldBe 0 // смена хода будет после chooseColor
    }
  }

  "GameState.chooseColor" should {

    "do nothing if not awaiting color" in {
      val state = GameState(
        deck = Deck.empty,
        discard = Vector.empty,
        players = Vector(Player("P1", Vector.empty)),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false
      )

      val s2 = state.chooseColor("r")
      s2 shouldBe state
    }

    "set chosenColor and advance to next player when valid color is chosen" in {
      val top  = Card(Color.Red, Rank.Number(5))
      val wild = Card(Color.Black, Rank.Wild)
      val p1   = Player("P1", Vector.empty)
      val p2   = Player("P2", Vector.empty)

      // состояние после уже сыгранного Wild
      val afterWild = GameState(
        deck = Deck.empty,
        discard = Vector(top, wild),
        players = Vector(p1, p2),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = true
      )

      val s2 = afterWild.chooseColor("g")

      s2.awaitingColor shouldBe false
      s2.chosenColor   shouldBe Some(Color.Green)
      s2.currentPlayerIndex shouldBe 1
    }

    "stay unchanged on invalid color token" in {
      val state = GameState(
        deck = Deck.empty,
        discard = Vector.empty,
        players = Vector(Player("P1", Vector.empty)),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = true
      )

      val s2 = state.chooseColor("invalid")
      s2 shouldBe state
    }
  }

  "GameState.toDisplayString" should {

    "contain basic information about the game" in {
      val p = Player("P1", Vector(Card(Color.Red, Rank.Number(1))))
      val state = GameState(
        deck = Deck.empty,
        discard = Vector(Card(Color.Green, Rank.Number(3))),
        players = Vector(p),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false
      )

      val s = state.toDisplayString
      s should include ("Aktueller Spieler: P1")
      s should include ("Oberste Karte")
      s should include ("Deckgröße")
    }
  }