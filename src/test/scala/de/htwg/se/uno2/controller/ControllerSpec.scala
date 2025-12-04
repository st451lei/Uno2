package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ControllerSpec extends AnyWordSpec with Matchers:

  "A Controller" should {

    "return fallback text before game is started" in {
      val controller = new Controller
      controller.gameStateToString shouldBe "Noch kein Spiel gestartet"
    }

    "initialize a game correctly with startGame()" in {
      val controller = new Controller
      controller.startGame(Seq("Alice", "Bob"))

      val current = controller.currentPlayer

      Seq("Alice", "Bob") should contain (current.name)
      current.hand.size shouldBe 7

      val s = controller.gameStateToString
      s should include ("Aktueller Spieler")
      s should include ("Deckgröße")
    }

    "let the current player draw a card" in {
      val controller = new Controller
      controller.startGame(Seq("Alice", "Bob"))

      val before = controller.currentPlayer.hand.size
      controller.drawCard
      controller.currentPlayer.hand.size shouldBe (before + 1)
    }

    "not change hand size when an invalid index is played" in {
      val controller = new Controller
      controller.startGame(Seq("Alice", "Bob"))

      val before = controller.currentPlayer.hand.size
      controller.playCard(99)
      controller.currentPlayer.hand.size shouldBe before
    }

    "handle a valid index without crashing (card is either played or rejected)" in {
      val controller = new Controller
      controller.startGame(Seq("Alice", "Bob"))

      val before = controller.currentPlayer.hand.size
      controller.playCard(0)
      
      controller.currentPlayer.hand.size should (be <= before)
    }

    "expose awaiting-color flag consistently with GameState" in {
      val controller = new Controller
      controller.startGame(Seq("Alice", "Bob"))
      
      controller.isAwaitingColorChoise shouldBe false
      
      controller.chooseColor("r")
      controller.isAwaitingColorChoise shouldBe false
    }

    "produce a readable state string at any time" in {
      val controller = new Controller
      controller.startGame(Seq("Alice", "Bob"))

      val s1 = controller.gameStateToString
      s1 should include ("Aktueller Spieler")

      controller.drawCard
      val s2 = controller.gameStateToString
      s2 should include ("Aktueller Spieler")
    }

    "use the provided GameStateFactory" in {
      val customState = GameState(
        deck = Deck.empty,
        discard = Vector(Card(Color.Red, Rank.Number(1))),
        players = Vector(Player("Only", Vector.empty)),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = false,
        ruleSet = ClassicRuleSet
      )

      val factory = new GameStateFactory:
        override def create(names: Seq[String]): GameState = customState

      val controller = new Controller(factory)
      controller.startGame(Seq("ignored"))
      
      controller.currentPlayer.name shouldBe "Only"
      controller.gameStateToString should include ("Oberste Karte")
    }
    
    "apply AwaitingColorState behaviour correctly" in {
      val top = Card(Color.Red, Rank.Number(5))
      val wild = Card(Color.Black, Rank.Wild)
      val p1 = Player("P1", Vector.empty)
      val p2 = Player("P2", Vector.empty)
      
      val baseState = GameState(
        deck = Deck.empty,
        discard = Vector(top,wild),
        players = Vector(p1, p2),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = true,
        ruleSet = ClassicRuleSet
      )
      
      val factory = new GameStateFactory:
        override def create(names: Seq[String]): GameState = baseState
        
      val controller = new Controller(factory)
      controller.startGame(Seq("P1", "P2"))
      
      controller.isAwaitingColorChoise shouldBe true
      
      val beforePlayer = controller.currentPlayer
      val beforeHand = beforePlayer.hand.size
      
      AwaitingColorState.playCard(controller, 0)
      AwaitingColorState.drawCard(controller)
      
      controller.currentPlayer shouldBe beforePlayer
      controller.currentPlayer.hand.size shouldBe beforeHand
      
      AwaitingColorState.chooseColor(controller, "g")
      controller.isAwaitingColorChoise shouldBe false
    }
  }
