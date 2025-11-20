package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
import de.htwg.se.uno2.model.Color.*
import de.htwg.se.uno2.model.Rank.*
import de.htwg.se.uno2.controller.Controller
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ControllerSpec extends AnyWordSpec with Matchers:

  "A Controller" should {

    "initialize a game correctly with startGame()" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))

      val players = Seq("Alice", "Bob")
      players should contain (controller.currentPlayer.name)
      controller.gameStateToString should include ("Aktueller Spieler")

      controller.currentPlayer.hand.size shouldBe 7
    }

    "allow a player to draw a card" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val before = controller.currentPlayer.hand.size
      controller.drawCard()
      controller.currentPlayer.hand.size shouldBe (before + 1)
    }

    "advance to the next player" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val first = controller.currentPlayer.name
      controller.nextPlayer()
      controller.currentPlayer.name should not equal first
    }
    
    "not allow invalid card index to be played" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val before = controller.currentPlayer.hand.size
      controller.playCard(99)
      controller.currentPlayer.hand.size shouldBe before
    }
    
    "play a valid card correctly" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val firstCard = controller.currentPlayer.hand.head
      controller.playCard(0)
      controller.currentPlayer.hand.size should (be < 7 or equal(7))
    }

    "enter awaiting-color mode when Wild is played" in {
      val controller = Controller()

      controller.startGame(Seq("Alice", "Bob"))
      val wild = Card(Color.Black, Rank.Wild)
      val p = controller.currentPlayer
      val newPlayer = p.copy(hand = Vector(wild))
      val updatedPlayers = Vector(newPlayer, controller.currentPlayer)

      val field = classOf[Controller].getDeclaredField("players")
      field.setAccessible(true)
      field.set(controller, updatedPlayers)

      controller.playCard(0)
      controller.isAwaitingColorChoise shouldBe true
    }

    "choose a color after playing a Wild" in {
      val controller = Controller()

      controller.startGame(Seq("Alice", "Bob"))

      val wild = Card(Color.Black, Rank.Wild)
      val p = controller.currentPlayer
      val newPlayer = p.copy(hand = Vector(wild))
      val updatedPlayers = Vector(newPlayer, controller.currentPlayer)

      val field = classOf[Controller].getDeclaredField("players")
      field.setAccessible(true)
      field.set(controller, updatedPlayers)

      controller.playCard(0)
      controller.isAwaitingColorChoise shouldBe true

      controller.chooseColor("r")
      controller.isAwaitingColorChoise shouldBe false
    }

    "rotate through players correctly with nextPlayer()" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val first = controller.currentPlayer.name
      controller.nextPlayer()
      controller.currentPlayer.name should not equal first
    }

    "generate a readable state string" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val s = controller.gameStateToString

      s should include("Aktueller Spieler")
      s should include("Oberste Karte")
      s should include("Deckgröße")
    }
  }
