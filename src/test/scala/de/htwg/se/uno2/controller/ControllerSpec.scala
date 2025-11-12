package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model._
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
    }
    
    "have a current player after start" in {
      val controller = Controller()
      controller.startGame(Seq("Alice","Bob"))
      val cp = controller.currentPlayer
      cp.name shouldBe "Alice"
    }
    
    "allow a player to draw a card" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val before = controller.currentPlayer.hand.size
      controller.drawCard()
      controller.currentPlayer.hand.size should be (before + 1)
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
      controller.currentPlayer.hand.size should be < 7
    }
    
    "print a proper game state string" in {
      val controller = Controller()
      controller.startGame(Seq("Alice", "Bob"))
      val state = controller.gameStateToString
      state should include ("Oberste Karte")
      state should include ("Deckgröße")
    }
  }
