package de.htwg.se.uno2.controller

import de.htwg.se.uno2.model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ControllerSpec extends AnyWordSpec with Matchers:

  "A Controller" should {

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
  }
