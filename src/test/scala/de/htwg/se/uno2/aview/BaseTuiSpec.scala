package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.Controller
import de.htwg.se.uno2.model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream

class BaseTuiSpec extends AnyWordSpec with Matchers:

  "BaseTui" should {

    "dispatch normal input to handleNormalInput and stop when shouldExit become true" in {
      val controller = new Controller

      var handledInputs = Vector.empty[String]
      var quitCalled = false

      class TestBaseTui(c: Controller) extends BaseTui(c):
        override def update: Unit = ()

        override protected def shouldExit: Boolean = quitCalled
        override protected def handleQuit(): Unit =
          quitCalled = true

        override protected def handleAwaitingColorInput(input: String): Unit = ()

        override protected def handleNormalInput(input: String): Unit =
          handledInputs :+= input

        override protected def askPlayers(): Seq[String] = Seq("A", "B")

      val tui = new TestBaseTui(controller)

      val in = new ByteArrayInputStream("foo\nquit\n".getBytes("UTF-8"))
      Console.withIn(in):
        tui.run()

      handledInputs should contain ("foo")
      quitCalled shouldBe true
    }

    "dispatch input to handleAwaitingColorInput when controller is awaiting a color" in {
      val top = Card(Color.Red, Rank.Number(5))
      val wild = Card(Color.Black, Rank.Wild)
      val p1 = Player("P1", Vector.empty)
      val p2 = Player("P2", Vector.empty)

      val baseState = GameState(
        deck = Deck.empty,
        discard = Vector(top, wild),
        players = Vector(p1, p2),
        currentPlayerIndex = 0,
        chosenColor = None,
        awaitingColor = true,
        ruleSet = ClassicRuleSet
      )

      val factory = new GameStateFactory:
        override def create(names: Seq[String]): GameState = baseState

      val controller = new Controller(factory)
      var awaitingInputs = Vector.empty[String]
      var quitCalled = false

      class AwaitingBaseTui(c: Controller) extends BaseTui(c):
        override def update: Unit = ()
        override protected def shouldExit: Boolean = quitCalled
        override protected def handleQuit(): Unit =
          quitCalled = true

        override protected def handleAwaitingColorInput(input: String): Unit =
          awaitingInputs :+= input

        override protected def handleNormalInput(input: String): Unit = ()
        override protected def askPlayers(): Seq[String] = Seq("P1", "P2")

      val tui = new AwaitingBaseTui(controller)
      val in = new ByteArrayInputStream("color g\nquit\n".getBytes("UTF-8"))
      Console.withIn(in):
        tui.run()

      awaitingInputs should contain ("color g")
      quitCalled shouldBe true
    }
  }

