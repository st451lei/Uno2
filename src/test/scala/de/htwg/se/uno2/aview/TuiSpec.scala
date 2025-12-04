package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.Controller
import de.htwg.se.uno2.model.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayOutputStream
import java.io.PrintStream

class TuiSpec extends AnyWordSpec with Matchers:

  class TestTui(controller: Controller) extends Tui(controller):
    def testInput(cmd: String): String =
      val out = new ByteArrayOutputStream()
      Console.withOut(PrintStream(out)):

        this.processInput(cmd)
      out.toString

  class ExposedTui(contoller: Controller) extends Tui(contoller):
    def callHandleNormal(input:String): Unit = handleNormalInput(input)
    def callHandleAwaiting(input: String): Unit = handleAwaitingColorInput(input)
    def callHandleQuitPublic(): Unit = handleQuit()
    def exited: Boolean = shouldExit

    override protected def askPlayers(): Seq[String] = Seq("A", "B")

  extension (t: Tui)
    def processInput(input: String): Unit =
      val trimmed = input.trim
      if t.controller.isAwaitingColorChoise then
        trimmed.toLowerCase match
          case s if s.startsWith("color") =>
            val p = s.split("\\s+")
            if p.length >= 2 then t.controller.chooseColor(p(1))
            else println("Verwendung: color r|y|g|b")
          case "quit" => println("Spiel beendet.")
          case _ => t.controller.chooseColor("")
      else
        trimmed match
          case "quit" => println("Spiel beendet.")
          case "draw" => t.controller.drawCard
          case s if s.startsWith("play") =>
            val p = s.split("\\s+")
            if p.length == 2 then
              p(1).toIntOption match
                case Some(i) => t.controller.playCard(i)
                case _       => println("Ungültiger Index.")
            else println("Verwendung: play <index>")
          case s if s.startsWith("color") =>
            println("Jetzt keine Farbauswahl nötig.")
          case _ =>
            println("Unbekannter Befehl. (play <index>, draw, quit)")

  "A TUI" should {

    "receive update() when controller notifies observers" in {
      val controller = new Controller
      val tui = new TestTui(controller)

      val out = new ByteArrayOutputStream()
      Console.withOut(PrintStream(out)):
        controller.startGame(Seq("A", "B"))

      out.toString should include ("Aktueller Spieler: A")
    }

    "print error on unknown command" in {
      val controller = new Controller
      controller.startGame(Seq("A", "B"))
      val tui = new TestTui(controller)

      val result = tui.testInput("foobar")
      result should include ("Unbekannter Befehl")
    }

    "execute draw command" in {
      val controller = new Controller
      controller.startGame(Seq("A", "B"))
      val oldHandSize = controller.currentPlayer.hand.size

      val tui = new TestTui(controller)
      tui.testInput("draw")

      controller.currentPlayer.hand.size shouldBe oldHandSize + 1
    }

    "reject invalid play command index" in {
      val controller = new Controller
      controller.startGame(Seq("A", "B"))
      val tui = new TestTui(controller)

      val out = tui.testInput("play 99")
      out should include ("")
      controller.currentPlayer.hand.size shouldBe 7
    }
    "handleNormalInput draw correctly" in {
      val controller = new Controller
      controller.startGame(Seq("A", "B"))
      val tui = new ExposedTui(controller)

      val before = controller.currentPlayer.hand.size
      tui.callHandleNormal("draw")
      controller.currentPlayer.hand.size shouldBe before + 1
    }

    "handleNormalInput print error on invalid index" in {
      val controller = new Controller
      controller.startGame(Seq("A", "B"))
      val tui = new ExposedTui(controller)

      val out = new ByteArrayOutputStream()
      Console.withOut(PrintStream(out)):
        tui.callHandleNormal("play foo")

      out.toString should include ("Ungültiger Index.")
    }

    "handleNormalInput print hint when color is not expected" in {
      val controller = new Controller
      controller.startGame(Seq("A", "B"))
      val tui = new ExposedTui(controller)

      val out = new ByteArrayOutputStream()
      Console.withOut(PrintStream(out)):
        tui.callHandleNormal("color r")

      out.toString should include ("Jetzt keine Farbauswahl nötig")
    }

    "handleAwaitingColorInput choose color or fallback" in {
      val top  = Card(Color.Red, Rank.Number(5))
      val wild = Card(Color.Black, Rank.Wild)
      val p1   = Player("P1", Vector.empty)
      val p2   = Player("P2", Vector.empty)

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
      controller.startGame(Seq("P1", "P2"))
      val tui = new ExposedTui(controller)

      controller.isAwaitingColorChoise shouldBe true
      tui.callHandleAwaiting("color g")
      controller.isAwaitingColorChoise shouldBe false
    }

    "handleQuit set exit flag and print message" in {
      val controller = new Controller
      controller.startGame(Seq("A", "B"))
      val tui = new ExposedTui(controller)

      val out = new ByteArrayOutputStream()
      Console.withOut(PrintStream(out)):
        tui.callHandleQuitPublic()

      out.toString should include ("Spiel beendet")
      tui.exited shouldBe true
    }
  }

