package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.Controller
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
    
}

