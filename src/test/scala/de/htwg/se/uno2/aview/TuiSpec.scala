package de.htwg.se.uno2.aview

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.core.impl.model.{Card, Color}
import de.htwg.se.uno2.util.Observer

import java.io.ByteArrayInputStream

final class TuiSpec extends AnyWordSpec with Matchers {

  final class SpyController extends ControllerInterface {
    var startedWith: Option[Seq[String]] = None

    var drawCalls = 0
    var playCalls: Vector[Int] = Vector.empty
    var endTurnCalls = 0
    var chooseCalls: Vector[String] = Vector.empty

    var saveCalls = 0
    var loadCalls = 0
    var undoCalls = 0
    var redoCalls = 0

    var awaitingColor: Boolean = false
    var gameOver: Boolean = false

    override def addObserver(o: Observer): Unit = ()
    override def removeObserver(o: Observer): Unit = ()

    override def startGame(names: Seq[String]): Unit = startedWith = Some(names)

    override def drawCard: Unit = drawCalls += 1
    override def playCard(index: Int): Unit = playCalls = playCalls :+ index
    override def endTurn(): Unit = endTurnCalls += 1
    override def canEndTurn: Boolean = true
    override def chooseColor(token: String): Unit = chooseCalls = chooseCalls :+ token

    override def save(): Unit = saveCalls += 1
    override def load(): Unit = loadCalls += 1

    override def undo(): Unit = undoCalls += 1
    override def redo(): Unit = redoCalls += 1

    override def isAwaitingColorChoise: Boolean = awaitingColor
    override def isGameOver: Boolean = gameOver
    override def winnerName: Option[String] = None
    override def gameStateToString: String = "STATE"

    override def currentHand: Vector[Card] = Vector.empty
    override def topDiscard: Option[Card] = None
    override def deckSize: Int = 0
    override def currentPlayerName: String = "P"

    override def opponentCardCounts: Vector[(String, Int)] = Vector.empty

    override def activeColor: Option[Color] = None
  }

  final class ExposedTui(c: ControllerInterface) extends Tui(c) {
    def normal(input: String): Unit = handleNormalInput(input)
    def awaiting(input: String): Unit = handleAwaitingColorInput(input)
    def playersFromInput(in: String): Seq[String] =
      Console.withIn(new ByteArrayInputStream(in.getBytes("UTF-8"))) {
        askPlayers()
      }
  }

  "Tui.handleNormalInput" should {

    "call drawCard on 'draw'" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      tui.normal("draw")
      c.drawCalls shouldBe 1
    }

    "call playCard on 'play <index>'" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      tui.normal("play 2")
      c.playCalls shouldBe Vector(2)
    }

    "not call playCard when index is invalid" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      tui.normal("play abc")
      c.playCalls shouldBe Vector.empty
    }

    "route save/load/undo/redo commands" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      tui.normal("save")
      tui.normal("load")
      tui.normal("undo")
      tui.normal("redo")

      c.saveCalls shouldBe 1
      c.loadCalls shouldBe 1
      c.undoCalls shouldBe 1
      c.redoCalls shouldBe 1
    }
  }

  "Tui.handleAwaitingColorInput" should {

    "accept 'color r' and call chooseColor('r')" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      tui.awaiting("color r")
      c.chooseCalls shouldBe Vector("r")
    }

    "route save/load/undo/redo while awaiting color" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      tui.awaiting("save")
      tui.awaiting("load")
      tui.awaiting("undo")
      tui.awaiting("redo")

      c.saveCalls shouldBe 1
      c.loadCalls shouldBe 1
      c.undoCalls shouldBe 1
      c.redoCalls shouldBe 1
    }

    "fallback to chooseColor(\"\") on unknown input" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      tui.awaiting("???")
      c.chooseCalls shouldBe Vector("")
    }
  }

  "Tui.askPlayers" should {

    "parse names from stdin" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      val names = tui.playersFromInput("Ann Ben\n")
      names shouldBe Seq("Ann", "Ben")
    }

    "use default players on empty input" in {
      val c = new SpyController
      val tui = new ExposedTui(c)

      val names = tui.playersFromInput("\n")
      names shouldBe Seq("Player1", "Player2")
    }
  }
}
