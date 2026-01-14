package de.htwg.se.uno2.aview

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.core.impl.model.Card
import de.htwg.se.uno2.util.Observer

import java.io.ByteArrayInputStream

final class BaseTuiSpec extends AnyWordSpec with Matchers {

  final class SpyController extends ControllerInterface {
    var startedWith: Option[Seq[String]] = None
    var awaiting: Boolean = false

    override def addObserver(o: Observer): Unit = ()
    override def removeObserver(o: Observer): Unit = ()

    override def startGame(names: Seq[String]): Unit = startedWith = Some(names)

    override def drawCard: Unit = ()
    override def playCard(index: Int): Unit = ()
    override def endTurn(): Unit = ()
    override def canEndTurn: Boolean = true
    override def chooseColor(token: String): Unit = ()

    override def save(): Unit = ()
    override def load(): Unit = ()

    override def undo(): Unit = ()
    override def redo(): Unit = ()

    override def isAwaitingColorChoise: Boolean = awaiting
    override def isGameOver: Boolean = false
    override def winnerName: Option[String] = None
    override def gameStateToString: String = "STATE"

    override def currentHand: Vector[Card] = Vector.empty
    override def topDiscard: Option[Card] = None
    override def deckSize: Int = 0
    override def currentPlayerName: String = "P"
  }

  "BaseTui.run" should {

    "start game with askPlayers result and handle quit from input" in {
      val c = new SpyController

      final class TestTui(ctrl: ControllerInterface) extends BaseTui(ctrl) {
        var exit = false

        override def update: Unit = ()
        override protected def shouldExit: Boolean = exit
        override protected def handleQuit(): Unit = exit = true
        override protected def handleAwaitingColorInput(input: String): Unit = exit = true
        override protected def handleNormalInput(input: String): Unit = exit = true
        override protected def askPlayers(): Seq[String] = Seq("A", "B")
      }

      val tui = new TestTui(c)

      Console.withIn(new ByteArrayInputStream("quit\n".getBytes("UTF-8"))) {
        tui.run()
      }

      c.startedWith shouldBe Some(Seq("A", "B"))
    }
  }
}
