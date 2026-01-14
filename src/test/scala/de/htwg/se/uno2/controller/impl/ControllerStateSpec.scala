package de.htwg.se.uno2.controller.impl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.{Game, GameFactory}
import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.core.impl.model.{Card, Deck, GameState, Player}
import de.htwg.se.uno2.core.Game.Snapshot

final class ControllerStateSpec extends AnyWordSpec with Matchers {

  final class DummyFileIO extends FileIOInterface {
    override def save(state: GameState): Unit = ()
    override def load(): Option[GameState] = None
  }

  final case class DummyGame() extends Game {
    override def drawCard: Game = this
    override def playCard(index: Int): Game = this
    override def endTurn: Game = this
    override def chooseColor(token: String): Game = this
    override def gameState: GameState = null.asInstanceOf[GameState]
    override def isAwaitingColorChoise: Boolean = false
    override def isGameOver: Boolean = false
    override def winnerName: Option[String] = None
    override def snapshot: Snapshot = Snapshot("-", Vector.empty, None, 0, awaitingColor = false, winnerName = None)
    override def currentPlayer: Player = null.asInstanceOf[Player]
    override def currentHand: Vector[Card] = Vector.empty
    override def topDiscard: Option[Card] = None
    override def deckSize: Int = 0
    override def deck: Deck = Deck.empty
    override def discard: Vector[Card] = Vector.empty
    override def currentPlayerName: String = "-"
  }

  final class DummyFactory extends GameFactory {
    override def create(names: Seq[String]): Game = DummyGame()
  }

  final class SpyController extends Controller(new DummyFactory, new DummyFileIO) {
    var playInternal: Vector[Int] = Vector.empty
    var drawInternal: Int = 0
    var chooseInternal: Vector[String] = Vector.empty

    override private[controller] def playCardInternal(index: Int): Unit =
      playInternal = playInternal :+ index

    override private[controller] def drawCardInternal(): Unit =
      drawInternal += 1

    override private[controller] def chooseColorInternal(token: String): Unit =
      chooseInternal = chooseInternal :+ token
  }

  "NormalState" should {

    "delegate playCard and drawCard to controller internals" in {
      val c = new SpyController

      NormalState.playCard(c, 2)
      NormalState.drawCard(c)

      c.playInternal shouldBe Vector(2)
      c.drawInternal shouldBe 1
      c.chooseInternal shouldBe Vector.empty
    }

    "ignore chooseColor" in {
      val c = new SpyController

      NormalState.chooseColor(c, "r")

      c.playInternal shouldBe Vector.empty
      c.drawInternal shouldBe 0
      c.chooseInternal shouldBe Vector.empty
    }
  }

  "AwaitingColorState" should {

    "ignore playCard and drawCard" in {
      val c = new SpyController

      AwaitingColorState.playCard(c, 1)
      AwaitingColorState.drawCard(c)

      c.playInternal shouldBe Vector.empty
      c.drawInternal shouldBe 0
      c.chooseInternal shouldBe Vector.empty
    }

    "delegate chooseColor to controller internal" in {
      val c = new SpyController

      AwaitingColorState.chooseColor(c, "blue")

      c.playInternal shouldBe Vector.empty
      c.drawInternal shouldBe 0
      c.chooseInternal shouldBe Vector("blue")
    }
  }

  "ControllerState (polymorphic)" should {

    "behave according to concrete state implementation" in {
      val c = new SpyController

      val s1: ControllerState = NormalState
      s1.playCard(c, 0)
      s1.chooseColor(c, "r")

      val s2: ControllerState = AwaitingColorState
      s2.drawCard(c)
      s2.chooseColor(c, "g")

      c.playInternal shouldBe Vector(0)
      c.drawInternal shouldBe 0
      c.chooseInternal shouldBe Vector("g")
    }
  }
}
