package de.htwg.se.uno2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.controller.impl.Controller
import de.htwg.se.uno2.core.{Game, GameFactory}
import de.htwg.se.uno2.core.Game.Snapshot
import de.htwg.se.uno2.core.impl.model.{Card, Deck, GameState, Player}
import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.util.Observer

final class InterfacesSmokeSpec extends AnyWordSpec with Matchers {

  final class DummyFileIO extends FileIOInterface {
    var saved: Int = 0
    var toLoad: Option[GameState] = None

    override def save(state: GameState): Unit = saved += 1
    override def load(): Option[GameState] = toLoad
  }

  final case class FakeGame(deckCount: Int = 10, gs: GameState = null.asInstanceOf[GameState]) extends Game {
    override def drawCard: Game = copy(deckCount = deckCount - 1)
    override def playCard(index: Int): Game = this
    override def endTurn: Game = this
    override def chooseColor(token: String): Game = this

    override def gameState: GameState = gs
    override def isAwaitingColorChoise: Boolean = false
    override def isGameOver: Boolean = false
    override def winnerName: Option[String] = None

    override def snapshot: Snapshot =
      Snapshot("-", Vector.empty, None, deckCount, awaitingColor = false, winnerName = None)

    override def currentPlayer: Player = null.asInstanceOf[Player]
    override def currentHand: Vector[Card] = Vector.empty
    override def topDiscard: Option[Card] = None

    override def deckSize: Int = deckCount
    override def deck: Deck = Deck.empty
    override def discard: Vector[Card] = Vector.empty
    override def currentPlayerName: String = "-"
  }

  final class StubFactory(game: Game) extends GameFactory {
    override def create(names: Seq[String]): Game = game
  }

  "ControllerInterface / FileIOInterface" should {

    "allow Controller to be used as ControllerInterface and call save/load" in {
      val fio = new DummyFileIO
      val controller: ControllerInterface = new Controller(new StubFactory(FakeGame()), fio)

      controller.startGame(Seq("A"))
      controller.save()
      fio.saved shouldBe 1

      fio.toLoad = None
      noException should be thrownBy controller.load()
    }
  }
}
