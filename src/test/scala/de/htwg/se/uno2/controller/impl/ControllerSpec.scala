package de.htwg.se.uno2.controller.impl

import de.htwg.se.uno2.core.Game.Snapshot
import de.htwg.se.uno2.core.impl.model.{Card, GameState, Player, Deck}
import de.htwg.se.uno2.core.{Game, GameFactory}
import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.util.Observer
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

final class ControllerSpec extends AnyWordSpec with Matchers {
  
  final class CountingObserver extends Observer {
    var updates: Int = 0
    override def update: Unit = updates += 1
  }
  
  final class SpyFileIO(var nextLoad: Option[GameState] = None) extends FileIOInterface {
    var saveCalls: Int = 0
    var lastSaved: Option[GameState] = None

    override def save(state: GameState): Unit = {
      saveCalls += 1
      lastSaved = Some(state)
    }

    override def load(): Option[GameState] = nextLoad
  }
  
  final class StubFactory(initial: Game) extends GameFactory {
    var lastNames: Seq[String] = Seq.empty
    var gameToReturn: Game = initial

    override def create(names: Seq[String]): Game = {
      lastNames = names
      gameToReturn
    }
  }


  final case class FakeGame(
                             currentName: String = "Alice",
                             hand: Vector[Card] = Vector.empty,
                             top: Option[Card] = None,
                             deckCount: Int = 10,
                             awaitingColor: Boolean = false,
                             gameOver: Boolean = false,
                             winner: Option[String] = None,
                             throwOnDraw: Boolean = false,
                             throwOnPlay: Boolean = false,
                             throwOnEndTurn: Boolean = false,
                             throwOnChoose: Boolean = false,
                             setAwaitingColorAfterPlay: Boolean = false,
                             gs: GameState = null.asInstanceOf[GameState]
                           ) extends Game {

    override def gameState: GameState = gs
    
    override def snapshot: Snapshot =
      Snapshot(
        currentPlayerName = currentName,
        currentHand = hand.map(_.toString),
        topDiscard = top.map(_.toString),
        deckSize = deckCount,
        awaitingColor = awaitingColor,
        winnerName = winner
      )
    
    override def currentPlayer: Player = null.asInstanceOf[Player]

    override def currentHand: Vector[Card] = hand
    override def topDiscard: Option[Card] = top
    override def deckSize: Int = deckCount
    override def currentPlayerName: String = currentName
    override def deck: Deck = Deck.empty
    override def discard: Vector[Card] = Vector.empty
    override def isAwaitingColorChoise: Boolean = awaitingColor
    override def isGameOver: Boolean = gameOver
    override def winnerName: Option[String] = winner

    override def drawCard: Game =
      if (throwOnDraw) throw new RuntimeException("draw failed")
      else copy(deckCount = deckCount - 1)

    override def playCard(index: Int): Game =
      if (throwOnPlay) throw new RuntimeException("play failed")
      else if (index < 0 || index >= hand.size) throw new IndexOutOfBoundsException(index)
      else copy(awaitingColor = setAwaitingColorAfterPlay)

    override def endTurn: Game =
      if (throwOnEndTurn) throw new RuntimeException("endTurn failed")
      else this

    override def chooseColor(token: String): Game =
      if (throwOnChoose) throw new RuntimeException("choose failed")
      else copy(awaitingColor = false)
  }

  "Controller" should {

    "return 'Noch kein Spiel gestartet' before startGame" in {
      val c = new Controller(new StubFactory(FakeGame()), new SpyFileIO())
      c.gameStateToString shouldBe "Noch kein Spiel gestartet"
    }

    "startGame: set state, reset undoManager, and notify observers" in {
      val game = FakeGame(currentName = "Bob", deckCount = 7)
      val factory = new StubFactory(game)
      val fileIO = new SpyFileIO()
      val c = new Controller(factory, fileIO)

      val obs = new CountingObserver
      c.addObserver(obs)

      c.startGame(Seq("Bob", "Clara"))
      factory.lastNames shouldBe Seq("Bob", "Clara")
      c.currentPlayerName shouldBe "Bob"
      c.deckSize shouldBe 7
      obs.updates should be >= 1
    }

    "drawCard: change state, undo, redo (happy path)" in {
      val game = FakeGame(deckCount = 10)
      val c = new Controller(new StubFactory(game), new SpyFileIO())
      c.startGame(Seq("A"))

      c.deckSize shouldBe 10
      c.drawCard
      c.deckSize shouldBe 9

      c.undo()
      c.deckSize shouldBe 10

      c.redo()
      c.deckSize shouldBe 9
    }

    "drawCard: if Game.drawCard throws, controller restores backup (Failure branch covered)" in {
      val game = FakeGame(deckCount = 10, throwOnDraw = true)
      val c = new Controller(new StubFactory(game), new SpyFileIO())
      c.startGame(Seq("A"))

      noException should be thrownBy c.drawCard
      c.deckSize shouldBe 10
    }

    "playCard: if index invalid -> restored (Failure branch of PlayCardCommand)" in {
      val game = FakeGame(hand = Vector.empty, deckCount = 10)
      val c = new Controller(new StubFactory(game), new SpyFileIO())
      c.startGame(Seq("A"))

      noException should be thrownBy c.playCard(0)
      c.deckSize shouldBe 10
      c.isAwaitingColorChoise shouldBe false
    }

    "playCard: can switch to awaiting-color mode (state-driven)" in {
      val game = FakeGame(
        hand = Vector.empty,
        deckCount = 10
      ).copy(hand = Vector(null.asInstanceOf[Card]), setAwaitingColorAfterPlay = true)

      val c = new Controller(new StubFactory(game), new SpyFileIO())
      c.startGame(Seq("A"))

      c.isAwaitingColorChoise shouldBe false
      c.playCard(0)
      c.isAwaitingColorChoise shouldBe true
    }

    "chooseColor: turn off awaiting-color mode" in {
      val game = FakeGame(awaitingColor = true)
      val c = new Controller(new StubFactory(game), new SpyFileIO())
      c.startGame(Seq("A"))

      c.isAwaitingColorChoise shouldBe true
      c.chooseColor("r")
      c.isAwaitingColorChoise shouldBe false
    }

    "save(): call fileIO.save with current gameState" in {
      val fileIO = new SpyFileIO()
      val game = FakeGame(gs = null.asInstanceOf[GameState])
      val c = new Controller(new StubFactory(game), fileIO)

      c.startGame(Seq("A"))
      c.save()

      fileIO.saveCalls shouldBe 1
      fileIO.lastSaved.isDefined shouldBe true
    }

    "load(): when fileIO.load() is None, do nothing (cover None-branch)" in {
      val fileIO = new SpyFileIO(nextLoad = None)
      val game = FakeGame(deckCount = 10)
      val c = new Controller(new StubFactory(game), fileIO)
      c.startGame(Seq("A"))

      noException should be thrownBy c.load()
      c.deckSize shouldBe 10
    }

    "guards: if game is over, draw/play/choose/endTurn do nothing" in {
      val game = FakeGame(gameOver = true, deckCount = 10).copy(hand = Vector(null.asInstanceOf[Card]))
      val c = new Controller(new StubFactory(game), new SpyFileIO())
      c.startGame(Seq("A"))

      c.drawCard
      c.playCard(0)
      c.chooseColor("g")
      c.endTurn()

      c.deckSize shouldBe 10
    }
  }
}
