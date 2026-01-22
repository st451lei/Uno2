package de.htwg.se.uno2.controller.impl

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.{Game, GameFactory}
import de.htwg.se.uno2.core.Game.Snapshot
import de.htwg.se.uno2.core.impl.GameImpl
import de.htwg.se.uno2.core.impl.model.*
import de.htwg.se.uno2.core.impl.model.Color.*
import de.htwg.se.uno2.core.impl.model.Rank.*
import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.util.Observer

final class ControllerCommandCoverageSpec extends AnyWordSpec with Matchers {

  final class CountingObserver extends Observer {
    var updates = 0
    override def update: Unit = updates += 1
  }

  final class SpyFileIO(var nextLoad: Option[GameState] = None) extends FileIOInterface {
    var saveCalls = 0
    override def save(state: GameState): Unit = saveCalls += 1
    override def load(): Option[GameState] = nextLoad
  }

  final class StubFactory(initial: Game) extends GameFactory {
    override def create(names: Seq[String]): Game = initial
  }

  private def C(c: Color, r: Rank): Card = Card(c, r)

  private def mkGs(
                    pendingNumber: Option[Int],
                    awaiting: Boolean = false,
                    winner: Option[String] = None,
                    deckSize: Int = 0
                  ): GameState =
    GameState(
      deck = Deck(Vector.fill(deckSize)(C(Red, Number(0)))),
      discard = Vector(C(Red, Number(1))),
      players = Vector(Player("A", Vector.empty), Player("B", Vector.empty)),
      currentPlayerIndex = 0,
      chosenColor = None,
      awaitingColor = awaiting,
      ruleSet = ClassicRuleSet,
      direction = 1,
      pendingWild = None,
      winnerName = winner,
      pendingNumber = pendingNumber
    )

  final case class FakeGame(
                             currentName: String = "A",
                             hand: Vector[Card] = Vector.empty,
                             top: Option[Card] = None,
                             deckCount: Int = 10,
                             awaitingColor: Boolean = false,
                             gameOver: Boolean = false,
                             winner: Option[String] = None,
                             throwOnEndTurn: Boolean = false,
                             throwOnChoose: Boolean = false,
                             gs: GameState = mkGs(None)
                           ) extends Game {

    override def drawCard: Game = this
    override def playCard(index: Int): Game = this

    override def endTurn: Game =
      if throwOnEndTurn then throw new RuntimeException("endTurn failed")
      else copy(deckCount = deckCount - 1)

    override def chooseColor(token: String): Game =
      if throwOnChoose then throw new RuntimeException("chooseColor failed")
      else copy(awaitingColor = false)

    override def gameState: GameState = gs

    override def isAwaitingColorChoise: Boolean = awaitingColor
    override def isGameOver: Boolean = gameOver
    override def winnerName: Option[String] = winner

    override def snapshot: Snapshot =
      Snapshot(
        currentPlayerName = currentName,
        currentHand = hand.map(_.toString),
        topDiscard = top.map(_.toString),
        deckSize = deckCount,
        awaitingColor = awaitingColor,
        winnerName = winner
      )

    override def currentPlayer: Player = Player(currentName, hand)
    override def currentHand: Vector[Card] = hand
    override def topDiscard: Option[Card] = top
    override def deckSize: Int = deckCount
    override def deck: Deck = Deck.empty
    override def discard: Vector[Card] = Vector.empty
    override def currentPlayerName: String = currentName
  }

  "EndTurnCommand + Controller" should {

    "cover EndTurnCommand happy path via controller.endTurn + undo/redo" in {
      val fio = new SpyFileIO()
      val game = FakeGame(deckCount = 10)
      val c = new Controller(new StubFactory(game), fio)

      val obs = new CountingObserver
      c.addObserver(obs)

      c.startGame(Seq("A"))
      c.deckSize shouldBe 10

      c.endTurn()
      c.deckSize shouldBe 9

      c.undo()
      c.deckSize shouldBe 10

      c.redo()
      c.deckSize shouldBe 9

      obs.updates should be >= 1
    }

    "cover EndTurnCommand failure branch (doStep + redoStep swallow exception)" in {
      val fio = new SpyFileIO()
      val game = FakeGame(deckCount = 10, throwOnEndTurn = true)
      val c = new Controller(new StubFactory(game), fio)

      c.startGame(Seq("A"))
      c.deckSize shouldBe 10

      noException should be thrownBy c.endTurn()
      c.deckSize shouldBe 10

      noException should be thrownBy c.undo()
      c.deckSize shouldBe 10

      noException should be thrownBy c.redo()
      c.deckSize shouldBe 10
    }

    "cover chooseColor failure branch in ChooseColorCommand" in {
      val fio = new SpyFileIO()
      val game = FakeGame(awaitingColor = true, throwOnChoose = true)
      val c = new Controller(new StubFactory(game), fio)

      c.startGame(Seq("A"))
      c.isAwaitingColorChoise shouldBe true

      noException should be thrownBy c.chooseColor("r")
      c.isAwaitingColorChoise shouldBe true
    }

    "cover save() when state is None and when state is Some" in {
      val fio = new SpyFileIO()
      val c = new Controller(new StubFactory(FakeGame()), fio)

      c.save()
      fio.saveCalls shouldBe 0

      c.startGame(Seq("A"))
      c.save()
      fio.saveCalls shouldBe 1
    }

    "cover load(Some) branch and restoreState(GameImpl(gs))" in {
      val loadedGs = mkGs(pendingNumber = Some(7), awaiting = true, deckSize = 5)
      val fio = new SpyFileIO(nextLoad = Some(loadedGs))

      val c = new Controller(new StubFactory(FakeGame(deckCount = 99)), fio)
      c.startGame(Seq("A"))
      c.deckSize shouldBe 99

      c.load()
      c.isAwaitingColorChoise shouldBe true
      c.deckSize shouldBe 5
      c.canEndTurn shouldBe false
    }

    "cover canEndTurn true/false via GameState.canEndTurn" in {
      val gsTrue = mkGs(pendingNumber = Some(1), awaiting = false, winner = None)
      val gsFalse = mkGs(pendingNumber = None, awaiting = false, winner = None)

      val c1 = new Controller(new StubFactory(FakeGame(gs = gsTrue)), new SpyFileIO())
      c1.startGame(Seq("A"))
      c1.canEndTurn shouldBe true

      val c2 = new Controller(new StubFactory(FakeGame(gs = gsFalse)), new SpyFileIO())
      c2.startGame(Seq("A"))
      c2.canEndTurn shouldBe false
    }

    "cover gameStateToString Some-branch and currentPlayer getter" in {
      val fio = new SpyFileIO()
      val game = FakeGame(currentName = "A", deckCount = 12)
      val c = new Controller(new StubFactory(game), fio)

      c.startGame(Seq("A"))
      c.currentPlayer.name shouldBe "A"

      val s = c.gameStateToString
      s should include("Aktueller Spieler")
      s should include("Deckgröße")
    }

    "cover Controller.default (companion object)" in {
      val c = Controller.default
      c.gameStateToString shouldBe "Noch kein Spiel gestartet"
    }
  }
}

