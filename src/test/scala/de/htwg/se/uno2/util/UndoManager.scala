package de.htwg.se.uno2.util

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

final class UndoManagerSpec extends AnyWordSpec with Matchers {

  final class SpyCommand extends Command {
    var did = 0
    var undid = 0
    var redid = 0

    override def doStep(): Unit = did += 1
    override def undoStep(): Unit = undid += 1
    override def redoStep(): Unit = redid += 1
  }

  "UndoManager" should {

    "doStep executes command and clears redo stack" in {
      val um = new UndoManager
      val c1 = new SpyCommand
      val c2 = new SpyCommand

      um.doStep(c1)
      c1.did shouldBe 1

      um.undoStep()
      c1.undid shouldBe 1

      um.doStep(c2)
      c2.did shouldBe 1

      um.redoStep()
      c1.redid shouldBe 0
      c2.redid shouldBe 0
    }

    "undoStep on empty does nothing" in {
      val um = new UndoManager
      noException should be thrownBy um.undoStep()
    }

    "redoStep on empty does nothing" in {
      val um = new UndoManager
      noException should be thrownBy um.redoStep()
    }

    "undo then redo moves same command between stacks" in {
      val um = new UndoManager
      val c = new SpyCommand

      um.doStep(c)
      um.undoStep()
      c.undid shouldBe 1

      um.redoStep()
      c.redid shouldBe 1
    }
  }
}
