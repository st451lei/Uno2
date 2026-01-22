package de.htwg.se.uno2.util

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

final class CommandSpec extends AnyWordSpec with Matchers {

  "Command" should {

    "be implementable and its methods callable" in {
      var log: Vector[String] = Vector.empty

      val cmd: Command = new Command {
        override def doStep(): Unit = log = log :+ "do"
        override def undoStep(): Unit = log = log :+ "undo"
        override def redoStep(): Unit = log = log :+ "redo"
      }

      cmd.doStep()
      cmd.undoStep()
      cmd.redoStep()

      log shouldBe Vector("do", "undo", "redo")
    }
  }
}
