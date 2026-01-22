package de.htwg.se.uno2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.awt.{HeadlessException, Window}

final class MainSpec extends AnyWordSpec with Matchers {

  "main.run" should {

    "be callable without breaking the test process" in {
      try {
        de.htwg.se.uno2.run()
      } catch {
        case _: HeadlessException => succeed
      } finally {
        Window.getWindows.foreach(_.dispose())
      }
      succeed
    }
  }
}


