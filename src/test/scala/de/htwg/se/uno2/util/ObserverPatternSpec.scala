package de.htwg.se.uno2.util

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ObserverPatternSpec extends AnyWordSpec with Matchers:

  "ObserverPattern object" should {

    "initialize without throwing" in {
      noException shouldBe thrownBy {
        ObserverPattern.observable
      }
    }
  }
