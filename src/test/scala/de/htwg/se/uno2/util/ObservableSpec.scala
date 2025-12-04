package de.htwg.se.uno2.util

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class ObservableSpec extends AnyWordSpec with Matchers:

  "An Observable" should {

    "notify all observers and allow add/remove" in {
      val observable = new Observable
      var counter = 0

      val o1 = new Observer:
        override def update: Unit = counter += 1

      val o2 = new Observer:
        override def update: Unit = counter += 10

      observable.addObserver(o1)
      observable.addObserver(o2)

      observable.notifyObservers
      counter shouldBe 11

      observable.removeObserver(o1)
      observable.notifyObservers
      counter shouldBe 21
    }
  }

