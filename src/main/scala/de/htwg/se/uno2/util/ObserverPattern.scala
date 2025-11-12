package de.htwg.se.uno2.util

import de.htwg.se.uno2.util._

class TestObject extends Observer {
  def update:Unit = println("Ping")
}
object ObserverPattern {
  val observable = new Observable
  val observer1 = new TestObject
  val observer2 = new TestObject
  observable.addObserver(observer1)
  observable.addObserver(observer2)
  observable.notifyObservers
  observable.removeObserver(observer1)
  observable.notifyObservers
  observable.removeObserver(observer2)
  observable.notifyObservers
}