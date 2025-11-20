package de.htwg.se.uno2.util

trait Observer {
  def update: Unit
}

class Observable {
  private var observers: Vector[Observer] = Vector()

  def addObserver(s: Observer): Unit =
    observers :+= s

  def removeObserver(s: Observer): Unit =
    observers = observers.filterNot(o => o == s)

  def notifyObservers: Unit =
    observers.foreach(o => o.update)
}