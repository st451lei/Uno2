package de.htwg.se.uno2.core.api

trait GameFactory:
  def create(names: Seq[String]): Game
