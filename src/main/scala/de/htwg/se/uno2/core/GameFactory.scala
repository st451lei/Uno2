package de.htwg.se.uno2.core

import de.htwg.se.uno2.core.Game

trait GameFactory:
  def create(names: Seq[String]): Game
