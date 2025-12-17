package de.htwg.se.uno2.core.impl

import de.htwg.se.uno2.core.api.*
import de.htwg.se.uno2.model.{DefaultGameStateFactory, GameStateFactory}

object DefaultGameFactory extends GameFactory:
  private val sf: GameStateFactory = DefaultGameStateFactory
  
  override def create(names: Seq[String]): Game =
    GameImpl(sf.create(names))
