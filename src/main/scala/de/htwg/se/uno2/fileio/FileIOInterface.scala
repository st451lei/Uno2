package de.htwg.se.uno2.fileio

import de.htwg.se.uno2.core.impl.model.GameState

trait FileIOInterface:
  def save(state: GameState): Unit
  def load(): Option[GameState]
