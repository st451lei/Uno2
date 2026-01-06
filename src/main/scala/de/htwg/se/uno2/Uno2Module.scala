package de.htwg.se.uno2

import com.google.inject.AbstractModule
import net.codingwell.scalaguice.ScalaModule

import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.controller.impl.Controller
import de.htwg.se.uno2.core.GameFactory
import de.htwg.se.uno2.core.impl.DefaultGameFactory

class Uno2Module extends AbstractModule with ScalaModule:
  override def configure(): Unit =
    bind[ControllerInterface].to[Controller]
    
    bind[GameFactory].toInstance(DefaultGameFactory)