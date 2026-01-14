package de.htwg.se.uno2

import com.google.inject.Guice
import net.codingwell.scalaguice.InjectorExtensions.*

import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.aview.{Tui, GUI}

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.swing.Swing

@main def run(): Unit =
  val injector = Guice.createInjector(new Uno2Module)
  
  val controller: ControllerInterface = injector.instance[ControllerInterface]
  
  val tui = new Tui(controller)
  val gui = new GUI(controller)
  
  controller.addObserver(tui)
  controller.addObserver(gui)

  Future {
    tui.run()
  }