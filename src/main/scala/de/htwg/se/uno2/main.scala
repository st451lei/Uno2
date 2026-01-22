package de.htwg.se.uno2

import com.google.inject.Guice
import net.codingwell.scalaguice.InjectorExtensions.*

import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.aview.{Tui, GUI}

import scala.concurrent.{ExecutionContext, Future}
import scala.swing.Swing
import java.util.concurrent.CountDownLatch
import java.awt.event.{WindowAdapter, WindowEvent}

@main def run(): Unit =
  val injector = Guice.createInjector(new Uno2Module)
  val controller: ControllerInterface = injector.instance[ControllerInterface]

  val uiMode =
    sys.props.get("ui")
      .orElse(sys.env.get("ui"))
      .getOrElse("both")
      .toLowerCase

  val gui = new GUI(controller)
  controller.addObserver(gui)

  val latch = new CountDownLatch(1)
  Swing.onEDTWait {
    gui.peer.addWindowListener(new WindowAdapter {
      override def windowClosing(e: WindowEvent): Unit = latch.countDown()
      override def windowClosed(e: WindowEvent): Unit = latch.countDown()
    })
    gui.visible = true
  }

  uiMode match
    case "gui" =>
      latch.await()

    case _ =>
      val tui = new Tui(controller)
      controller.addObserver(tui)

      given ExecutionContext = ExecutionContext.global
      Future { tui.run() }

      latch.await()