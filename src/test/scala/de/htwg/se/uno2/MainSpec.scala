package de.htwg.se.uno2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.awt.{GraphicsEnvironment, Window}
import java.awt.event.WindowEvent
import javax.swing.{JFrame, SwingUtilities}

final class MainSpec extends AnyWordSpec with Matchers {

  private def onEDT(body: => Unit): Unit =
    if (SwingUtilities.isEventDispatchThread) body
    else SwingUtilities.invokeAndWait(() => body)

  private def waitForGuiWindow(timeoutMs: Long = 5000): Window = {
    val start = System.currentTimeMillis()
    var w: Window = null
    while (w == null && (System.currentTimeMillis() - start) < timeoutMs) {
      val ws = Window.getWindows.toVector
        .filter(_.isDisplayable)
        .filter(_.isVisible)
      w = ws.find(_.getClass.getName.contains("de.htwg.se.uno2.aview.GUI"))
        .orElse(ws.collectFirst { case f: JFrame if Option(f.getTitle).exists(_.toLowerCase.contains("uno")) => f })
        .orNull
      if (w == null) Thread.sleep(20)
    }
    if (w == null) throw new RuntimeException("GUI window not found")
    w
  }

  private def closeWindow(w: Window): Unit = {
    onEDT {
      w.dispatchEvent(new WindowEvent(w, WindowEvent.WINDOW_CLOSING))
      w.dispose()
    }
  }

  "main run()" should {

    "finish in ui=gui mode after closing the GUI window" in {
      assume(!GraphicsEnvironment.isHeadless)

      val prev = sys.props.get("ui")
      sys.props.put("ui", "gui")

      try {
        val t = new Thread(() => de.htwg.se.uno2.run())
        t.setDaemon(true)
        t.start()

        val w = waitForGuiWindow()
        closeWindow(w)

        t.join(5000)
        t.isAlive.shouldBe(false)
      } finally {
        prev match
          case Some(v) => sys.props.put("ui", v)
          case None    => sys.props.remove("ui")
      }
    }
  }
}

