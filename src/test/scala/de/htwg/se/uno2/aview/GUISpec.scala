package de.htwg.se.uno2.aview

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.core.impl.model.{Card, Color, Rank}
import de.htwg.se.uno2.core.impl.model.Color.*
import de.htwg.se.uno2.core.impl.model.Rank.*
import de.htwg.se.uno2.util.Observer

import scala.swing.*
import java.awt.{GraphicsEnvironment, Rectangle, Window}
import java.awt.event.MouseEvent
import java.awt.image.BufferedImage
import javax.swing.{JDialog, SwingUtilities}
import scala.reflect.ClassTag
import java.util.concurrent.atomic.AtomicReference

final class GUISpec extends AnyWordSpec with Matchers {

  private def onEDTResult[T](body: => T): T = {
    if (SwingUtilities.isEventDispatchThread) body
    else {
      val ref = new AtomicReference[T]()
      val err = new AtomicReference[Throwable]()
      SwingUtilities.invokeAndWait(() => {
        try ref.set(body)
        catch case t: Throwable => err.set(t)
      })
      if (err.get() != null) throw err.get()
      ref.get()
    }
  }

  private def onEDT(body: => Unit): Unit = {
    if (SwingUtilities.isEventDispatchThread) body
    else SwingUtilities.invokeAndWait(() => body)
  }

  final class SpyController extends ControllerInterface {
    var startedWith: Option[Seq[String]] = None
    var drawCalls: Int = 0
    var playCalls: Vector[Int] = Vector.empty
    var endTurnCalls: Int = 0
    var chooseCalls: Vector[String] = Vector.empty
    var saveCalls: Int = 0
    var loadCalls: Int = 0
    var undoCalls: Int = 0
    var redoCalls: Int = 0

    var awaitingColor: Boolean = false
    var gameOver: Boolean = false
    var winner: Option[String] = None

    var hand: Vector[Card] = Vector.empty
    var top: Option[Card] = None
    var deck: Int = 60
    var current: String = "-"

    override def addObserver(o: Observer): Unit = ()
    override def removeObserver(o: Observer): Unit = ()

    override def startGame(names: Seq[String]): Unit = {
      startedWith = Some(names)
      current = names.headOption.getOrElse("-")
    }

    override def drawCard: Unit = { drawCalls += 1 }
    override def playCard(index: Int): Unit = { playCalls = playCalls :+ index }
    override def endTurn(): Unit = { endTurnCalls += 1 }
    override def canEndTurn: Boolean = true
    override def chooseColor(token: String): Unit = { chooseCalls = chooseCalls :+ token }

    override def save(): Unit = { saveCalls += 1 }
    override def load(): Unit = { loadCalls += 1 }
    override def undo(): Unit = { undoCalls += 1 }
    override def redo(): Unit = { redoCalls += 1 }

    override def isAwaitingColorChoise: Boolean = awaitingColor
    override def isGameOver: Boolean = gameOver
    override def winnerName: Option[String] = winner
    override def gameStateToString: String = "STATE"

    override def currentHand: Vector[Card] = hand
    override def topDiscard: Option[Card] = top
    override def deckSize: Int = deck
    override def currentPlayerName: String = current

    override def opponentCardCounts: Vector[(String, Int)] = Vector.empty
    override def activeColor: Option[Color] = None
  }

  final class TestGUI(c: ControllerInterface) extends GUI(c) {
    override def visible_=(b: Boolean): Unit = ()
    override def centerOnScreen(): Unit = ()
  }

  private def allFields(c: Class[?]): Vector[java.lang.reflect.Field] = {
    if (c == null) Vector.empty
    else c.getDeclaredFields.toVector ++ allFields(c.getSuperclass)
  }

  private def allMethods(c: Class[?]): Vector[java.lang.reflect.Method] = {
    if (c == null) Vector.empty
    else c.getDeclaredMethods.toVector ++ allMethods(c.getSuperclass)
  }

  private def findByType[T: ClassTag](obj: AnyRef)(pred: T => Boolean): T = {
    val cls = summon[ClassTag[T]].runtimeClass
    val fields = allFields(obj.getClass)
    fields.foreach(_.setAccessible(true))
    fields.iterator
      .map(f => f.get(obj))
      .collect { case v if v != null && cls.isInstance(v) => v.asInstanceOf[T] }
      .find(pred)
      .get
  }

  private def valueByName[T](obj: AnyRef, name: String): T = {
    val fields = allFields(obj.getClass)
    fields.foreach(_.setAccessible(true))
    fields.find(_.getName == name)
      .orElse(fields.find(_.getName.contains(name)))
      .map(_.get(obj).asInstanceOf[T])
      .getOrElse {
        val ms = allMethods(obj.getClass)
        val mOpt = ms.find(m => m.getParameterCount == 0 && (m.getName == name || m.getName.contains(name)))
        val m = mOpt.getOrElse(throw new NoSuchElementException(name))
        m.setAccessible(true)
        m.invoke(obj).asInstanceOf[T]
      }
  }

  private def findButton(gui: AnyRef, text: String): Button =
    findByType[Button](gui)(b => b.text == text)

  private def findButtonByTooltip(gui: AnyRef, tip: String): Button =
    findByType[Button](gui)(b => Option(b.tooltip).contains(tip))

  private def findTextFieldByColumns(gui: AnyRef, cols: Int): TextField =
    findByType[TextField](gui)(t => t.columns == cols)

  private def findGamePanelCenter(gui: AnyRef): Panel =
    findByType[Panel](gui)(p => p.preferredSize.width == 900 && p.preferredSize.height == 600)

  private def killDialogsLoop(maxIters: Int = 80, sleepMs: Int = 20): Thread = {
    val t = new Thread(() => {
      var i = 0
      while (i < maxIters) {
        val ds = Window.getWindows.toVector.collect { case d: JDialog if d.isVisible => d }
        ds.foreach(_.dispose())
        Thread.sleep(sleepMs.toLong)
        i += 1
      }
    })
    t.setDaemon(true)
    t.start()
    t
  }

  private def withDialogKiller(body: => Unit): Unit = {
    val t = killDialogsLoop()
    body
    t.join(2000)
  }

  private def enterGame(gui: TestGUI, names: String): Unit = {
    findButton(gui, "Spiel starten").peer.doClick()
    val nameField = findTextFieldByColumns(gui, 30)
    nameField.text = names
    findButton(gui, "Spiel mit diesen Spielern starten").peer.doClick()
  }

  "GUI" should {

    "wire start + startGame + endTurn + chooseColor buttons to controller" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController
      val gui = onEDTResult { new TestGUI(c) }

      onEDT { enterGame(gui, "Ann Ben") }
      c.startedWith.shouldBe(Some(Seq("Ann", "Ben")))

      c.awaitingColor = true
      onEDT { gui.update }

      val rotSquare = onEDTResult { findButtonByTooltip(gui, "Rot") }
      withDialogKiller { onEDT { rotSquare.peer.doClick() } }
      c.chooseCalls.last.shouldBe("r")

      val endTurnButton = onEDTResult { findButton(gui, "Zug beenden") }
      onEDT { endTurnButton.peer.doClick() }
      c.endTurnCalls.shouldBe(1)

      onEDT { gui.dispose() }
    }

    "wire save/load buttons, and cover load-fail path by keeping currentPlayerName '-'" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController
      val gui = onEDTResult { new TestGUI(c) }

      onEDT { enterGame(gui, "A") }
      onEDT { gui.update }

      val saveButton = onEDTResult { findButton(gui, "Speichern") }
      withDialogKiller { onEDT { saveButton.peer.doClick() } }
      c.saveCalls.shouldBe(1)

      c.current = "-"
      val loadButton = onEDTResult { findButton(gui, "Laden") }
      withDialogKiller { onEDT { loadButton.peer.doClick() } }
      c.loadCalls.shouldBe(1)

      onEDT { gui.dispose() }
    }

    "react to mouse press on deck and on hand card" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController
      c.current = "A"
      c.hand = Vector(Card(Red, Number(1)), Card(Blue, Skip))
      c.top = Some(Card(Green, Number(9)))
      c.deck = 12

      val gui = onEDTResult { new TestGUI(c) }
      onEDT { enterGame(gui, "A") }

      val panel = onEDTResult { findGamePanelCenter(gui) }

      onEDT {
        panel.peer.setSize(900, 600)
        panel.peer.doLayout()
        val img = new BufferedImage(900, 600, BufferedImage.TYPE_INT_ARGB)
        val g2 = img.createGraphics()
        panel.peer.paint(g2)
        g2.dispose()
      }

      val deckRect = valueByName[Rectangle](gui, "deckRect")
      val meDeck = new MouseEvent(
        panel.peer,
        MouseEvent.MOUSE_PRESSED,
        System.currentTimeMillis(),
        0,
        deckRect.x + 2,
        deckRect.y + 2,
        1,
        false,
        MouseEvent.BUTTON1
      )

      withDialogKiller { onEDT { panel.peer.dispatchEvent(meDeck) } }
      c.drawCalls.shouldBe(1)

      val hitboxes = valueByName[Vector[(Int, Rectangle)]](gui, "handHitboxes")
      hitboxes.nonEmpty.shouldBe(true)
      val (idx, rect) = hitboxes.head

      val meCard = new MouseEvent(
        panel.peer,
        MouseEvent.MOUSE_PRESSED,
        System.currentTimeMillis(),
        0,
        rect.x + 2,
        rect.y + 2,
        1,
        false,
        MouseEvent.BUTTON1
      )

      onEDT { panel.peer.dispatchEvent(meCard) }
      c.playCalls.shouldBe(Vector(idx))

      onEDT { gui.dispose() }
    }

    "update should not throw when game not over" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController
      c.current = "A"
      c.gameOver = false

      val gui = onEDTResult { new TestGUI(c) }
      noException shouldBe thrownBy(onEDT { gui.update })
      onEDT { gui.dispose() }
    }

    "update should toggle gameOverDialogShown across transitions" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController
      c.current = "A"
      c.gameOver = true
      c.winner = Some("A")

      val gui = onEDTResult { new TestGUI(c) }

      withDialogKiller { onEDT { gui.update } }
      valueByName[Boolean](gui, "gameOverDialogShown").shouldBe(true)

      withDialogKiller { onEDT { gui.update } }
      valueByName[Boolean](gui, "gameOverDialogShown").shouldBe(true)

      c.gameOver = false
      onEDT { gui.update }
      valueByName[Boolean](gui, "gameOverDialogShown").shouldBe(false)

      c.gameOver = true
      withDialogKiller { onEDT { gui.update } }
      valueByName[Boolean](gui, "gameOverDialogShown").shouldBe(true)

      onEDT { gui.dispose() }

    }
  }
}
