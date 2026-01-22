package de.htwg.se.uno2.aview

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.core.impl.model.{Color, *}
import de.htwg.se.uno2.core.impl.model.Color.*
import de.htwg.se.uno2.core.impl.model.Rank.*
import de.htwg.se.uno2.util.Observer

import scala.swing.*
import scala.swing.event.*
import java.awt.event.MouseEvent
import java.awt.{GraphicsEnvironment, Point, Rectangle}
import java.awt.image.BufferedImage
import scala.reflect.ClassTag

final class GUISpec extends AnyWordSpec with Matchers {

  final class SpyController extends ControllerInterface {
    var startedWith: Option[Seq[String]] = None
    var drawCalls = 0
    var playCalls: Vector[Int] = Vector.empty
    var endTurnCalls = 0
    var chooseCalls: Vector[String] = Vector.empty

    var saveCalls = 0
    var loadCalls = 0
    var undoCalls = 0
    var redoCalls = 0

    var awaitingColor = false
    var gameOver = false
    var winner: Option[String] = None

    var hand: Vector[Card] = Vector.empty
    var top: Option[Card] = None
    var deck: Int = 60
    var current: String = "A"

    override def addObserver(o: Observer): Unit = ()
    override def removeObserver(o: Observer): Unit = ()

    override def startGame(names: Seq[String]): Unit = startedWith = Some(names)

    override def drawCard: Unit = drawCalls += 1
    override def playCard(index: Int): Unit = playCalls = playCalls :+ index
    override def endTurn(): Unit = endTurnCalls += 1
    override def canEndTurn: Boolean = true
    override def chooseColor(token: String): Unit = chooseCalls = chooseCalls :+ token

    override def save(): Unit = saveCalls += 1
    override def load(): Unit = loadCalls += 1

    override def undo(): Unit = undoCalls += 1
    override def redo(): Unit = redoCalls += 1

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

  private def allFields(c: Class[?]): Vector[java.lang.reflect.Field] =
    if c == null then Vector.empty
    else c.getDeclaredFields.toVector ++ allFields(c.getSuperclass)

  private def findByType[T: ClassTag](obj: AnyRef)(pred: T => Boolean): T = {
    val cls = summon[ClassTag[T]].runtimeClass
    val fields = allFields(obj.getClass)
    fields.foreach(_.setAccessible(true))
    fields
      .iterator
      .map(f => f.get(obj))
      .collect { case v if v != null && cls.isInstance(v) => v.asInstanceOf[T] }
      .find(pred)
      .get
  }

  private def findButton(gui: AnyRef, text: String): Button =
    findByType[Button](gui)(_.text == text)

  private def findTextFieldByColumns(gui: AnyRef, cols: Int): TextField =
    findByType[TextField](gui)(_.columns == cols)

  private def findGamePanel(gui: AnyRef): Panel =
    findByType[Panel](gui)(p => p.preferredSize.width == 900 && p.preferredSize.height == 600)

  private def getDeckRect(gui: AnyRef): Rectangle =
    findByType[Rectangle](gui)(_ => true)

  private def getHandHitboxes(gui: AnyRef): Vector[(Int, Rectangle)] = {
    val fields = allFields(gui.getClass)
    fields.foreach(_.setAccessible(true))
    fields
      .iterator
      .map(f => f.get(gui))
      .collect { case v: Vector[?] => v }
      .collectFirst {
        case v if v.headOption.exists {
          case (i: Int, r: Rectangle) => true
          case _ => false
        } =>
          v.asInstanceOf[Vector[(Int, Rectangle)]]
      }
      .getOrElse(Vector.empty)
  }

  "GUI" should {

    "wire button clicks to controller methods" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController

      Swing.onEDTWait {
        val gui = new TestGUI(c)

        val startButton = findButton(gui, "Spiel starten")
        startButton.peer.doClick()

        val nameField = findTextFieldByColumns(gui, 30)
        val startGameButton = findButton(gui, "Spiel mit diesen Spielern starten")

        nameField.text = "Ann Ben"
        startGameButton.peer.doClick()
        c.startedWith shouldBe Some(Seq("Ann", "Ben"))

        val colorField = findTextFieldByColumns(gui, 5)
        val colorButton = findButton(gui, "Farbe setzen")
        colorField.text = "r"
        colorButton.peer.doClick()
        c.chooseCalls shouldBe Vector("r")
        colorField.text shouldBe ""

        val endTurnButton = findButton(gui, "Zug beenden")
        endTurnButton.peer.doClick()
        c.endTurnCalls shouldBe 1

        gui.dispose()
      }
    }

    "react to mouse press on deck and on hand card" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController
      c.hand = Vector(Card(Red, Number(1)), Card(Blue, Skip))
      c.top = Some(Card(Green, Number(9)))
      c.deck = 12
      c.current = "A"

      Swing.onEDTWait {
        val gui = new TestGUI(c)

        val panel = findGamePanel(gui)
        panel.peer.setSize(900, 600)

        val img = new BufferedImage(900, 600, BufferedImage.TYPE_INT_ARGB)
        val g2 = img.createGraphics()
        panel.peer.paint(g2)
        g2.dispose()

        val deckRect = getDeckRect(gui)
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
        panel.peer.dispatchEvent(meDeck)
        c.drawCalls shouldBe 1

        val hitboxes = getHandHitboxes(gui)
        hitboxes.nonEmpty shouldBe true
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
        panel.peer.dispatchEvent(meCard)
        c.playCalls shouldBe Vector(idx)

        gui.dispose()
      }
    }

    "update should repaint without showing dialogs when game is not over" in {
      assume(!GraphicsEnvironment.isHeadless)

      val c = new SpyController
      c.gameOver = false

      Swing.onEDTWait {
        val gui = new TestGUI(c)
        noException should be thrownBy gui.update
        gui.dispose()
      }
    }
  }
}
