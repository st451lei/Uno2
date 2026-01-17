package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.ControllerInterface
import de.htwg.se.uno2.util.Observer
import de.htwg.se.uno2.core.impl.model.*
import de.htwg.se.uno2.core.impl.model.Color
import de.htwg.se.uno2.core.impl.model.Color.*

import java.awt
import scala.swing.*
import scala.swing.event.*
import java.awt.{Font, Graphics2D, Rectangle, RenderingHints, Color as AwtColor}
import scala.swing.MenuBar.NoMenuBar.revalidate


class GUI(controller: ControllerInterface) extends Frame with Observer:

  title = "UNO2"
  private var handHitboxes: Vector[(Int, Rectangle)] = Vector.empty
  private var deckRect: Rectangle = new Rectangle(0, 0, 0, 0)

  private var hoveredIndex: Option[Int] = None
  private val cardWidth = 80
  private val cardHeight = 120
  private val cardGap = 15
  private var gameOverDialogShown: Boolean = false

  private def colorToAwt(c: Color): AwtColor =
    c match
      case Red => AwtColor.RED
      case Yellow => AwtColor.YELLOW
      case Green => AwtColor.GREEN
      case Blue => AwtColor.BLUE
      case Black => AwtColor.BLACK

  private def rankToString(card: Card): String =
    card.rank match
      case Rank.Number(n) => n.toString
      case Rank.Skip => "->"
      case Rank.Reverse => "Rev"
      case Rank.DrawTwo => "+2"
      case Rank.Wild => "W"
      case Rank.WildDrawFour => "W+4"

  private def maybeShowGameOverDialog(): Unit =
    if controller.isGameOver && !gameOverDialogShown then
      gameOverDialogShown = true
      val winner = controller.winnerName.getOrElse(controller.currentPlayerName)
      Dialog.showMessage(
        parent = this,
        message = s" $winner hat gewonnen",
        title = "Spiel beendet",
        messageType = Dialog.Message.Info
      )
    else if !controller.isGameOver then
      gameOverDialogShown = false

  private val gamePanelCenter = new Panel:

    preferredSize = new Dimension(900, 600)
    override def paintComponent(g: Graphics2D): Unit =
      super.paintComponent(g)

      g.setRenderingHint(
        RenderingHints.KEY_ANTIALIASING,
        RenderingHints.VALUE_ANTIALIAS_ON
      )

      g.setColor(new AwtColor(0, 120, 0))
      g.fillRect(0, 0, size.width, size.height)

      val hand = controller.currentHand
      val y = size.height - cardHeight - 40
      val startX = 40

      handHitboxes = hand.zipWithIndex.map { case (card, idx) =>
        val x = startX + idx * (cardWidth + cardGap)
        val rect = new Rectangle(x,y, cardWidth, cardHeight)

        g.setColor(AwtColor.LIGHT_GRAY)
        g.fillRoundRect(x, y, cardWidth, cardHeight, 14, 14)
        g.setColor(AwtColor.BLACK)
        g.drawRoundRect(x, y, cardWidth, cardHeight, 14, 14)

        g.setFont(new Font("Arial", Font.PLAIN, 12))
        g.drawString(s"[$idx]", x + 6, y + cardHeight - 8)

        if hoveredIndex.contains(idx) then
          g.setColor(AwtColor.WHITE)
          g.drawRoundRect(x - 2, y - 2, cardWidth + 4, cardHeight + 4, 14, 14)

        (idx, rect)
      }

      val deckX = 60
      val deckY = size.height/2 - cardHeight/2
      deckRect = new Rectangle(deckX, deckY, cardWidth, cardHeight)

      g.setColor(AwtColor.DARK_GRAY)
      g.fillRoundRect(deckX, deckY, cardWidth, cardHeight, 14, 14)

      val top = controller.topDiscard
      val deckCount = controller.deckSize
      val player = controller.currentPlayerName

      g.setColor(AwtColor.WHITE)
      g.setFont(new Font("Arial", Font.BOLD, 24))
      g.drawString(s"Spieler: $player", 20, 40)

      drawDeck(g, deckCount, x = 50, y = size.height / 2 - cardHeight / 2)

      drawDiscard(g, top, x = 180, y = size.height / 2 - cardHeight / 2)

      drawHand(g, hand, startX = 50, y = size.height - cardHeight - 50)

      drawOpponentCounts(g)

      if controller.isGameOver then
        val winner = controller.winnerName.getOrElse(player)

        g.setColor(new AwtColor(0, 0, 0, 170))
        g.fillRect(0, 0, size.width, size.height)

        g.setColor(AwtColor.WHITE)
        g.setFont(new Font("Arial", Font.BOLD, 48))
        val msg = s"$winner gewinnt!"
        val fm = g.getFontMetrics
        val x = (size.width - fm.stringWidth(msg)) / 2
        val y = size.height / 2
        g.drawString(msg, x, y)

  private def drawDeck(g: Graphics2D, count: Int, x: Int, y: Int): Unit =

    g.setColor(AwtColor.DARK_GRAY)
    g.fillRoundRect(x, y, cardWidth, cardHeight, 15, 15)
    g.setColor(AwtColor.WHITE)
    g.setFont(new Font("Arial", Font.BOLD, 18))
    g.drawString("UNO", x + 15, y + cardHeight / 2)
    g.setFont(new Font("Arial", Font.PLAIN, 12))
    g.drawString(s"$count Karten", x + 5, y + cardHeight - 15)

  private def drawDiscard(g: Graphics2D, top: Option[Card], x: Int, y: Int): Unit =
    top match
      case None =>
        g.setColor(AwtColor.LIGHT_GRAY)
        g.drawRoundRect(x, y, cardWidth, cardHeight, 15, 15)
        g.setColor(AwtColor.WHITE)
        g.drawString("Leer", x + 20, y + cardHeight / 2)
      case Some(card) =>
        val c = colorToAwt(card.color)
        g.setColor(c)
        g.fillRoundRect(x, y, cardWidth, cardHeight, 15, 15)
        g.setColor(if c == AwtColor.YELLOW || c == AwtColor.WHITE then AwtColor.BLACK else AwtColor.WHITE)
        g.setFont(new Font("Arial", Font.BOLD, 24))
        g.drawString(rankToString(card), x + 25, y + cardHeight / 2)

  private def drawHand(g: Graphics2D, hand: Vector[Card], startX: Int, y: Int): Unit =
    var x = startX
    for (card, idx) <- hand.zipWithIndex do
      val c = colorToAwt(card.color)
      g.setColor(c)
      g.fillRoundRect(x, y, cardWidth, cardHeight, 15, 15)

      g.setColor(if c == AwtColor.YELLOW || c == AwtColor.WHITE then AwtColor.BLACK else AwtColor.WHITE)
      g.setFont(new Font("Arial", Font.BOLD, 20))
      g.drawString(rankToString(card), x + 10, y + 30)

      g.setFont(new Font("Arial", Font.PLAIN, 12))
      g.drawString(s"[$idx]", x + 10, y + cardHeight - 10)

      x += cardWidth + cardGap

  private def drawCardStackIcon(g: Graphics2D, x: Int, y: Int, count: Int): Unit =
    val w = 22
    val h = 30
    val offset = 3
    val layers = math.min(3, math.max(1, (count + 9) / 10))

    for i <- 0 until layers do
      val xx = x + i * offset
      val yy = y + i * offset
      g.setColor(new AwtColor(30, 60, 120))
      g.fillRoundRect(xx, yy, w, h, 8, 8)
      g.setColor(AwtColor.WHITE)
      g.drawRoundRect(xx, yy, w, h, 8, 8)

      g.setFont(new Font("Arial", Font.BOLD, 10))
      g.drawString("UNO", xx + 4, yy + 16)

    val badgeR = 10
    val bx = x + w + (layers - 1) * offset - badgeR + 2
    val by = y - badgeR / 2
    g.setColor(new AwtColor(200, 30, 30))
    g.fillOval(bx, by, badgeR * 2, badgeR * 2)
    g.setColor(AwtColor.WHITE)
    g.drawOval(bx, by, badgeR * 2, badgeR * 2)

    val txt = count.toString
    g.setFont(new Font("Arial", Font.BOLD, 11))
    val fm = g.getFontMetrics
    val tx = bx + badgeR - fm.stringWidth(txt) / 2
    val ty = by + badgeR + fm.getAscent / 2 - 1
    g.drawString(txt, tx, ty)
    
  private def drawOpponentCounts(g: Graphics2D): Unit =
    val opp = controller.opponentCardCounts
    if opp.nonEmpty then
      
      title = "Spieler"
      val padding = 10
      val rowH = 38
      val iconW = 22
      val iconH = 30
      val nameGap = 12
      
      g.setFont( new Font("Arial", Font.BOLD, 14))
      val fmTitle = g.getFontMetrics
      g.setFont(new Font("Arial", Font.PLAIN, 14))
      val fm = g.getFontMetrics
      
      val maxNameW = opp.map { case (n, _) => fm.stringWidth(n)}.max
      val boxW = padding + iconW + 18 + nameGap + maxNameW + padding
      val boxH = padding + fmTitle.getHeight + 6 + opp.size * rowH + padding

      val x = size.width - boxW - 30
      val y = 15

      g.setColor(new AwtColor(0, 0, 0, 140))
      g.fillRoundRect(x, y, boxW, boxH, 20, 20)

      g.setColor(AwtColor.WHITE)
      g.setFont(new Font("Arial", Font.BOLD, 15))
      g.drawString(title, x + padding, y + padding + fmTitle.getAscent)
      
      var ry = y + padding + fmTitle.getHeight + 6
      opp.foreach { case (name, cnt) =>
        
        val ix = x + padding
        val iy = ry + (rowH - iconH) / 2
        drawCardStackIcon(g, ix, iy, cnt)
        
        g.setColor(AwtColor.WHITE)
        g.setFont(new Font("Arial", Font.PLAIN, 15))
        val nx = ix + iconW + 18 + nameGap
        val ny = ry + rowH / 2 + fm.getAscent / 2 - 2
        g.drawString(name, nx, ny)
        
        ry += rowH
      }
    
  private def mkColorSquare(token: String, awt: AwtColor, tip: String): Button =
    new Button("") {
      preferredSize = new Dimension(26,26)
      background = awt
      tooltip = tip
      peer.setOpaque(true)
      peer.setContentAreaFilled(true)
      peer.setBorderPainted(true)
      peer.setFocusPainted(false)
    }

  private def chooseColorWithDialog(token: String, colorName: String): Unit =
    controller.chooseColor(token)

    Dialog.showMessage(
      parent = this,
      message = s"Aktive Farbe: $colorName",
      title = "Farbe gesetzt",
      messageType = Dialog.Message.Info
    )
    
  private val colorPromptLabel = new Label("Farbe wählen:")
  private val redSquare = mkColorSquare("r", AwtColor.RED, "Rot")
  private val yellowSquare = mkColorSquare("y", AwtColor.YELLOW, "Gelb")
  private val greenSquare = mkColorSquare("g", AwtColor.GREEN, "Grün")
  private val blueSquare = mkColorSquare("b", AwtColor.BLUE, "Blau")

  private val endTurnButton = new Button("Zug beenden")

  private val saveButton = new Button("Speichern")
  private val loadButton = new Button("Laden")

  private val gamePanel: BorderPanel = new BorderPanel {
    layout(new ScrollPane(gamePanelCenter)) = BorderPanel.Position.Center

    layout(new GridPanel(3,1) {
      contents += FlowPanel(saveButton, loadButton /*, ioStatusLabel */)
      contents += FlowPanel(endTurnButton)

      contents += FlowPanel(
        colorPromptLabel,
        redSquare,
        yellowSquare,
        greenSquare,
        blueSquare
      )
    }) = BorderPanel.Position.South
  }

  private val titleLabel = new Label("Uno2"):
    font = new Font("Arial", java.awt.Font.BOLD, 48)
    horizontalAlignment = Alignment.Center

  private val startButton = new Button:
    text = "Spiel starten"
    preferredSize = new Dimension(200, 60)
    font = new Font("Arial", java.awt.Font.BOLD, 24)

  private val startPanel: BorderPanel = new BorderPanel:
    layout(titleLabel) = BorderPanel.Position.Center
    layout(new FlowPanel(startButton)) = BorderPanel.Position.South

  private val nameInfoLabel = new Label("Spielernamen eingeben:")
  private val nameField = new TextField {
    columns = 30
  }
  private val startGameButton = new Button("Spiel mit diesen Spielern starten")

  private val namePanel: BorderPanel = new BorderPanel:
    layout(new BoxPanel(Orientation.Vertical) {
      contents += Swing.VStrut(30)
      contents += nameInfoLabel
      contents += Swing.VStrut(10)
      contents += nameField
      contents += Swing.VStrut(10)
      contents += startGameButton
      border = Swing.EmptyBorder(20, 20, 20, 20)
    }) = BorderPanel.Position.Center

  peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

  override def update: Unit =
    Swing.onEDT {
      val hasGame = controller.currentPlayerName != "-"
      saveButton.enabled = hasGame
      endTurnButton.enabled = controller.canEndTurn

      val awaiting = controller.isAwaitingColorChoise
      redSquare.enabled = awaiting
      yellowSquare.enabled = awaiting
      greenSquare.enabled = awaiting
      blueSquare.enabled = awaiting

      maybeShowGameOverDialog()
      gamePanelCenter.repaint()
    }

  listenTo(gamePanelCenter.mouse.clicks)
  listenTo(gamePanelCenter.mouse.moves)

  listenTo(startButton)
  listenTo(startGameButton)

  listenTo(redSquare)
  listenTo(yellowSquare)
  listenTo(greenSquare)
  listenTo(blueSquare)

  listenTo(endTurnButton)

  listenTo(saveButton)
  listenTo(loadButton)

  reactions += {

    case e: MouseMoved =>
      val p = e.point
      val newHover = handHitboxes.collectFirst { case (idx, rect) if rect.contains(p) => idx}
      if newHover != hoveredIndex then
        hoveredIndex = newHover
        gamePanelCenter.repaint()

    case e: MousePressed =>
      val p = e.point

      if deckRect.contains(p) then
        val beforeHand = controller.currentHand.size
        controller.drawCard
        val afterHand = controller.currentHand.size

        if beforeHand == afterHand then
          Dialog.showMessage(
            this,
            "Du darfst ziehen, nur wenn du keine spielbare Karte hast",
            "Hinweis", Dialog.Message.Info
          )

      else
        val hit = handHitboxes.collectFirst { case (idx, rect) if rect.contains(p) => idx}
        hit.foreach(controller.playCard)

    case ButtonClicked(`startButton`) =>
      contents = namePanel
      revalidate()
      repaint()

    case ButtonClicked(`startGameButton`) =>
      val raw = nameField.text.trim
      val names = raw.split("[, ]+").filter(_.nonEmpty).toSeq

      if names.isEmpty then
        Dialog.showMessage(
          parent = this,
          message = "Bitte mindestens einen Spielernamen eingeben.",
          title = "Eingabefehler",
          messageType = Dialog.Message.Error
        )
      else
        controller.startGame(names)
        contents = gamePanel
        revalidate()
        repaint()

    case ButtonClicked(`redSquare`) => chooseColorWithDialog("r", "Rot")
    case ButtonClicked(`yellowSquare`) => chooseColorWithDialog("y", "Gelb")
    case ButtonClicked(`greenSquare`) => chooseColorWithDialog("g", "Grün")
    case ButtonClicked(`blueSquare`) => chooseColorWithDialog("b", "Blau")

    case ButtonClicked(`endTurnButton`) =>
      controller.endTurn()

    case ButtonClicked(`saveButton`) =>
      controller.save()
      val file = sys.props.getOrElse("fileio", "xml").toLowerCase match
        case "json" => "uno2.json"
        case _ => "uno2.xml"

      Dialog.showMessage(
        parent = this,
        message = s"Spiel gespeichert ($file).",
        title = "Speichern",
        messageType = Dialog.Message.Info
      )
    case ButtonClicked(`loadButton`) =>
      controller.load()

      if controller.currentPlayerName != "-" then
        contents = gamePanel
        revalidate()
        repaint()
        Dialog.showMessage(
          parent = this,
          message = "Spiel geladen.",
          title = "Laden",
          messageType = Dialog.Message.Info
        )
      else
        Dialog.showMessage(
          parent = this,
          message = "Laden fehlgeschlagen (keine gültige Speicherdatei gefunden oder Parse-Fehler).",
          title = "Laden",
          messageType = Dialog.Message.Warning
        )
  }
  contents = startPanel
  centerOnScreen()
  visible = true