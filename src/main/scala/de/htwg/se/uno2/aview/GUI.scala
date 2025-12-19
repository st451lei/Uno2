package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.Controller
import de.htwg.se.uno2.util.Observer

import scala.swing.*
import scala.swing.event.*
import java.awt.{Font, Graphics2D, RenderingHints, Color as AwtColor}
import scala.swing.MenuBar.NoMenuBar.revalidate

  class GUI(controller: Controller) extends Frame with Observer:

    private val cardWidth = 80
    private val cardHeight = 120
    private val cardGap = 15

    private def colorToAwt(c: de.htwg.se.uno2.model.Color): AwtColor =
      import de.htwg.se.uno2.model.Color._
      c match
        case Red => AwtColor.RED
        case Yellow => AwtColor.YELLOW
        case Green => AwtColor.GREEN
        case Blue => AwtColor.BLUE
        case Black => AwtColor.BLACK

    private def rankToString(card: de.htwg.se.uno2.model.Card): String =
      import de.htwg.se.uno2.model.Rank
      card.rank match
        case Rank.Number(n) => n.toString
        case Rank.Skip => "->"
        case Rank.Reverse => "REv"
        case Rank.DrawTwo => "+2"
        case Rank.Wild => "W"
        case Rank.WildDrawFour => "W+4"

    private val gamePanelCenter = new Panel:

      preferredSize = new Dimension(900, 600)

      override def paintComponent(g: Graphics2D): Unit =
        super.paintComponent(g)

        // Anti-Aliasing für schöneren Text/Grafik
        g.setRenderingHint(
          RenderingHints.KEY_ANTIALIASING,
          RenderingHints.VALUE_ANTIALIAS_ON
        )

        // Hintergrund
        g.setColor(new AwtColor(0, 120, 0)) // dunkelgrün wie Spielfeld
        g.fillRect(0, 0, size.width, size.height)

        // Aktueller GameState
        val hand = controller.currentHand
        val top = controller.topDiscard
        val deckCount = controller.deckSize
        val player = controller.currentPlayerName

        // Titel oben
        g.setColor(AwtColor.WHITE)
        g.setFont(new Font("Arial", Font.BOLD, 24))
        g.drawString(s"Spieler: $player", 20, 40)

        // Deck links-mitte
        drawDeck(g, deckCount, x = 50, y = size.height / 2 - cardHeight / 2)

        // Ablagestapel daneben
        drawDiscard(g, top, x = 180, y = size.height / 2 - cardHeight / 2)

        // Hand des Spielers unten
        drawHand(g, hand, startX = 50, y = size.height - cardHeight - 50)

    private def drawDeck(g: Graphics2D, count: Int, x: Int, y: Int): Unit =
      // Rückseite des Stapels
      g.setColor(AwtColor.DARK_GRAY)
      g.fillRoundRect(x, y, cardWidth, cardHeight, 15, 15)
      g.setColor(AwtColor.WHITE)
      g.setFont(new Font("Arial", Font.BOLD, 18))
      g.drawString("UNO", x + 15, y + cardHeight / 2)
      g.setFont(new Font("Arial", Font.PLAIN, 12))
      g.drawString(s"$count Karten", x + 5, y + cardHeight - 15)

    private def drawDiscard(g: Graphics2D, top: Option[de.htwg.se.uno2.model.Card], x: Int, y: Int): Unit =
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

    private def drawHand(g: Graphics2D, hand: Vector[de.htwg.se.uno2.model.Card], startX: Int, y: Int): Unit =
      var x = startX
      for (card, idx) <- hand.zipWithIndex do
        val c = colorToAwt(card.color)
        g.setColor(c)
        g.fillRoundRect(x, y, cardWidth, cardHeight, 15, 15)

        g.setColor(if c == AwtColor.YELLOW || c == AwtColor.WHITE then AwtColor.BLACK else AwtColor.WHITE)
        g.setFont(new Font("Arial", Font.BOLD, 20))
        g.drawString(rankToString(card), x + 10, y + 30)

        // optional: kleinen Index auf die Karte schreiben
        g.setFont(new Font("Arial", Font.PLAIN, 12))
        g.drawString(s"[$idx]", x + 10, y + cardHeight - 10)

        x += cardWidth + cardGap

    title = "UNO2"

    private val drawButton = new Button("Karte ziehen")
    private val playLabel = new Label("Index der zu spielenden Karte: ")
    private val playField = new TextField { columns = 5 }
    private val playButton = new Button("Karte spielen")

    private val colorLabel = new Label("Farbe wählen (r/g/b/y): ")
    private val colorField = new TextField { columns = 5}
    private val colorButton = new Button("Farbe setzen")

    private val gamePanel: BorderPanel = new BorderPanel:
      layout(gamePanelCenter) = BorderPanel.Position.Center

      layout(new GridPanel(3,1) {
        contents += new FlowPanel(drawButton)

        contents += new FlowPanel(
          playLabel,
          playField,
          playButton
        )

        contents += FlowPanel(
          colorLabel,
          colorField,
          colorButton
        )
      }) = BorderPanel.Position.South

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

    private val nameInfoLabel = new Label("Spielernamen eingeben (z.B. 'Anna Ben Chris'):")
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
      gamePanelCenter.repaint()

    listenTo(startButton)
    listenTo(startGameButton)
    listenTo(drawButton)
    listenTo(playButton)
    listenTo(colorButton)

    reactions += {
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
      case ButtonClicked(`drawButton`) =>
        controller.drawCard

      case ButtonClicked(`playButton`) =>
        val indexText = playField.text.trim
        if indexText.nonEmpty then
          try
            val idx = indexText.toInt
            controller.playCard(idx)
          catch
            case _: NumberFormatException =>
              ()
        playField.text = ""

      case ButtonClicked(`colorButton`) =>
      val colorInput = colorField.text.trim
      if colorInput.nonEmpty then
        controller.chooseColor(colorInput)
      colorField.text = ""
    }
    contents = startPanel
    centerOnScreen()
    visible = true