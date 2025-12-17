package de.htwg.se.uno2.aview

import de.htwg.se.uno2.controller.api.ControllerInterface
import de.htwg.se.uno2.util.Observer

import scala.swing.*
import scala.swing.event.*

class GUI(controller: ControllerInterface) extends Frame with Observer:

  title = "UNO - Scala Swing"
  preferredSize = new Dimension(800, 600)

  private val outputArea = new TextArea {
    editable = false
    rows = 15
    lineWrap = true
    wordWrap = true
  }

  private val drawButton = new Button("Karte ziehen")
  private val playLabel = new Label("Index der zu spielenden Karte: ")
  private val playField = new TextField { columns = 5 }
  private val playButton = new Button("Karte spielen")

  private val colorLabel = new Label("Farbe wählen (r/g/b/y): ")
  private val colorField = new TextField { columns = 5}
  private val colorButton = new Button("Farbe setzen")

  contents = new BorderPanel {
    layout(new ScrollPane(outputArea)) = BorderPanel.Position.Center

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
  }

  peer.setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE)

  override def update: Unit =
    outputArea.text = controller.gameStateToString

  listenTo(drawButton)
  listenTo(playButton)
  listenTo(colorButton)

  reactions += {
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
  
  visible = true
  outputArea.text = controller.gameStateToString