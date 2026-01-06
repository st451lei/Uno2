package de.htwg.se.uno2.controller.view

import de.htwg.se.uno2.core.Game.Snapshot

object GameSnapshotRenderer:

  def render(s: Snapshot): String =
    val handStr = s.currentHand.zipWithIndex
      .map { case (c, i) => s"[$i] $c" }
      .mkString(", ")
    val top = s.topDiscard.getOrElse("Keine Karte")

    val winnerLine =
      s.winnerName match
        case Some(w) => s"\n Gewinner: $w\n"
        case None    => ""
        
    val cmd =
      if s.winnerName.isDefined then
        "Spiel ist vorbei. (quit | undo | redo)"
      else if s.awaitingColor then
        "color r|y|g|b | undo | redo | quit"
      else
        "play <i> | draw | undo | redo | quit"

    s"""
       |Aktueller Spieler: ${s.currentPlayerName}
       |Oberste Karte: $top
       |Hand: $handStr
       |Deckgröße: ${s.deckSize}$winnerLine
       |Befehl: $cmd
       |""".stripMargin
