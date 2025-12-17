package de.htwg.se.uno2.controller.view

import de.htwg.se.uno2.core.api.Game.Snapshot

object GameSnapshotRenderer:

  def render(s: Snapshot): String =
    val handStr = s.currentHand.zipWithIndex
      .map { case (c, i) => s"[$i] $c" }
      .mkString(", ")
    val top = s.topDiscard.getOrElse("Keine Karte")

    s"""
       |Aktueller Spieler: ${s.currentPlayerName}
       |Oberste Karte: $top
       |Hand: $handStr
       |Deckgröße: ${s.deckSize}
       |Befehl: ${if s.awaitingColor then "color r|y|g|b | quit" else "play <i> | draw | quit"}
       |""".stripMargin
