package de.htwg.se.uno2.fileio.xml

import de.htwg.se.uno2.fileio.FileIOInterface
import de.hwtg.se.uno2.core.impl.model.*
import java.io.{File,PrintWriter}
import scala.util.Try
import scala.xml.{Elem, Node, PrettyPrinter, XML}

class XmlFileIO extends FileIOInterface:
  private val filename = "uno2.xml"
  
  override def save(state: GameState): Unit =
    val xml = game
