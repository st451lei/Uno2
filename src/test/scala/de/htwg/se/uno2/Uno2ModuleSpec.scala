package de.htwg.se.uno2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import com.google.inject.Guice
import net.codingwell.scalaguice.InjectorExtensions._

import de.htwg.se.uno2.fileio.FileIOInterface
import de.htwg.se.uno2.fileio.json.JsonFileIO
import de.htwg.se.uno2.fileio.xml.XmlFileIO

final class Uno2ModuleSpec extends AnyWordSpec with Matchers {

  "Uno2Module" should {

    "bind XmlFileIO by default" in {
      val old = sys.props.get("fileio")
      try {
        sys.props -= "fileio"
        val injector = Guice.createInjector(new Uno2Module)
        val fio = injector.instance[FileIOInterface]
        fio shouldBe a [XmlFileIO]
      } finally {
        old match {
          case Some(v) => sys.props("fileio") = v
          case None    => sys.props -= "fileio"
        }
      }
    }

    "bind JsonFileIO when sys.props(fileio)=json" in {
      val old = sys.props.get("fileio")
      try {
        sys.props("fileio") = "json"
        val injector = Guice.createInjector(new Uno2Module)
        val fio = injector.instance[FileIOInterface]
        fio shouldBe a [JsonFileIO]
      } finally {
        old match {
          case Some(v) => sys.props("fileio") = v
          case None    => sys.props -= "fileio"
        }
      }
    }
  }
}
