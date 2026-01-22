package de.htwg.se.uno2.fileio.xml

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.{Files, Path}

import de.htwg.se.uno2.core.impl.model.*

class XmlFileIOSpec extends AnyWordSpec with Matchers:

  import Color.*
  import Rank.*

  private def deleteRecursive(path: Path): Unit =
    if Files.exists(path) then
      val stream = Files.walk(path)
      try
        stream
          .sorted(java.util.Comparator.reverseOrder())
          .forEach(p => Files.deleteIfExists(p))
      finally stream.close()

  private def prepareDirs(base: Path): Unit =
    List(
      base.resolve("data"),
      base.resolve("src").resolve("main").resolve("resources"),
      base.resolve("src").resolve("test").resolve("resources")
    ).foreach(p => Files.createDirectories(p))

  private def withTempDir(testCode: Path => Any): Unit =
    val oldUserDir = System.getProperty("user.dir")
    val dir = Files.createTempDirectory("uno2-xml-spec")
    prepareDirs(dir)
    System.setProperty("user.dir", dir.toString)
    try testCode(dir)
    finally
      System.setProperty("user.dir", oldUserDir)
      deleteRecursive(dir)

  private def sampleState: GameState =
    GameState(
      Deck(Vector(Card(Red, Number(1)), Card(Black, WildDrawFour), Card(Blue, Skip))),
      Vector(Card(Green, Number(9)), Card(Yellow, DrawTwo)),
      Vector(
        Player("TEST_A", Vector(Card(Red, Reverse), Card(Black, Wild))),
        Player("TEST_B", Vector(Card(Blue, Number(7))))
      ),
      1,
      Some(Blue),
      true,
      ColorOnlyRuleSet,
      -1,
      Some(WildDrawFour),
      Some("TEST_A"),
      None
    )

  "XmlFileIO" should {

    "not throw on load" in withTempDir { _ =>
      val fileIO = new XmlFileIO()
      noException should be thrownBy(fileIO.load())
    }

    "save and then load a game state" in withTempDir { _ =>
      val fileIO = new XmlFileIO()
      val state = sampleState

      noException should be thrownBy(fileIO.save(state))

      val loadedOpt = fileIO.load()
      loadedOpt.isDefined shouldBe true

      val loaded = loadedOpt.get

      loaded.players.map(_.name) shouldBe state.players.map(_.name)
      loaded.currentPlayerIndex shouldBe state.currentPlayerIndex
      loaded.chosenColor shouldBe state.chosenColor
      loaded.awaitingColor shouldBe state.awaitingColor
      loaded.direction shouldBe state.direction
      loaded.winnerName shouldBe state.winnerName
    }
  }
