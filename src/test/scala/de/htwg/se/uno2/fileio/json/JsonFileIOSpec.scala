package de.htwg.se.uno2.fileio.json

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import de.htwg.se.uno2.core.impl.model.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

final class JsonFileIOSpec extends AnyWordSpec with Matchers {

  import Color.*
  import Rank.*

  private val target: Path =
    Paths.get("uno2.json").toAbsolutePath.normalize()

  private def withIsolatedTarget[A](body: Path => A): A = {
    val backup = target.resolveSibling(target.getFileName.toString + ".scalatest.bak")
    val hadOriginal = Files.exists(target)

    if (hadOriginal) Files.move(target, backup, StandardCopyOption.REPLACE_EXISTING)

    try body(target)
    finally {
      if (Files.exists(target)) Files.delete(target)
      if (hadOriginal && Files.exists(backup)) Files.move(backup, target, StandardCopyOption.REPLACE_EXISTING)
      if (!hadOriginal && Files.exists(backup)) Files.delete(backup)
    }
  }

  private def C(c: Color, r: Rank): Card = Card(c, r)

  private def sampleState: GameState =
    GameState(
      deck = Deck(Vector(C(Red, Number(1)), C(Black, WildDrawFour), C(Blue, Skip))),
      discard = Vector(C(Green, Number(9)), C(Yellow, DrawTwo)),
      players = Vector(
        Player("A", Vector(C(Red, Reverse), C(Black, Wild))),
        Player("B", Vector(C(Blue, Number(7))))
      ),
      currentPlayerIndex = 1,
      chosenColor = Some(Blue),
      awaitingColor = true,
      ruleSet = ColorOnlyRuleSet,
      direction = -1,
      pendingWild = Some(WildDrawFour),
      winnerName = Some("A"),
      pendingNumber = None
    )

  "JsonFileIO" should {

    "return None when no file exists" in withIsolatedTarget { _ =>
      val fileIO = new JsonFileIO
      fileIO.load() shouldBe None
    }

    "save and load a game state" in withIsolatedTarget { path =>
      val fileIO = new JsonFileIO
      val s = sampleState

      fileIO.save(s)
      Files.exists(path) shouldBe true

      val loaded = fileIO.load()
      loaded.isDefined shouldBe true

      val l = loaded.get
      l.players.map(_.name) shouldBe Vector("A", "B")
      l.currentPlayerIndex shouldBe 1
      l.chosenColor shouldBe Some(Blue)
      l.awaitingColor shouldBe true
      l.direction shouldBe -1
      l.pendingWild shouldBe Some(WildDrawFour)
      l.winnerName shouldBe Some("A")
    }

    "return None for invalid json" in withIsolatedTarget { path =>
      Files.write(path, "{ not-json".getBytes(StandardCharsets.UTF_8))
      val fileIO = new JsonFileIO
      fileIO.load() shouldBe None
    }

    "return None for empty json object" in withIsolatedTarget { path =>
      Files.write(path, "{}".getBytes(StandardCharsets.UTF_8))
      val fileIO = new JsonFileIO
      fileIO.load() shouldBe None
    }
  }
}
