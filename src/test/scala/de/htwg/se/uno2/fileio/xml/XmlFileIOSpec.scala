package de.htwg.se.uno2.fileio.xml

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

import de.htwg.se.uno2.core.impl.model.*
import de.htwg.se.uno2.core.impl.model.Color.*
import de.htwg.se.uno2.core.impl.model.Rank.*

import java.nio.file.Files

final class XmlFileIOSpec extends AnyWordSpec with Matchers {

  private def withTempDir[A](f: java.nio.file.Path => A): A = {
    val old = System.getProperty("user.dir")
    val dir = Files.createTempDirectory("uno2-xml-")
    System.setProperty("user.dir", dir.toAbsolutePath.toString)
    try f(dir)
    finally System.setProperty("user.dir", old)
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
      winnerName = Some("A")
    )

  "XmlFileIO" should {

    "save and load round-trip (relevant fields preserved)" in withTempDir { _ =>
      val io = new XmlFileIO
      val gs = sampleState

      io.save(gs)
      val loaded = io.load()
      loaded.isDefined shouldBe true

      val g2 = loaded.get
      g2.deck.cards shouldBe gs.deck.cards
      g2.discard shouldBe gs.discard
      g2.players.map(_.name) shouldBe gs.players.map(_.name)
      g2.players.map(_.hand) shouldBe gs.players.map(_.hand)
      g2.currentPlayerIndex shouldBe gs.currentPlayerIndex
      g2.chosenColor shouldBe gs.chosenColor
      g2.awaitingColor shouldBe gs.awaitingColor
      g2.ruleSet shouldBe gs.ruleSet
      g2.direction shouldBe gs.direction
      g2.pendingWild shouldBe gs.pendingWild
      g2.winnerName shouldBe gs.winnerName
      g2.pendingNumber shouldBe None
    }

    "load should return None when file is missing" in withTempDir { _ =>
      val io = new XmlFileIO
      io.load() shouldBe None
    }

    "load should return None on invalid xml" in withTempDir { dir =>
      val p = dir.resolve("uno2.xml")
      Files.writeString(p, "<uno2><meta></meta>")
      val io = new XmlFileIO
      io.load() shouldBe None
    }

    "load should return None on unknown rank" in withTempDir { dir =>
      val p = dir.resolve("uno2.xml")
      val bad =
        """
          |<uno2>
          |  <meta>
          |    <ruleSet>classic</ruleSet>
          |    <currentPlayerIndex>0</currentPlayerIndex>
          |    <direction>1</direction>
          |    <awaitingColor>false</awaitingColor>
          |    <chosenColor></chosenColor>
          |    <pendingWild></pendingWild>
          |    <winnerName></winnerName>
          |  </meta>
          |  <deck>
          |    <card color="Red" kind="Nope" n=""/>
          |  </deck>
          |  <discard>
          |    <card color="Red" kind="Skip" n=""/>
          |  </discard>
          |  <players>
          |    <player name="A"><hand></hand></player>
          |  </players>
          |</uno2>
          |""".stripMargin
      Files.writeString(p, bad)
      val io = new XmlFileIO
      io.load() shouldBe None
    }

    "load should return None on unknown color" in withTempDir { dir =>
      val p = dir.resolve("uno2.xml")
      val bad =
        """
          |<uno2>
          |  <meta>
          |    <ruleSet>classic</ruleSet>
          |    <currentPlayerIndex>0</currentPlayerIndex>
          |    <direction>1</direction>
          |    <awaitingColor>false</awaitingColor>
          |    <chosenColor></chosenColor>
          |    <pendingWild></pendingWild>
          |    <winnerName></winnerName>
          |  </meta>
          |  <deck>
          |    <card color="Purple" kind="Skip" n=""/>
          |  </deck>
          |  <discard>
          |    <card color="Red" kind="Skip" n=""/>
          |  </discard>
          |  <players>
          |    <player name="A"><hand></hand></player>
          |  </players>
          |</uno2>
          |""".stripMargin
      Files.writeString(p, bad)
      val io = new XmlFileIO
      io.load() shouldBe None
    }
  }
}
