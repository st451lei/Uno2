package de.htwg.se.uno2.core.impl.model

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

final class GameStateSpec extends AnyWordSpec with Matchers {

  import Color.*
  import Rank.*

  private def C(c: Color, r: Rank): Card = Card(c, r)

  private def mkState(
                       deckCards: Vector[Card],
                       discardTop: Card,
                       players: Vector[Player],
                       cur: Int = 0,
                       chosen: Option[Color] = None,
                       awaiting: Boolean = false,
                       rule: RuleSet = ClassicRuleSet,
                       dir: Int = 1,
                       pendingWild: Option[Rank] = None,
                       winner: Option[String] = None,
                       pendingNumber: Option[Int] = None
                     ): GameState =
    GameState(
      deck = Deck(deckCards),
      discard = Vector(discardTop),
      players = players,
      currentPlayerIndex = cur,
      chosenColor = chosen,
      awaitingColor = awaiting,
      ruleSet = rule,
      direction = dir,
      pendingWild = pendingWild,
      winnerName = winner,
      pendingNumber = pendingNumber
    )

  "GameState.parseColor" should {

    "parse multiple aliases" in {
      GameState.parseColor("r") shouldBe Some(Red)
      GameState.parseColor("rot") shouldBe Some(Red)

      GameState.parseColor("g") shouldBe Some(Green)
      GameState.parseColor("grün") shouldBe Some(Green)
      GameState.parseColor("gruen") shouldBe Some(Green)

      GameState.parseColor("x") shouldBe None
    }
  }

  "GameState.drawCard" should {

    "do nothing when awaitingColor or gameOver" in {
      val p = Player("A", Vector.empty)
      val s1 = mkState(Vector(C(Red, Number(1))), C(Blue, Number(0)), Vector(p), awaiting = true)
      s1.drawCard shouldBe s1

      val s2 = mkState(Vector(C(Red, Number(1))), C(Blue, Number(0)), Vector(p), winner = Some("A"))
      s2.drawCard shouldBe s2
    }

    "draw from deck and add to current player's hand" in {
      val p = Player("A", Vector.empty)
      val s = mkState(Vector(C(Red, Number(1)), C(Green, Number(2))), C(Blue, Number(0)), Vector(p))

      val s2 = s.drawCard
      s2.players(0).hand shouldBe Vector(C(Red, Number(1)))
      s2.deck.size shouldBe 1
    }
  }

  "GameState.playCard & endTurn" should {

    "do nothing for invalid index or when cannot play" in {
      val p = Player("A", Vector(C(Green, Number(9))))
      val s = mkState(Vector.empty, C(Red, Number(1)), Vector(p))

      s.playCard(-1) shouldBe s
      s.playCard(99) shouldBe s
      s.playCard(0) shouldBe s
    }

    "play Number sets pendingNumber and enables endTurn; endTurn advances and clears pendingNumber" in {
      val p0 = Player("A", Vector(C(Red, Number(7))))
      val p1 = Player("B", Vector.empty)
      val s = mkState(Vector.empty, C(Red, Number(1)), Vector(p0, p1))

      val afterPlay = s.playCard(0)
      afterPlay.pendingNumber shouldBe Some(7)
      afterPlay.canEndTurn shouldBe true

      val afterEnd = afterPlay.endTurn
      afterEnd.pendingNumber shouldBe None
      afterEnd.currentPlayerIndex shouldBe 1
    }

    "endTurn does nothing when canEndTurn is false" in {
      val p0 = Player("A", Vector(C(Red, Skip)))
      val p1 = Player("B", Vector.empty)
      val s = mkState(Vector.empty, C(Red, Number(1)), Vector(p0, p1))

      s.canEndTurn shouldBe false
      s.endTurn shouldBe s
    }

    "pendingNumber restricts: only Number(expected) is allowed; different number is rejected" in {
      val p0 = Player("A", Vector(C(Red, Number(5)), C(Red, Number(6))))
      val p1 = Player("B", Vector.empty)
      val s0 = mkState(Vector.empty, C(Red, Number(1)), Vector(p0, p1))

      val s1 = s0.playCard(0)
      s1.pendingNumber shouldBe Some(5)

      val s2 = s1.playCard(0)
      s2 shouldBe s1
    }

    "Skip advances by 2" in {
      val p0 = Player("A", Vector(C(Red, Skip)))
      val p1 = Player("B", Vector.empty)
      val p2 = Player("C", Vector.empty)
      val s = mkState(Vector.empty, C(Red, Number(1)), Vector(p0, p1, p2), cur = 0)

      val s2 = s.playCard(0)
      s2.currentPlayerIndex shouldBe 2
    }

    "DrawTwo gives 2 cards to next player and advances by 2" in {
      val p0 = Player("A", Vector(C(Red, DrawTwo)))
      val p1 = Player("B", Vector.empty)
      val p2 = Player("C", Vector.empty)

      val deckCards = Vector(C(Green, Number(1)), C(Blue, Number(2)), C(Yellow, Number(3)))
      val s = mkState(deckCards, C(Red, Number(1)), Vector(p0, p1, p2), cur = 0)

      val s2 = s.playCard(0)
      s2.players(1).hand.size shouldBe 2
      s2.deck.size shouldBe 1
      s2.currentPlayerIndex shouldBe 2
    }

    "Reverse cases for players<=1, players==2, players>=3" in {
      val s1 = mkState(Vector.empty, C(Red, Number(1)), Vector(Player("A", Vector(C(Red, Reverse)))), cur = 0)
      val r1 = s1.playCard(0)
      r1.direction shouldBe -1
      r1.currentPlayerIndex shouldBe 0

      val s2 = mkState(
        Vector.empty,
        C(Red, Number(1)),
        Vector(Player("A", Vector(C(Red, Reverse))), Player("B", Vector.empty)),
        cur = 0
      )
      val r2 = s2.playCard(0)
      r2.direction shouldBe -1
      r2.currentPlayerIndex shouldBe 0

      val s3 = mkState(
        Vector.empty,
        C(Red, Number(1)),
        Vector(
          Player("A", Vector(C(Red, Reverse))),
          Player("B", Vector.empty),
          Player("C", Vector.empty)
        ),
        cur = 0
      )
      val r3 = s3.playCard(0)
      r3.direction shouldBe -1
      r3.currentPlayerIndex shouldBe 2
    }

    "Wild and WildDrawFour set awaitingColor and pendingWild without advancing player" in {
      val p0 = Player("A", Vector(C(Black, Wild), C(Black, WildDrawFour)))
      val p1 = Player("B", Vector.empty)
      val top = C(Red, Number(1))

      val s = mkState(Vector.empty, top, Vector(p0, p1), cur = 0)

      val w = s.playCard(0)
      w.awaitingColor shouldBe true
      w.pendingWild shouldBe Some(Wild)
      w.currentPlayerIndex shouldBe 0

      val w4 = s.playCard(1)
      w4.awaitingColor shouldBe true
      w4.pendingWild shouldBe Some(WildDrawFour)
      w4.currentPlayerIndex shouldBe 0
    }

    "chooseColor: invalid token keeps state; valid resolves pendingWild=Wild and advances by 1" in {
      val p0 = Player("A", Vector.empty)
      val p1 = Player("B", Vector.empty)

      val s = mkState(
        deckCards = Vector.empty,
        discardTop = C(Black, Wild),
        players = Vector(p0, p1),
        cur = 0,
        awaiting = true,
        pendingWild = Some(Wild)
      )

      s.chooseColor("nope") shouldBe s

      val s2 = s.chooseColor("r")
      s2.awaitingColor shouldBe false
      s2.chosenColor shouldBe Some(Red)
      s2.pendingWild shouldBe None
      s2.currentPlayerIndex shouldBe 1
    }

    "chooseColor: pendingWild=WildDrawFour gives 4 cards to next player and advances by 2" in {
      val p0 = Player("A", Vector.empty)
      val p1 = Player("B", Vector.empty)
      val p2 = Player("C", Vector.empty)

      val deckCards = Vector(
        C(Green, Number(1)),
        C(Blue, Number(2)),
        C(Yellow, Number(3)),
        C(Red, Number(4)),
        C(Red, Number(5))
      )

      val s = mkState(
        deckCards = deckCards,
        discardTop = C(Black, WildDrawFour),
        players = Vector(p0, p1, p2),
        cur = 0,
        awaiting = true,
        pendingWild = Some(WildDrawFour)
      )

      val s2 = s.chooseColor("blue")
      s2.players(1).hand.size shouldBe 4
      s2.deck.size shouldBe 1
      s2.currentPlayerIndex shouldBe 2
      s2.pendingWild shouldBe None
      s2.awaitingColor shouldBe false
      s2.chosenColor shouldBe Some(Blue)
    }

    "winner: playing last card sets winnerName and ends game" in {
      val p0 = Player("A", Vector(C(Red, Number(7))))
      val p1 = Player("B", Vector.empty)

      val s = mkState(Vector.empty, C(Red, Number(1)), Vector(p0, p1), cur = 0)
      val s2 = s.playCard(0)

      s2.isGameOver shouldBe true
      s2.winnerName shouldBe Some("A")
      s2.awaitingColor shouldBe false
      s2.pendingWild shouldBe None
      s2.pendingNumber shouldBe None
    }
  }
}
