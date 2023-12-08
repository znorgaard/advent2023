package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DaySevenTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "32T3K 765",
    "T55J5 684",
    "KK677 28",
    "KTJJT 220",
    "QQQJA 483"
  )

  // First test
  test("buildHand") {
    DaySeven.buildHand("AAAAA") shouldBe Seq(14, 14, 14, 14, 14)
    DaySeven.buildHand("AA8AA") shouldBe Seq(14, 14, 8, 14, 14)
    DaySeven.buildHand("23456") shouldBe Seq(2, 3, 4, 5 ,6)
    DaySeven.buildHand("AAAAA", DaySeven.jokerCardMap) shouldBe Seq(14, 14, 14, 14, 14)
    DaySeven.buildHand("AA8AJ") shouldBe Seq(14, 14, 8, 14, 11)
    DaySeven.buildHand("AA8AJ", DaySeven.jokerCardMap) shouldBe Seq(14, 14, 8, 14, 0)
  }

  test("rankHand") {
    DaySeven.rankHand(Seq(14, 14, 14, 14, 14)) shouldBe 7
    DaySeven.rankHand(Seq(14, 13, 14, 14, 14)) shouldBe 6
    DaySeven.rankHand(Seq(14, 14, 12, 12, 12)) shouldBe 5
    DaySeven.rankHand(Seq(14, 13, 12, 12, 12)) shouldBe 4
    DaySeven.rankHand(Seq(14, 13, 13, 11, 11)) shouldBe 3
    DaySeven.rankHand(Seq(14, 13, 12, 11, 11)) shouldBe 2
    DaySeven.rankHand(Seq(14, 13, 12, 11, 10)) shouldBe 1
  }

  test("rankJokerHand") {
    DaySeven.rankJokerHand(Seq(14, 14, 14, 0, 14)) shouldBe 7
    DaySeven.rankJokerHand(Seq(14, 13, 14, 0, 14)) shouldBe 6
    DaySeven.rankJokerHand(Seq(14, 14, 12, 0, 12)) shouldBe 5
    DaySeven.rankJokerHand(Seq(14, 13, 12, 0, 12)) shouldBe 4
    DaySeven.rankJokerHand(Seq(14, 13, 12, 0, 11)) shouldBe 2
    DaySeven.rankJokerHand(Seq(0, 0, 0, 0, 0)) shouldBe 7
    DaySeven.rankJokerHand(Seq(14, 0, 0, 0, 0)) shouldBe 7
  }

  test("readLine") {
    val parsedLines = Seq(
      (Seq(3, 2, 10, 3, 13), 765),
      (Seq(10, 5, 5, 11, 5), 684),
      (Seq(13, 13, 6, 7, 7), 28),
      (Seq(13, 10, 11, 11, 10), 220),
      (Seq(12, 12, 12, 11, 14), 483)
    )
    testInput.map(DaySeven.readLine) should equal(parsedLines)
  }

  test("HandOrdering") {
    val hands = Seq(
      Seq(3, 2, 10, 3, 13),
      Seq(10, 5, 5, 11, 5),
      Seq(13, 13, 6, 7, 7),
      Seq(13, 10, 11, 11, 10),
      Seq(12, 12, 12, 11, 14)
    )
    val sortedHands = Seq(
      Seq(3, 2, 10, 3, 13),
      Seq(13, 10, 11, 11, 10),
      Seq(13, 13, 6, 7, 7),
      Seq(10, 5, 5, 11, 5),
      Seq(12, 12, 12, 11, 14)
    )
    hands.sorted(HandOrdering) should equal(sortedHands)
  }

  test("JokerHandOrdering") {
    val hands = Seq(
      Seq(3, 2, 10, 3, 13),
      Seq(10, 5, 5, 0, 5),
      Seq(13, 13, 6, 7, 7),
      Seq(13, 10, 0, 0, 10),
      Seq(12, 12, 12, 0, 14)
    )
    val sortedHands = Seq(
      Seq(3, 2, 10, 3, 13),
      Seq(13, 13, 6, 7, 7),
      Seq(10, 5, 5, 0, 5),
      Seq(12, 12, 12, 0, 14),
      Seq(13, 10, 0, 0, 10)
    )
    hands.sorted(JokerHandOrdering) should equal(sortedHands)
  }

  test("partOne") {
    DaySeven.partOne(Iterator("32T3K 765")) shouldBe 765
    DaySeven.partOne(testInput.iterator) shouldBe 6440
  }

  test("partTwo") {
    DaySeven.partTwo(Iterator("32T3K 765")) shouldBe 765
    DaySeven.partTwo(testInput.iterator) shouldBe 5905
  }
}
