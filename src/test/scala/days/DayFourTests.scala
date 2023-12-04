package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayFourTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
    "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19",
    "Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1",
    "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83",
    "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36",
    "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
  )

  // First test
  test("parseLine") {
    val expectedResults: Seq[Seq[Seq[Int]]] = Seq(
      Seq(Seq(1), Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)),
      Seq(Seq(2), Seq(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19)),
      Seq(Seq(3), Seq(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1)),
      Seq(Seq(4), Seq(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83)),
      Seq(Seq(5), Seq(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36)),
      Seq(Seq(6), Seq(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11))
    )
    testInput.map(DayFour.parseLine) should equal(expectedResults)
  }

  test("wins") {
    DayFour.wins(Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)) shouldBe 4
    DayFour.wins(Seq(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19)) shouldBe 2
    DayFour.wins(Seq(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1)) shouldBe 2
    DayFour.wins(Seq(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83)) shouldBe 1
    DayFour.wins(Seq(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36)) shouldBe 0
    DayFour.wins(Seq(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11)) shouldBe 0
  }

  test("score") {
    DayFour.score(Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)) shouldBe 8.0
    DayFour.score(Seq(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19)) shouldBe 2.0
    DayFour.score(Seq(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1)) shouldBe 2.0
    DayFour.score(Seq(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83)) shouldBe 1.0
    DayFour.score(Seq(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36)) shouldBe 0.0
    DayFour.score(Seq(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11)) shouldBe 0.0
  }

  test("full score") {
    val parsedLines: Seq[Seq[Seq[Int]]] = Seq(
      Seq(Seq(1), Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)),
      Seq(Seq(2), Seq(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19)),
      Seq(Seq(3), Seq(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1)),
      Seq(Seq(4), Seq(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83)),
      Seq(Seq(5), Seq(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36)),
      Seq(Seq(6), Seq(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11))
    )
    val total = parsedLines.map(
      lineSeq => {
        DayFour.score(lineSeq(1), lineSeq(2))
      }
    ).sum
    total shouldBe 13

  }

  test ("addCards") {
    DayFour.addCards(
      Seq(Seq(1), Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)),
      Seq.fill(6){1}
    ) shouldBe Seq(1, 2, 2, 2, 2, 1)
    DayFour.addCards(
      Seq(Seq(2), Seq(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19)),
      Seq(1, 2, 2, 2, 2, 1)
    ) shouldBe Seq(1, 2, 4, 4, 2, 1)
    DayFour.addCards(
      Seq(Seq(3), Seq(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1)),
      Seq(1, 2, 4, 4, 2, 1)
    ) shouldBe Seq(1, 2, 4, 8, 6, 1)
    DayFour.addCards(
      Seq(Seq(4), Seq(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83)),
      Seq(1, 2, 4, 8, 6, 1)
    ) shouldBe Seq(1, 2, 4, 8, 14, 1)
    DayFour.addCards(
      Seq(Seq(5), Seq(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36)),
      Seq(1, 2, 4, 8, 14, 1)
    ) shouldBe Seq(1, 2, 4, 8, 14, 1)
    DayFour.addCards(
      Seq(Seq(6), Seq(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11)),
      Seq(1, 2, 4, 8, 14, 1)
    ) shouldBe Seq(1, 2, 4, 8, 14, 1)
  }

  test("countCards") {
    val cards: Seq[Seq[Seq[Int]]] = Seq(
      Seq(Seq(1), Seq(41, 48, 83, 86, 17), Seq(83, 86, 6, 31, 17, 9, 48, 53)),
      Seq(Seq(2), Seq(13, 32, 20, 16, 61), Seq(61, 30, 68, 82, 17, 32, 24, 19)),
      Seq(Seq(3), Seq(1, 21, 53, 59, 44), Seq(69, 82, 63, 72, 16, 21, 14, 1)),
      Seq(Seq(4), Seq(41, 92, 73, 84, 69), Seq(59, 84, 76, 51, 58, 5, 54, 83)),
      Seq(Seq(5), Seq(87, 83, 26, 28, 32), Seq(88, 30, 70, 12, 93, 22, 82, 36)),
      Seq(Seq(6), Seq(31, 18, 13, 56, 72), Seq(74, 77, 10, 23, 35, 67, 36, 11))
    )
    DayFour.countCards(cards, Seq.fill(cards.length){1}) shouldBe 30
  }

}
