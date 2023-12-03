package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayTwoTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green",
    "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue",
    "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red",
    "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red",
    "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  )

  // First test
  test("maxDraws") {
    val expectedMaxes: Seq[(Int, Map[String, Int])] = Seq(
      (1 -> Map("blue" ->  6, "red" ->  4, "green" ->  2)),
      (2 -> Map("blue" ->  4, "red" ->  1, "green" ->  3)),
      (3 -> Map("blue" ->  6, "red" -> 20, "green" -> 13)),
      (4 -> Map("blue" -> 15, "red" -> 14, "green" ->  3)),
      (5 -> Map("blue" ->  2, "red" ->  6, "green" ->  3))
    )
    testInput.map(line => DayTwo.maxDraws(line)) should equal(expectedMaxes)
  }

  val bagContents: Map[String, Int] = Map("red" -> 12, "green" -> 13, "blue" -> 14)
  test("possible") {
    DayTwo.possible(bagContents, Map("blue" ->  6, "red" ->  4, "green" ->  2)) shouldBe true
    DayTwo.possible(bagContents, Map("blue" ->  4, "red" ->  1, "green" ->  3)) shouldBe true
    DayTwo.possible(bagContents, Map("blue" ->  6, "red" -> 20, "green" -> 13)) shouldBe false
    DayTwo.possible(bagContents, Map("blue" -> 15, "red" -> 14, "green" ->  3)) shouldBe false
    DayTwo.possible(bagContents, Map("blue" ->  2, "red" ->  6, "green" ->  3)) shouldBe true
  }

  test("sumGames") {
    DayTwo.sumGames(testInput.iterator, bagContents) shouldBe 8
  }
  test("gamePower") {
    DayTwo.gamePower(Map("blue" -> 6, "red" -> 4, "green" -> 2)) shouldBe 48
    DayTwo.gamePower(Map("blue" -> 4, "red" -> 1, "green" -> 3)) shouldBe 12
    DayTwo.gamePower(Map("blue" -> 6, "red" -> 20, "green" -> 13)) shouldBe 1560
    DayTwo.gamePower(Map("blue" -> 15, "red" -> 14, "green" -> 3)) shouldBe 630
    DayTwo.gamePower(Map("blue" -> 2, "red" -> 6, "green" -> 3)) shouldBe 36
  }

  test("sumPower") {
    DayTwo.sumPower(testInput.iterator) shouldBe 2286
  }

}
