package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayFiveTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "seeds: 79 14 55 13",
    "",
    "seed-to-soil map:",
    "50 98 2",
    "52 50 48",
    "",
    "soil-to-fertilizer map:",
    "0 15 37",
    "37 52 2",
    "39 0 15",
    "",
    "fertilizer-to-water map:",
    "49 53 8",
    "0 11 42",
    "42 0 7",
    "57 7 4",
    "",
    "water-to-light map:",
    "88 18 7",
    "18 25 70",
    "",
    "light-to-temperature map:",
    "45 77 23",
    "81 45 19",
    "68 64 13",
    "",
    "temperature-to-humidity map:",
    "0 69 1",
    "1 0 69",
    "",
    "humidity-to-location map:",
    "60 56 37",
    "56 93 4"
  )

  // First test
  test("partOne") {
    DayFive.partOne(testInput.iterator) shouldBe 35
  }

  test("partTwo") {
    DayFive.partTwo(testInput.iterator) shouldBe 46
  }

  test("mapOrElse") {
    DayFive.mapOrElse(2, Seq((1, 2, 3))) shouldBe 1
    DayFive.mapOrElse(3, Seq((1, 2, 3))) shouldBe 2
    DayFive.mapOrElse(3, Seq((1, 2, 3))) shouldBe 2
    DayFive.mapOrElse(10, Seq((1, 2, 3), (1, 9, 2))) shouldBe 2
    DayFive.mapOrElse(30, Seq((1, 2, 3))) shouldBe 30
  }

}
