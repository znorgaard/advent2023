package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayOneTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq("1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet")

  test("getDigits") {
    val result: Seq[Seq[String]] = testInput.map(line => DayOne.getDigits(line))
    result should equal(Seq(Seq("1", "2"), Seq("3", "8"), Seq("1", "2", "3", "4", "5"), Seq("7")))
  }

  test("doubleDigitInt") {
    val result: Seq[Int] = testInput.map(line => DayOne.getDigits(line))
      .map(digits => DayOne.doubleDigitInt(digits))
    result should equal(Seq(12, 38, 15, 77))
  }

  val test2Input: Seq[String] = Seq(
    "two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"
  )

  test("getDigitIndices") {
    val result: Seq[Map[String, Seq[Int]]] = test2Input.map(line => DayOne.getDigitIndices(line))
    val expectedResults: Seq[Map[String, Seq[Int]]] = Seq(
      Map( // two1nine
        "1" -> Seq(3),
        "2" -> Seq(0),
        "3" -> Seq(),
        "4" -> Seq(),
        "5" -> Seq(),
        "6" -> Seq(),
        "7" -> Seq(),
        "8" -> Seq(),
        "9" -> Seq(4)
      ),
      Map(
        // eightwothree
        "1" -> Seq(),
        "2" -> Seq(4),
        "3" -> Seq(7),
        "4" -> Seq(),
        "5" -> Seq(),
        "6" -> Seq(),
        "7" -> Seq(),
        "8" -> Seq(0),
        "9" -> Seq()
      ),
      Map(
        // abcone2threexyz
        "1" -> Seq(3),
        "2" -> Seq(6),
        "3" -> Seq(7),
        "4" -> Seq(),
        "5" -> Seq(),
        "6" -> Seq(),
        "7" -> Seq(),
        "8" -> Seq(),
        "9" -> Seq()
      ),
      Map(
        // xtwone3four
        "1" -> Seq(3),
        "2" -> Seq(1),
        "3" -> Seq(6),
        "4" -> Seq(7),
        "5" -> Seq(),
        "6" -> Seq(),
        "7" -> Seq(),
        "8" -> Seq(),
        "9" -> Seq()
      ),
      Map(
        // 4nineeightseven2
        "1" -> Seq(),
        "2" -> Seq(15),
        "3" -> Seq(),
        "4" -> Seq(0),
        "5" -> Seq(),
        "6" -> Seq(),
        "7" -> Seq(10),
        "8" -> Seq(5),
        "9" -> Seq(1)
      ),
      Map(
        // zoneight234
        "1" -> Seq(1),
        "2" -> Seq(8),
        "3" -> Seq(9),
        "4" -> Seq(10),
        "5" -> Seq(),
        "6" -> Seq(),
        "7" -> Seq(),
        "8" -> Seq(3),
        "9" -> Seq()
      ),
      Map(
        // 7pqrstsixteen
        "1" -> Seq(),
        "2" -> Seq(),
        "3" -> Seq(),
        "4" -> Seq(),
        "5" -> Seq(),
        "6" -> Seq(6),
        "7" -> Seq(0),
        "8" -> Seq(),
        "9" -> Seq()
      )
    )
    result.indices.foreach(
      n => {
        result(n) should contain theSameElementsAs expectedResults(n)
      }
    )
  }

  test("firstDigit") {
    DayOne.firstDigit(Map("1" -> Seq(), "2" -> Seq(0))) shouldBe "2"
    DayOne.firstDigit(Map("1" -> Seq(0), "2" -> Seq())) shouldBe "1"
  }

  test("lastDigit") {
    DayOne.lastDigit(Map("1" -> Seq(1), "2" -> Seq(0))) shouldBe "1"
    DayOne.lastDigit(Map("1" -> Seq(0), "2" -> Seq(1))) shouldBe "2"
  }

  test("doubleDigitIntFromLine") {
    //val test2Input: Seq[String] = Seq(
    //    "two1nine", "eightwothree", "abcone2threexyz", "xtwone3four", "4nineeightseven2", "zoneight234", "7pqrstsixteen"
    //  )
    val result: Seq[Int] = test2Input.map(line => DayOne.doubleDigitIntFromLine(line))
    result should equal(Seq(29, 83, 13, 24, 42, 14, 76))

  }

}
