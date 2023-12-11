package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayNineTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "0 3 6 9 12 15",
    "1 3 6 10 15 21",
    "10 13 16 21 30 45"
  )
  val splitLines: Seq[Seq[Int]] = Seq(
    Seq(0, 3, 6, 9, 12, 15),
    Seq(1, 3, 6, 10, 15, 21),
    Seq(10, 13, 16, 21, 30, 45)
  )

  // First test
  test("readLine") {
    testInput.map(DayNine.readLine) should equal(splitLines)
  }
  test("differences") {
    DayNine.differences(Seq(0, 3, 6, 9, 12, 15)) shouldBe Seq(3, 3, 3, 3, 3)
    DayNine.differences(Seq(3, 3, 3, 3, 3)) shouldBe Seq(0, 0, 0, 0)

    DayNine.differences(Seq(1, 3, 6, 10, 15, 21)) shouldBe Seq(2, 3, 4, 5, 6)
    DayNine.differences(Seq(2, 3, 4, 5, 6)) shouldBe Seq(1, 1, 1, 1)
    DayNine.differences(Seq(1, 1, 1, 1)) shouldBe Seq(0, 0, 0)

    DayNine.differences(Seq(10, 13, 16, 21, 30, 45)) shouldBe Seq(3, 3, 5, 9, 15)
    DayNine.differences(Seq(3, 3, 5, 9, 15)) shouldBe Seq(0, 2, 4, 6)
    DayNine.differences(Seq(0, 2, 4, 6)) shouldBe Seq(2, 2, 2)
    DayNine.differences(Seq(2, 2, 2)) shouldBe Seq(0, 0)
  }

  test("countDown") {
    DayNine.countDown(Seq(0, 3, 6, 9, 12, 15)) shouldBe
      Seq(Seq(0, 3, 6, 9, 12, 15), Seq(3, 3, 3, 3, 3), Seq(0, 0, 0, 0))

    DayNine.countDown(Seq(1, 3, 6, 10, 15, 21)) shouldBe
      Seq(Seq(1, 3, 6, 10, 15, 21), Seq(2, 3, 4, 5, 6), Seq(1, 1, 1, 1), Seq(0, 0, 0))

    DayNine.countDown(Seq(10, 13, 16, 21, 30, 45)) shouldBe
      Seq(Seq(10, 13, 16, 21, 30, 45), Seq(3, 3, 5, 9, 15), Seq(0, 2, 4, 6), Seq(2, 2, 2), Seq(0, 0))
  }

  test("countUp") {
    DayNine.countUp(Seq(Seq(0, 3, 6, 9, 12, 15), Seq(3, 3, 3, 3, 3), Seq(0, 0, 0, 0))) shouldBe 18
    DayNine.countUp(Seq(Seq(1, 3, 6, 10, 15, 21), Seq(2, 3, 4, 5, 6), Seq(1, 1, 1, 1), Seq(0, 0, 0))) shouldBe 28
    DayNine.countUp(
      Seq(Seq(10, 13, 16, 21, 30, 45), Seq(3, 3, 5, 9, 15), Seq(0, 2, 4, 6), Seq(2, 2, 2), Seq(0, 0))
    ) shouldBe 68
  }

  test("nextValue") {
    splitLines.map(DayNine.nextValue) shouldBe Seq(18, 28, 68)
  }

  test("firstValue") {
    splitLines.map(DayNine.firstValue) shouldBe Seq(-3, 0, 5)
  }

}
