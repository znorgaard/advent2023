package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DaySixTests extends AnyFunSuite {

  val testInput: Seq[String] = Seq(
    "Time:      7  15   30",
    "Distance:  9  40  200"
  )

  // First test
  test("readLines") {
    DaySix.readLines(testInput.iterator) should equal(Seq((7, 9), (15, 40), (30, 200)))
  }

  test("distances") {
    DaySix.distances(7) should equal(Seq(0, 6, 10, 12, 12, 10, 6, 0))
  }

  test("winOptions") {
    DaySix.winOptions(7, 9) shouldBe 4
    DaySix.winOptions(15, 40) shouldBe 8
    DaySix.winOptions(30, 200) shouldBe 9
    DaySix.winOptions(71530, 940200) shouldBe 71503
  }


}
