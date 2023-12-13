package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayTwelveTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "???.### 1,1,3",
    ".??..??...?##. 1,1,3",
    "?#?#?#?#?#?#?#? 1,3,1,6",
    "????.#...#... 4,1,1",
    "????.######..#####. 1,6,5",
    "?###???????? 3,2,1"
  )

  // First test
  test("parseLines") {
    testInput.iterator.map(DayTwelve.parseLine).toSeq shouldBe Seq(
      ("???.###", Seq(1,1,3)),
      (".??..??...?##.", Seq(1,1,3)),
      ("?#?#?#?#?#?#?#?", Seq(1,3,1,6)),
      ("????.#...#...", Seq(4,1,1)),
      ("????.######..#####.", Seq(1,6,5)),
      ("?###????????", Seq(3,2,1))
    )
  }

  test("combinations") {
    DayTwelve.combinations("", Seq(1,1,3)) shouldBe 0
    DayTwelve.combinations("...", Seq.empty) shouldBe 1
    DayTwelve.combinations("..#.", Seq.empty) shouldBe 0
    DayTwelve.combinations("#", Seq(1)) shouldBe 1
    DayTwelve.combinations("#....", Seq(1,1,3)) shouldBe 0
    DayTwelve.combinations("....", Seq(1,1,3)) shouldBe 0

    DayTwelve.combinations("???.###", Seq(1,1,3)) shouldBe 1
    DayTwelve.combinations(".??..??...?##.", Seq(1,1,3)) shouldBe 4
    DayTwelve.combinations("?#?#?#?#?#?#?#?", Seq(1,3,1,6)) shouldBe 1
    DayTwelve.combinations("????.#...#...", Seq(4,1,1)) shouldBe 1
    DayTwelve.combinations("????.######..#####.", Seq(1,6,5)) shouldBe 4
    DayTwelve.combinations("?###????????", Seq(3,2,1)) shouldBe 10
  }

  test("partTwoLines") {
    DayTwelve.partTwoLines(".# 1") shouldBe (".#?.#?.#?.#?.#", Seq(1, 1, 1, 1, 1))
  }

}
