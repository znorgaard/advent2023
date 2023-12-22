package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DaySixteenTests extends AnyFunSuite {
  val testLines: Seq[String] = Seq(
    ".|...\\....",
    "|.-.\\.....",
    ".....|-...",
    "........|.",
    "..........",
    ".........\\",
    "..../.\\\\..",
    ".-.-/..|..",
    ".|....-|.\\",
    "..//.|...."
  )
  val testMatrix: Seq[Seq[String]] = Seq(
    Seq(".", "|", ".", ".", ".", "\\", ".", ".", ".", "."),
    Seq("|", ".", "-", ".", "\\", ".", ".", ".", ".", "."),
    Seq(".", ".", ".", ".", ".", "|", "-", ".", ".", "."),
    Seq(".", ".", ".", ".", ".", ".", ".", ".", "|", "."),
    Seq(".", ".", ".", ".", ".", ".", ".", ".", ".", "."),
    Seq(".", ".", ".", ".", ".", ".", ".", ".", ".", "\\"),
    Seq(".", ".", ".", ".", "/", ".", "\\", "\\", ".", "."),
    Seq(".", "-", ".", "-", "/", ".", ".", "|", ".", "."),
    Seq(".", "|", ".", ".", ".", ".", "-", "|", ".", "\\"),
    Seq(".", ".", "/", "/", ".", "|", ".", ".", ".", "."),
  )

  // First test
  test("reflect") {
    DaySixteen.reflect(".", 0) shouldBe Seq(2)
    DaySixteen.reflect("|", 0) shouldBe Seq(1, 3)
    DaySixteen.reflect("\\", 0) shouldBe Seq(3)
  }

  test("getMatrix") {
    DaySixteen.getMatrix(testLines.iterator) shouldBe testMatrix
  }

  test("nextPositions") {
    DaySixteen.nextPositions((0,0), Seq(0, 1, 2, 3)) shouldBe Map(
      2 -> (0, -1), 3 -> (-1, 0), 0 -> (0, 1), 1 -> (1, 0)
    )
  }


  test("countCharged") {
    DaySixteen.countCharged(testMatrix) shouldBe 46
  }

  test("maxCharged") {
    DaySixteen.maxCharged(testMatrix) shouldBe 51
  }

}
