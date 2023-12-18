package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayFourteenTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "O....#....", // 10
    "O.OO#....#", //  9
    ".....##...", //  8
    "OO.#O....O", //  7
    ".O.....O#.", //  6
    "O.#..O.#.#", //  5
    "..O..#O..O", //  4
    ".......O..", //  3
    "#....###..", //  2
    "#OO..#...."  //  1
  )

  val testColumns: Seq[String] = Seq(
    "OO.O.O..##",
    "...OO....O",
    ".O...#O..O",
    ".O.#......",
    ".#.O......",
    "#.#..O#.##",
    "..#...O.#.",
    "....O#.O#.",
    "....#.....",
    ".#.O.#O..."
  )

  val rolledColumns: Seq[String] = Seq(
    "OOOO....##",
    "OOO.......",
    "O....#OO..",
    "O..#......",
    ".#O.......",
    "#.#O..#.##",
    "..#O....#.",
    "O....#O.#.",
    "....#.....",
    ".#O..#O..."
  )

  // First test
  test("getColumns") {
    DayFourteen.getColumns(testInput) shouldBe testColumns
  }

  test("columnLoad") {
    testColumns.map(DayFourteen.columnLoad) shouldBe Seq(31, 14, 14, 9, 7, 5, 4, 9, 0, 11)
  }

  test("roll") {
    testColumns.map(DayFourteen.roll) shouldBe rolledColumns
  }

}
