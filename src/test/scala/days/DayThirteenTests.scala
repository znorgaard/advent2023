package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayThirteenTests extends AnyFunSuite {

  val testTextInput: Seq[String] = Seq(
    "#.##..##.",
    "..#.##.#.",
    "##......#",
    "##......#",
    "..#.##.#.",
    "..##..##.",
    "#.#.##.#.",
    "",
    "#...##..#",
    "#....#..#",
    "..##..###",
    "#####.##.",
    "#####.##.",
    "..##..###",
    "#....#..#"
  )
  val testInput: Seq[Seq[String]] = Seq(
    Seq(
      "#.##..##.",
      "..#.##.#.",
      "##......#",
      "##......#",
      "..#.##.#.",
      "..##..##.",
      "#.#.##.#."
    ),
    Seq(
      "#...##..#",
      "#....#..#",
      "..##..###",
      "#####.##.",
      "#####.##.",
      "..##..###",
      "#....#..#"
    )
  )

  // First test
  test("getPatterns") {
    DayThirteen.getPatterns(testTextInput.iterator.buffered) shouldBe testInput
  }

  test("reflectionIndices") {
    DayThirteen.reflectionIndices("#.##..##.") shouldBe Seq(5, 7)
    DayThirteen.reflectionIndices("..#.##.#.") shouldBe Seq(1, 5)
    DayThirteen.reflectionIndices("##......#") shouldBe Seq(1, 5)
    DayThirteen.reflectionIndices("##......#") shouldBe Seq(1, 5)
    DayThirteen.reflectionIndices("..#.##.#.") shouldBe Seq(1, 5)
    DayThirteen.reflectionIndices("..##..##.") shouldBe Seq(1, 3, 5, 7)
    DayThirteen.reflectionIndices("#.#.##.#.") shouldBe Seq(5)
  }

  test("findReflections") {
    DayThirteen.findReflections(testInput.head) shouldBe Some(5)
    DayThirteen.findReflections(testInput.last) shouldBe None
  }

  test("transformPattern") {
    DayThirteen.transformPattern(testInput.head) shouldBe Seq(
      "#.##..#",
      "..##...",
      "##..###",
      "#....#.",
      ".#..#.#",
      ".#..#.#",
      "#....#.",
      "##..###",
      "..##..."
    )
  }

  test("patternValue") {
    testInput.map(DayThirteen.patternValue) shouldBe Seq(5, 400)
  }

  test("getCandidateReflections") {
    DayThirteen.getCandidateReflections(
      Seq(Seq(1,2), Seq(1,3), Seq(1,4), Seq(1,5), Seq(1,6)),
      Some(1)
    ) shouldBe Seq.empty

    DayThirteen.getCandidateReflections(
      Seq(Seq(1, 2), Seq(1, 2), Seq(1, 2), Seq(1, 2), Seq(1, 6)),
      Some(1)
    ) shouldBe Seq(2)
    DayThirteen.getCandidateReflections(
      Seq(Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2, 3), Seq(1, 2), Seq(1, 6, 3)),
      Some(1)
    ) shouldBe Seq(2, 3)
  }

  test("rowsToFix") {
    DayThirteen.rowsToFix(
      reflections = Seq(Seq(1, 2), Seq(2, 3), Seq(2)),
      candidateReflections = Seq.empty
    ) shouldBe Seq.empty
    DayThirteen.rowsToFix(
      reflections = Seq(Seq(1, 2), Seq(2, 3), Seq(2, 4)),
      candidateReflections = Seq.empty
    ) shouldBe Seq.empty
    DayThirteen.rowsToFix(
      reflections = Seq(Seq(1), Seq(1, 3), Seq(3)),
      candidateReflections = Seq(3)
    ) shouldBe Seq(0)
    DayThirteen.rowsToFix(
      reflections = Seq(Seq(1, 3), Seq(2, 3), Seq(2, 3), Seq(1, 3), Seq(1)),
      candidateReflections = Seq(3)
    ) shouldBe Seq(4)
  }

  test("alternateReflectionIndices") {
    DayThirteen.alternateReflectionIndices("#.##..##.") shouldBe Seq(
      Seq(1, 3, 5, 7), // ..##..##.
      Seq(1, 2, 7),    // ####..##.
      Seq(2, 7),       // #..#..##.
      Seq(7),          // #.#...##.
      Seq(7),          // #.###.##.
      Seq(3),          // #.##.###.
      Seq.empty,       // #.##...#.
      Seq(8),          // #.##..#..
      Seq(8),          // #.##..###
    )
  }

  test("newReflection") {
    DayThirteen.newReflection(testInput.head) shouldBe None
    DayThirteen.newReflection(testInput.last) shouldBe None

    DayThirteen.newReflection(DayThirteen.transformPattern(testInput.head)) shouldBe Some(3)
    DayThirteen.newReflection(DayThirteen.transformPattern(testInput.last)) shouldBe Some(1)
  }

  test("newPatternValue") {
    testInput.map(DayThirteen.newPatternValue) shouldBe Seq(300, 100)
  }

}
