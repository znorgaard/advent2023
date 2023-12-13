package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayElevenTests extends AnyFunSuite {

  val testTextInput: Seq[String] = Seq(
    "...#......",
    ".......#..",
    "#.........",
    "..........",
    "......#...",
    ".#........",
    ".........#",
    "..........",
    ".......#..",
    "#...#....."
  )

  val testInput: Seq[Seq[String]] = Seq(
    "...#......".split("").toSeq,
    ".......#..".split("").toSeq,
    "#.........".split("").toSeq,
    "..........".split("").toSeq,
    "......#...".split("").toSeq,
    ".#........".split("").toSeq,
    ".........#".split("").toSeq,
    "..........".split("").toSeq,
    ".......#..".split("").toSeq,
    "#...#.....".split("").toSeq
  )

  val expandedInput: Seq[Seq[String]] = Seq(
    "....#........".split("").toSeq,
    ".........#...".split("").toSeq,
    "#............".split("").toSeq,
    ".............".split("").toSeq,
    ".............".split("").toSeq,
    "........#....".split("").toSeq,
    ".#...........".split("").toSeq,
    "............#".split("").toSeq,
    ".............".split("").toSeq,
    ".............".split("").toSeq,
    ".........#...".split("").toSeq,
    "#....#.......".split("").toSeq
  )

  test("getUniverse") {
    DayEleven.getUniverse(testTextInput) shouldBe testInput
  }
  test("expandRows") {
    val expectedOut: Seq[Seq[String]] = Seq(
      "...#......".split("").toSeq,
      ".......#..".split("").toSeq,
      "#.........".split("").toSeq,
      "..........".split("").toSeq,
      "..........".split("").toSeq,
      "......#...".split("").toSeq,
      ".#........".split("").toSeq,
      ".........#".split("").toSeq,
      "..........".split("").toSeq,
      "..........".split("").toSeq,
      ".......#..".split("").toSeq,
      "#...#.....".split("").toSeq
    )
    DayEleven.expandRows(testInput) shouldBe expectedOut
  }

  test("expandUniverse") {
    DayEleven.expand(testInput) shouldBe expandedInput
  }

  test("galaxyCoordinates") {
    val expectedOut: Seq[(BigInt, BigInt)] = Seq(
      (0, 4),
      (1, 9),
      (2, 0),
      (5, 8),
      (6, 1),
      (7, 12),
      (10, 9),
      (11, 0), (11, 5)
    )

    DayEleven.galaxyCoordinates(expandedInput) shouldBe expectedOut
  }

  test("distance") {
    DayEleven.distance((0, 4), (10, 9)) shouldBe 15
    DayEleven.distance((2, 0), (7, 12)) shouldBe 17
    DayEleven.distance((11, 0), (11, 5)) shouldBe 5
  }

  test("allDistances") {
    DayEleven.allDistances(expandedInput).sum shouldBe 374
  }

  test("emptyRows") {
    DayEleven.emptyRows(testInput) shouldBe Seq(3, 7)
  }

  test("emptyColumns") {
    DayEleven.emptyColumns(testInput) shouldBe Seq(2, 5, 8)
  }

  test("expandedCoordinates") {
    val galaxyCoordinates: Seq[(BigInt, BigInt)] = Seq(
      (0, 3),
      (1, 7),
      (2, 0),
      (4, 6),
      (5, 1),
      (6, 9),
      (8, 7),
      (9, 0), (9, 4)
    )
    DayEleven.galaxyCoordinates(testInput) shouldBe galaxyCoordinates

    DayEleven.expandCoordinates(
      coordinates = Seq((0,3)),
      expandedRows = Seq(3,7),
      expandedColumns = Seq(2, 5, 8),
      expansion = 10
    ) shouldBe Seq((0,12))

    DayEleven.expandCoordinates(
      coordinates = Seq((5, 5)),
      expandedRows = Seq(1, 2, 3),
      expandedColumns = Seq(1, 2),
      expansion = 10
    ) shouldBe Seq((32, 23))


    DayEleven.distanceFromCoordinates(
      DayEleven.expandCoordinates(
        coordinates = galaxyCoordinates,
        expandedRows = Seq(3, 7),
        expandedColumns = Seq(2, 5, 8),
        expansion = 10
      )
    ).sum shouldBe 1030

    DayEleven.distanceFromCoordinates(
      DayEleven.expandCoordinates(
        coordinates = galaxyCoordinates,
        expandedRows = Seq(3, 7),
        expandedColumns = Seq(2, 5, 8),
        expansion = 100
      )
    ).sum shouldBe 8410
  }

}
