package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayThreeTests extends AnyFunSuite {
  val testInput: Seq[String] = Seq(
    "467..114..",
    "...*......",
    "..35..633.",
    "......#...",
    "617*......",
    ".....+.58.",
    "..592.....",
    "......755.",
    "...$.*....",
    ".664.598.."
  )

  // First test
  test("numericPositions") {
    val expectedResults: Seq[Map[(Int, Int), Int]] = Seq(
      Map((0, 2) -> 467, (5, 7) -> 114),
      Map.empty,
      Map((2, 3) -> 35, (6, 8) -> 633),
      Map.empty,
      Map((0, 2) -> 617),
      Map((7, 8) -> 58),
      Map((2, 4) -> 592),
      Map((6, 8) -> 755),
      Map.empty,
      Map((1, 3) -> 664, (5, 7) -> 598)
    )
    testInput.map(DayThree.numericPositions) should equal(expectedResults)
  }

  test("symbolPositions") {
    val expectedResults: Seq[Seq[Int]] = Seq(
      Seq.empty,
      Seq(3),
      Seq.empty,
      Seq(6),
      Seq(3),
      Seq(5),
      Seq.empty,
      Seq.empty,
      Seq(3, 5),
      Seq.empty
    )
    testInput.map(DayThree.symbolPositions) should equal(expectedResults)
  }

  test("textToCoordinates") {
    val expectedResults: Map[Int, (Map[(Int, Int), Int], Seq[Int])] = Map(
      0 -> (Map((0, 2) -> 467, (5, 7) -> 114), Seq.empty),
      1 -> (Map.empty, Seq(3)),
      2 -> (Map((2, 3) -> 35, (6, 8) -> 633), Seq.empty),
      3 -> (Map.empty, Seq(6)),
      4 -> (Map((0, 2) -> 617), Seq(3)),
      5 -> (Map((7, 8) -> 58), Seq(5)),
      6 -> (Map((2, 4) -> 592), Seq.empty),
      7 -> (Map((6, 8) -> 755), Seq.empty),
      8 -> (Map.empty, Seq(3, 5)),
      9 -> (Map((1, 3) -> 664, (5, 7) -> 598), Seq.empty)
    )
    DayThree.textToCoordinates(testInput.iterator) should equal(expectedResults)
  }

  test("symbolPosFromRows") {
    val coordinates = DayThree.textToCoordinates(testInput.iterator)
    DayThree.symbolPosFromRows(coordinates, Seq(0, 1, 2)) should equal(Seq(3))
    DayThree.symbolPosFromRows(coordinates, Seq(-1, 0, 1)) should equal(Seq(3))

  }

  test("numericFilter") {
    val coordinates = DayThree.textToCoordinates(testInput.iterator)
    DayThree.numericFilter(coordinates) should contain theSameElementsAs Seq(467, 35, 633, 617, 592, 755, 664, 598)
    DayThree.numericFilter(coordinates).sum shouldBe 4361

    val dupInput = Seq(
      "..1..1..",
      "..*..?.."
    )
    DayThree.numericFilter(DayThree.textToCoordinates(dupInput.iterator)).sum shouldBe 2

  }

  test("textToGearCoordinates") {
    val expectedResults: Map[Int, (Map[(Int, Int), Int], Seq[Int])] = Map(
      0 -> (Map((0, 2) -> 467, (5, 7) -> 114), Seq.empty),
      1 -> (Map.empty, Seq(3)),
      2 -> (Map((2, 3) -> 35, (6, 8) -> 633), Seq.empty),
      3 -> (Map.empty, Seq.empty),
      4 -> (Map((0, 2) -> 617), Seq(3)),
      5 -> (Map((7, 8) -> 58), Seq.empty),
      6 -> (Map((2, 4) -> 592), Seq.empty),
      7 -> (Map((6, 8) -> 755), Seq.empty),
      8 -> (Map.empty, Seq(5)),
      9 -> (Map((1, 3) -> 664, (5, 7) -> 598), Seq.empty)
    )
    
    DayThree.textToGearCoordinates(testInput.iterator) should equal(expectedResults)

  }

  test("gearFilterAndId") {
    DayThree.gearFilterAndId(DayThree.textToGearCoordinates(testInput.iterator)).sum shouldBe 467835
  }



}
