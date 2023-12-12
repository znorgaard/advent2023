package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayTenTests extends AnyFunSuite {
  val simpleTextInput: Seq[String] = Seq(
    ".....",
    ".S-7.",
    ".|.|.",
    ".L-J.",
    "....."
  )
  val simplestInput: Seq[Seq[String]] = Seq(
    ".....".split("").toSeq,
    ".S-7.".split("").toSeq,
    ".|.|.".split("").toSeq,
    ".L-J.".split("").toSeq,
    ".....".split("").toSeq
  )

  val simpleInput: Seq[Seq[String]] = Seq(
    "-L|F7".split("").toSeq,
    "7S-7|".split("").toSeq,
    "L|7||".split("").toSeq,
    "-L-J|".split("").toSeq,
    "L|-JF".split("").toSeq
  )

  val mediumInput: Seq[Seq[String]] = Seq(
    "..F7.".split("").toSeq,
    ".FJ|.".split("").toSeq,
    "SJ.L7".split("").toSeq,
    "|F--J".split("").toSeq,
    "LJ...".split("").toSeq
  )

  val hardInput: Seq[Seq[String]] = Seq(
    "7-F7-".split("").toSeq,
    ".FJ|7".split("").toSeq,
    "SJLL7".split("").toSeq,
    "|F--J".split("").toSeq,
    "LJ.LJ".split("").toSeq
  )

  // First test
  test("getMatrix") {
    DayTen.getMatrix(simpleTextInput) shouldBe simplestInput
  }

  test("findAnimal") {
    DayTen.findAnimal(simplestInput) shouldBe (1,1)
    DayTen.findAnimal(simpleInput) shouldBe (1,1)
    DayTen.findAnimal(mediumInput) shouldBe (2,0)
    DayTen.findAnimal(hardInput) shouldBe (2,0)
  }

  test("checkConnection") {
    DayTen.checkConnection("S", 1) shouldBe true
    DayTen.checkConnection("|", 1) shouldBe true
    DayTen.checkConnection("|", 3) shouldBe true
    DayTen.checkConnection("|", 2) shouldBe false
    DayTen.checkConnection("L", 1) shouldBe true
    DayTen.checkConnection("L", 2) shouldBe true
  }

  test("nextPosition") {
    DayTen.nextPosition((0,0), "F", 3) shouldBe (0,1)
    DayTen.nextPosition((1,1), "|", 1) shouldBe (2,1)
    DayTen.nextPosition((1,1), "-", 0) shouldBe (1,2)
    DayTen.nextPosition((1,1), "-", 2) shouldBe (1,0)
    DayTen.nextPosition((1,1), "L", 1) shouldBe (1,2)
    DayTen.nextPosition((1,1), "J", 1) shouldBe (1,0)
    DayTen.nextPosition((1,1), "7", 0) shouldBe (2,1)
  }

  test("whereTo") {
    DayTen.whereTo("-", 0) shouldBe 2
  }

  test("getSymbol") {
    DayTen.getSymbol(Seq(Seq("S", "J"), Seq("7", "L")), (0,1)) shouldBe "J"
  }

  test("buildLoop") {
    val simpleLoop: Seq[(Int, Int)] = Seq(
      (1, 2),
      (1, 3),
      (2, 3),
      (3, 3),
      (3, 2),
      (3, 1),
      (2, 1),
      (1, 1)
    )
    DayTen.buildLoop(simplestInput, (1, 2), 0) shouldBe simpleLoop
    DayTen.buildLoop(simpleInput, (1, 2), 0) shouldBe simpleLoop
    DayTen.buildLoop(simplestInput, (0, 1), 0) shouldBe Seq.empty
    DayTen.buildLoop(simpleInput, (0, 1), 0) shouldBe Seq.empty

    val hardLoop: Seq[(Int, Int)] = Seq(
      (2, 1),
      (1, 1),
      (1, 2),
      (0, 2),
      (0, 3),
      (1, 3),
      (2, 3),
      (2, 4),
      (3, 4),
      (3, 3),
      (3, 2),
      (3, 1),
      (4, 1),
      (4, 0),
      (3, 0),
      (2, 0)
    )

    DayTen.buildLoop(mediumInput, (2, 1), 0) shouldBe hardLoop
    DayTen.buildLoop(hardInput, (2, 1), 0) shouldBe hardLoop
    DayTen.buildLoop(mediumInput, (1, 0), 3) shouldBe Seq.empty
    DayTen.buildLoop(hardInput, (1, 0), 3) shouldBe Seq.empty
  }

}
