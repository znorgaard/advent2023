package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._

class DayFifteenTests extends AnyFunSuite {
  val testInput: String = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7"
  val testFileLine: Seq[String] = Seq(testInput)

  // First test
  test("getInput") {
    DayFifteen.getInput(Seq("HASH").iterator) shouldBe Seq(
      Seq(72, 65, 83, 72)
    )
    DayFifteen.getInput(testFileLine.iterator) shouldBe Seq(
      Seq(114, 110, 61, 49),
      Seq(99, 109, 45),
      Seq(113, 112, 61, 51),
      Seq(99, 109, 61, 50),
      Seq(113, 112, 45),
      Seq(112, 99, 61, 52),
      Seq(111, 116, 61, 57),
      Seq(97, 98, 61, 53),
      Seq(112, 99, 45),
      Seq(112, 99, 61, 54),
      Seq(111, 116, 61, 55)
    )
  }

  test("hash") {
    DayFifteen.hash(Seq(72, 65, 83, 72)) shouldBe 52
    DayFifteen.getInput(testFileLine.iterator).map(asciiSeq => DayFifteen.hash(asciiSeq)) shouldBe Seq(
      30, 253, 97, 47, 14, 180, 9, 197, 48, 214, 231
    )
    DayFifteen.getInput(testFileLine.iterator).map(asciiSeq => DayFifteen.hash(asciiSeq)).sum shouldBe 1320
  }

  test("hashLabel") {
    DayFifteen.hashLabel("HASH") shouldBe 52
  }

  test("parseStep") {
    DayFifteen.parseStep("rn=1") shouldBe ("rn", "=", Some(1))
    DayFifteen.parseStep("cm-") shouldBe ("cm", "-", None)
  }

  test("getInstructions") {
    DayFifteen.getInstructions("rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7") shouldBe Seq(
      "rn=1", "cm-", "qp=3", "cm=2", "qp-", "pc=4", "ot=9", "ab=5", "pc-", "pc=6", "ot=7"
    )
  }

  test("boxPower") {
    DayFifteen.boxPower(0, Seq(("rn", 1), ("cm", 2))) shouldBe 5
    DayFifteen.boxPower(3, Seq(("ot", 7), ("ab", 5), ("pc", 6))) shouldBe 140
  }

  test("foldSteps") {
    val steps = Seq("rn=1", "cm-", "qp=3", "cm=2", "qp-", "pc=4", "ot=9", "ab=5", "pc-", "pc=6", "ot=7")
    DayFifteen.foldSteps(steps) shouldBe 145
  }

  test("updateBoxes") {
    DayFifteen.updateBoxes(Map(0-> Seq.empty), "rn=1") shouldBe Map(0 -> Seq(("rn", 1)))
    DayFifteen.updateBoxes(
      boxMap = Map(0 -> Seq(("rn", 1))),
      step   = "cm-"
    ) shouldBe Map(0 -> Seq(("rn", 1)))
    DayFifteen.updateBoxes(
      boxMap = Map(0 -> Seq(("rn", 1)), 1 -> Seq.empty),
      step = "qp=3"
    ) shouldBe Map(0 -> Seq(("rn", 1)), 1-> Seq(("qp", 3)))
    DayFifteen.updateBoxes(
      boxMap = Map(0 -> Seq(("rn", 1)), 1 -> Seq(("qp", 3))),
      step = "cm=2"
    ) shouldBe Map(0 -> Seq(("rn", 1), ("cm", 2)), 1 -> Seq(("qp", 3)))

  }

}
