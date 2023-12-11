package advent
package days

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers._
class DayEightTests extends AnyFunSuite {

  val testInput1: Seq[String] = Seq(
    "RL",
    "",
    "AAA = (BBB, CCC)",
    "BBB = (DDD, EEE)",
    "CCC = (ZZZ, GGG)",
    "DDD = (DDD, DDD)",
    "EEE = (EEE, EEE)",
    "GGG = (GGG, GGG)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  val parsed1: (Seq[Int], LocationMap) = (
    Seq(1, 0),
    Map(
      "AAA" -> Seq("BBB", "CCC"),
      "BBB" -> Seq("DDD", "EEE"),
      "CCC" -> Seq("ZZZ", "GGG"),
      "DDD" -> Seq("DDD", "DDD"),
      "EEE" -> Seq("EEE", "EEE"),
      "GGG" -> Seq("GGG", "GGG"),
      "ZZZ" -> Seq("ZZZ", "ZZZ")
    )
  )

  val testInput2: Seq[String] = Seq(
    "LLR",
    "",
    "AAA = (BBB, BBB)",
    "BBB = (AAA, ZZZ)",
    "ZZZ = (ZZZ, ZZZ)"
  )

  val parsed2: (Seq[Int], LocationMap) = (
    Seq(0, 0, 1),
    Map(
      "AAA" -> Seq("BBB", "BBB"),
      "BBB" -> Seq("AAA", "ZZZ"),
      "ZZZ" -> Seq("ZZZ", "ZZZ")
    )
  )

  val ghostInput: Seq[String] = Seq(
    "LR",
    "",
    "11A = (11B, XXX)",
    "11B = (XXX, 11Z)",
    "11Z = (11B, XXX)",
    "22A = (22B, XXX)",
    "22B = (22C, 22C)",
    "22C = (22Z, 22Z)",
    "22Z = (22B, 22B)",
    "XXX = (XXX, XXX)"
  )

  val ghostParsed: (Seq[Int], LocationMap) = (
    Seq(0, 1),
    Map(
      "11A" -> Seq("11B", "XXX"),
      "11B" -> Seq("XXX", "11Z"),
      "11Z" -> Seq("11B", "XXX"),
      "22A" -> Seq("22B", "XXX"),
      "22B" -> Seq("22C", "22C"),
      "22C" -> Seq("22Z", "22Z"),
      "22Z" -> Seq("22B", "22B"),
      "XXX" -> Seq("XXX", "XXX")
    )
  )

  // First test
  test("parseInstructions") {
    DayEight.parseInstructions("RL") shouldBe Seq(1, 0)
  }

  test( "parseLines") {
    DayEight.parseLines(testInput1.iterator) shouldBe parsed1
    DayEight.parseLines(testInput2.iterator) shouldBe parsed2
    DayEight.parseLines(ghostInput.iterator) shouldBe ghostParsed
  }

  test("walk") {
    DayEight.walk(parsed1._1, parsed1._2, "AAA") shouldBe 2
    DayEight.walk(parsed2._1, parsed2._2, "AAA") shouldBe 6
  }

  test("ghostStarts") {
    DayEight.ghostStarts(ghostParsed._2) should contain theSameElementsAs Seq("11A", "22A")
  }

  test("ghostWalk") {
    DayEight.ghostWalk(ghostParsed._1, ghostParsed._2, "11A") shouldBe 2
    DayEight.ghostWalk(ghostParsed._1, ghostParsed._2, "22A") shouldBe 3
  }

  test("ghostSteps") {
    DayEight.ghostSteps(ghostParsed._1, ghostParsed._2, Seq("11A", "22A")) shouldBe 6
  }

}
