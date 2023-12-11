package advent.days

import scala.io.{BufferedSource, Source}

object DayEight {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayEightInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val instructionsAndMap = parseLines(lines)
        val steps = walk(instructionsAndMap._1, instructionsAndMap._2, "AAA")
        println(steps)
        val starts = ghostStarts(instructionsAndMap._2)
        val spookySteps = ghostSteps(instructionsAndMap._1, instructionsAndMap._2, starts)
        println(spookySteps)

    def parseInstructions(instructions: String): Seq[Int] =
        instructions.split("").map { case "L" => 0; case "R" => 1 }

    def parseLines(lines: Iterator[String]): (Seq[Int], LocationMap) =
        val instructions = parseInstructions(lines.next)
        lines.next // Skip spacer
        val posMap: Map[String, Seq[String]] = lines.map(
            line => {
                val lineElements = "[()]".r.replaceAllIn(line, "").split(" = |, ")
                lineElements.head -> lineElements.tail.toSeq
            }
        ).toMap
        (instructions, posMap)

    def walk(instructions: Seq[Int], locations: LocationMap, currentLocation: String): Int =
        val nextLocation = locations(currentLocation)(instructions.head)
        if (nextLocation == "ZZZ") {1} else {
            1 + walk(instructions.tail :+ instructions.head, locations, nextLocation)
        }

    def ghostStarts(locations: LocationMap): Seq[String] =
        locations.view.filterKeys(_.endsWith("A")).keys.toSeq

    def gcd(a: BigInt, b: BigInt): BigInt =
        if (a == 0) b else gcd(b % a, a)

    def lcm(a: BigInt, b: BigInt): BigInt =
        (a / gcd(a, b)) * b

    def lcmSeq(ints: Seq[BigInt]): BigInt =
        ints.foldLeft(BigInt(1))(lcm)

    def ghostWalk(instructions: Seq[Int], locations: LocationMap, currentLocation: String): Int =
        val nextLocation = locations(currentLocation)(instructions.head)
        if (nextLocation.endsWith("Z")) {
            1
        } else {
            1 + ghostWalk(instructions.tail :+ instructions.head, locations, nextLocation)
        }

    def ghostSteps(instructions: Seq[Int], locations: LocationMap, starts: Seq[String]): BigInt =
        val stops: Seq[BigInt] = starts.map(start => ghostWalk(instructions, locations, start)).map(BigInt(_))
        lcmSeq(stops)
}

type LocationMap = Map[String, Seq[String]]