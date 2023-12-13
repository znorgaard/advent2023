package advent.days

import scala.collection.mutable
import scala.io.{BufferedSource, Source}

// Adapted from a python tutorial because these were not concepts I was familiar with
// https://www.reddit.com/r/adventofcode/comments/18hbbxe/2023_day_12python_stepbystep_tutorial_with_bonus/

object DayTwelve {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayTwelveInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val parsedLines: Iterator[(String, Seq[Int])] = lines.map(partTwoLines)
        println(parsedLines.map((pattern, groupCounts) => combinations(pattern, groupCounts)).sum)

    def partOne(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val parsedLines: Iterator[(String, Seq[Int])] = lines.map(parseLine)
        println(parsedLines.map((pattern, groupCounts) => combinations(pattern, groupCounts)).sum)

    def combinations(pattern: String, groupCounts: Seq[Int]): BigInt = {
        def pound(pattern: String, groupCounts: Seq[Int]): BigInt = {
            val nextGroupSize: Int = groupCounts.head
            if (nextGroupSize > pattern.length) { // not enough pattern left to make the group
                0
            } else {
                val potentialGroup: String = pattern.take(nextGroupSize)
                val allDamaged: Boolean = potentialGroup.forall(s => Seq("#", "?").contains(s.toString))
                if (!allDamaged) {
                    0
                } else if (allDamaged & nextGroupSize == pattern.length & groupCounts.length == 1) {
                    1 // This is the end of the pattern and the last group and they match!
                } else if (nextGroupSize == pattern.length) {
                    0 // This is the end of the pattern but there are more groups to consider
                } else {
                    // This is not the end of the pattern
                    val nextCharacter: String = pattern(nextGroupSize).toString
                    if (allDamaged & nextCharacter == "#") {
                        0 // The nextCharacter would need to be included in the group, so this doesn't work
                    } else {
                        // The nextCharacter is "." or "?" but must be treated as "."
                        // Skip ahead in the pattern and groups
                        f((pattern.drop(nextGroupSize + 1), groupCounts.tail))
                    }
                }
            }
        }

        lazy val f: ((String, Seq[Int])) => BigInt = memoize {
            case (pattern, groupCounts) if !pattern.contains("#") & groupCounts.isEmpty => 1
            case (pattern, groupCounts) if pattern.isEmpty | groupCounts.isEmpty => 0
            case (pattern, groupCounts) =>
                val firstCharacter: String = pattern.head.toString
                val firstGroup: Int = groupCounts.head
                if (firstCharacter == "#") {
                    pound(pattern, groupCounts)
                } else if (firstCharacter == ".") {
                    // No group consumed, move to next position
                    f((pattern.tail, groupCounts))
                } else { //? could be either, take both paths
                    pound(pattern, groupCounts) + f((pattern.tail, groupCounts))
                }
        }
        f((pattern, groupCounts))
    }

    def parseLine(line: String): (String, Seq[Int]) =
        val splitLine: Seq[String] = line.split(" ")
        (
          splitLine.head,
          splitLine.last.split(",").map(_.toInt)
        )

    def partTwoLines(line: String): (String, Seq[Int]) =
        val splitLine = parseLine(line)
        (
          "\\?$".r.replaceFirstIn((splitLine._1 + "?") * 5, ""),
          Seq.fill(5)(splitLine._2).flatten
        )

}

// From this very helpful stack overflow:
//   https://stackoverflow.com/questions/16257378/is-there-a-generic-way-to-memoize-in-scala
def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
    override def apply(key: I): O = self.synchronized(getOrElseUpdate(key, f(key)))
}