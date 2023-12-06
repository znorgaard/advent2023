package advent.days

import scala.io.{BufferedSource, Source}

object DaySix {
    // Could be improved by:
    //   find the first charge time that beats the required distance then
    //      winOptions = time - (firstWinningTime * 2) + 1
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DaySixInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val raceConditions: Seq[(Int, Int)] = readLines(lines)
        val wins: Seq[Int] = raceConditions.map(race => winOptions(race.head, race.last))
        val total: Int = wins.product
        println(total)
        println(longOptions(46857582, "208141212571410".toLong))

    def readLines(lines: Iterator[String]): Seq[(Int, Int)] =
        val times = "Time:\\s+".r.replaceAllIn(lines.next, "").split("\\s+").map(_.toInt)
        val distances = "Distance:\\s+".r.replaceAllIn(lines.next, "").split("\\s+").map(_.toInt)
        times.zip(distances)

    def distances(time: Int): Seq[Int] =
        (0 to time).map(
            chargeTime => {
                chargeTime * (time - chargeTime)
            }
        )

    def winOptions(time: Int, distance: Int): Int =
        distances(time).count(_ > distance)

    def longDistances(time: Long): Seq[Long] =
        (0L to time).map(
            chargeTime => {
                chargeTime * (time - chargeTime)
            }
        )
    def longOptions(time: Long, distance: Long): Int =
        longDistances(time).count(_ > distance)


}
