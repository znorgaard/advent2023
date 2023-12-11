package advent.days

import scala.io.{BufferedSource, Source}

object DayNine {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayNineInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val dataSets: Iterator[Seq[Int]] = lines.map(readLine)
        val total: Int = dataSets.map(firstValue).sum
        println(total)

    def readLine(line: String): Seq[Int] =
        line.split(" ").map(_.toInt)

    def differences(intSeq: Seq[Int]): Seq[Int] =
        // Unclear if it's absolute difference or not in the puzzle
        (1 until intSeq.length).map(i => intSeq(i) - intSeq(i - 1))

    def countDown(intSeq: Seq[Int]): Seq[Seq[Int]] =
        val nextDifferences = differences(intSeq)
        if (nextDifferences.head == 0 & nextDifferences.forall(_ == nextDifferences.head)) {
            Seq(intSeq, nextDifferences)
        } else {
            Seq(intSeq) ++ countDown(nextDifferences)
        }
    def countUp(countDownVals: Seq[Seq[Int]]): Int =
        countDownVals.map(_.last).sum
    def nextValue(intSeq: Seq[Int]): Int =
        countUp(countDown(intSeq))

    def firstValue(intSeq: Seq[Int]): Int =
        val firstValues: Seq[Int] = countDown(intSeq).map(_.head)
        firstValues.foldRight(0)((a,b) => a - b)

}
