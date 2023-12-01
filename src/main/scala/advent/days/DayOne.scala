package advent
package days

import scala.io.{BufferedSource, Source}
import scala.util.matching.Regex


object DayOne {
    def main(): Unit =
        // Part Two Solution
        val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayOneInput.txt")
        val total: Int = puzzleInput
          .getLines()
          .map(doubleDigitIntFromLine)
          .sum
        println(total)
        puzzleInput.close()

    def partOne(): Unit =
        val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayOneInput.txt")
        val total: Int = puzzleInput
          .getLines()
          .map(getDigits)
          .map(doubleDigitInt)
          .sum
        println(total)
        puzzleInput.close()

    def getDigits(line: String): Seq[String] =
        line.filter(_.isDigit).split("").toSeq

    def doubleDigitInt(digits: Seq[String]): Int =
        (digits.head + digits.last).toInt

    def getDigitIndices(line: String): Map[String, Seq[Int]] =
        digitsMap.map(
            digitPair => {
                val word: String = digitPair._1
                val digit: String = digitPair._2
                digit -> Regex(word + "|" + digit).findAllMatchIn(line).map(_.start).toSeq
            }
        )

    def firstDigit(digitIndices: Map[String, Seq[Int]]): String =
        val minIndex = digitIndices.values.flatten.min
        digitIndices.view.filterKeys(digitIndices(_) contains minIndex).keys.head

    def lastDigit(digitIndices: Map[String, Seq[Int]]): String =
        val maxIndex = digitIndices.values.flatten.max
        digitIndices.view.filterKeys(digitIndices(_) contains maxIndex).keys.head

    def doubleDigitIntFromLine(line: String): Int =
        val digitIndices = getDigitIndices(line)
        (firstDigit(digitIndices) + lastDigit(digitIndices)).toInt

    private val digitsMap: Map[String, String] = Map(
        "one" -> "1",
        "two" -> "2",
        "three" -> "3",
        "four" -> "4",
        "five" -> "5",
        "six" -> "6",
        "seven" -> "7",
        "eight" -> "8",
        "nine" -> "9"
    )

}
