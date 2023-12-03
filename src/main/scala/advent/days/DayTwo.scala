package advent.days

import java.awt.GridBagConstraints
import scala.io.{BufferedSource, Source}

object DayTwo {
    def main(): Unit =
        val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayTwoInput.txt")
        val total: Int = sumPower(puzzleInput.getLines())
        println(total)

    def partOne(): Unit =
        val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayTwoInput.txt")
        val bagContents: Map[String, Int] = Map("red" -> 12, "green" -> 13, "blue" -> 14)
        val total: Int = sumGames(puzzleInput.getLines(), bagContents)
        println(total)


    def maxDraws(line: String): (Int, Map[String, Int]) =
        val game: Seq[String] = line.split(": ")
        val gameNumber: Int = game.head.split(" ").last.toInt
        val draws: Seq[Map[String, Int]] = game.last.split("; ")
          .map(draw => draw.split(", ").map(
            color => {
                val countColorSeq: Seq[String] = color.split(" ")
                countColorSeq.last -> countColorSeq.head.toInt
            }).toMap
        )
        val maxCounts = draws.flatMap(_.keys).distinct.map(
            color => {
                color -> draws.map(_.getOrElse(color, 0)).max
            }
        ).toMap
        gameNumber -> maxCounts

    def possible(bagContents: Map[String, Int], maxCounts: Map[String, Int]): Boolean =
        bagContents.keys.map(
            color => {
                bagContents(color) >= maxCounts.getOrElse(color, 0)
            }
        ).forall(identity)

    def sumGames(lines: Iterator[String], bagContents: Map[String, Int]): Int =
        lines
          .map(maxDraws)
          .filter(gameTuple => possible(bagContents, gameTuple._2))
          .map(_._1)
          .sum

    def gamePower(maxCounts: Map[String, Int]): Int =
        maxCounts.values.product

    def sumPower(lines: Iterator[String]): Int =
        lines.map(maxDraws)
          .map(gameTuple => gamePower(gameTuple._2))
          .sum

}
