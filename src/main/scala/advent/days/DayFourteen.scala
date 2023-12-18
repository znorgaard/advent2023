package advent.days

import scala.io.{BufferedSource, Source}

object DayFourteen {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayFourteenInput.txt")

    def main(): Unit =
        val lines: Seq[String] = puzzleInput
          .getLines().toSeq
        println(getColumns(lines).map(roll).map(columnLoad).sum)

    def getColumns(lines: Seq[String]): Seq[String] =
        lines.head.indices.map(
            i => lines.map(_(i)).mkString
        )
    def columnLoad(column: String): Int =
        column.zipWithIndex.map((c, i) => if (c.toString == "O") {column.length - i} else {0}).sum

    def roll(column: String): String =
        // Need to find positions of cubes, "#", and round rocks "O"
        val cubePos = "#".r.findAllMatchIn(column).map(_.start).toSeq
        val roundPos = "O".r.findAllMatchIn(column).map(_.start).toSeq
        if (cubePos.isEmpty) {
            "O" * roundPos.length + "." * (column.length - roundPos.length)
        } else {
            val stopPos = Seq(-1) ++ cubePos ++ Seq(column.length + 1)
            val roundGroupSizes = stopPos.indices.tail.map(
                i => {
                    roundPos.count(roundIndex => roundIndex > stopPos(i - 1) & roundIndex < stopPos(i))
                }
            )
            val columnStart = cubePos.indices.map(
                i => {
                    val previousCubePos = if (i == 0) {0} else {cubePos(i - 1) + 1}
                    val dotCount = cubePos(i) - roundGroupSizes(i) - previousCubePos
                    "O" * roundGroupSizes(i) + "." * dotCount + "#"
                }
            ).mkString
            val lastRounds = if (cubePos.length == roundGroupSizes.length) {""} else {"O" * roundGroupSizes.last}
            columnStart + lastRounds + "." * (column.length - columnStart.length - lastRounds.length)
        }
}
