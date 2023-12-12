package advent.days

import scala.io.{BufferedSource, Source}

object DayTen {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayTenInput.txt")
    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        buildLoops(getMatrix(lines.toSeq))


    val symbolCodes = Map(
        // Direction codes
        //   1
        // 0 + 2
        //   3
        "|" -> Seq(1, 3),
        "-" -> Seq(0, 2),
        "L" -> Seq(1, 2),
        "J" -> Seq(0, 1),
        "7" -> Seq(0, 3),
        "F" -> Seq(2, 3),
        "." -> Seq.empty,
        "S" -> Seq.empty
    )

    val movementCodes = Map(
        0 -> ( 0, -1), // Same row, previous column
        1 -> (-1,  0), // Previous row, same column
        2 -> ( 0,  1), // Same row, next column
        3 -> ( 1,  0)  // Next row, same column
    )

    def getMatrix(lines: Seq[String]): Seq[Seq[String]] =
        lines.map(_.split(""))

    def findAnimal(matrix: Seq[Seq[String]]): (Int, Int) =
        matrix.zipWithIndex.flatMap((row, rowIndex) => {
            if (row.contains("S")) {
                Some((rowIndex, row.indexOf("S")))
            } else
                None
        }).head


    def checkConnection(symbol: String, from: Int): Boolean =
        symbol == "S" || symbolCodes(symbol).contains(from)

    def nextPosition(currentPostion: (Int, Int), symbol: String, from: Int): (Int, Int) =
        val movement = movementCodes(whereTo(symbol, from))
        (currentPostion._1 + movement._1, currentPostion._2 + movement._2)

    def whereTo(symbol: String, from: Int): Int =
        symbolCodes(symbol).filter(_ != from).head

    def whereFrom(toDirection: Int): Int =
        Map(0 -> 2, 1 -> 3, 2 -> 0, 3 -> 1)(toDirection)

    def getSymbol(matrix: Seq[Seq[String]], position: (Int, Int)): String =
        matrix(position._1)(position._2)

    def buildLoop(matrix: Seq[Seq[String]], currentPostion: (Int, Int), from: Int): Seq[(Int, Int)] =
        val currentSymbol: String = getSymbol(matrix, currentPostion)
        val connectionCheck: Boolean = checkConnection(currentSymbol, from)
        if (currentSymbol == "." || !connectionCheck) {
            Seq.empty // Not part of a loop, we're done!
        } else if (currentSymbol == "S") {
            Seq(currentPostion) // We looped, we're done!
        } else {
            val nextPostion = nextPosition(currentPostion, currentSymbol, from)
            val nextFrom = whereFrom(whereTo(currentSymbol, from))
            Seq(currentPostion) ++ buildLoop(matrix, nextPostion, nextFrom)
        }

    def buildLoops(matrix: Seq[Seq[String]]): Unit =
        val startLocation = findAnimal(matrix)
        // Technically only need to check until you find the full loop, but oh well
        // Go North
        val northLoop = buildLoop(
            matrix,
            (startLocation._1 - 1, startLocation._2),
            3
        )
        // Go South
        val southLoop = buildLoop(
            matrix,
            (startLocation._1 + 1, startLocation._2),
            1
        )
        // Go West
        val westLoop = buildLoop(
            matrix,
            (startLocation._1, startLocation._2 + 1),
            0
        )
        // Go East
        val eastLoop = buildLoop(
            matrix,
            (startLocation._1, startLocation._2 - 1),
            2
        )
        val loops: Seq[Seq[(Int, Int)]] = Seq(northLoop, eastLoop, southLoop, westLoop)
        println(loops.map(_.length).max / 2)


}
