package advent.days

import scala.io.{BufferedSource, Source}

object DayThree {
    // Reflection: This could be simplified a lot.
    //  - Single function to find x coordinates of any regex
    //  - Coordinates object could be Map((Seq(Xs), Y) -> matchedString)
    //  - Single function to identify neighboring elements in a different coordinates object

    def main(): Unit =
        val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayThreeInput.txt")
        val coordinates: Map[Int, (Map[(Int, Int), Int], Seq[Int])] = textToGearCoordinates(puzzleInput.getLines())
        val total: Int = gearFilterAndId(coordinates).sum
        println(total)

    def partOne(): Unit =
        val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayThreeInput.txt")
        val coordinates: Map[Int, (Map[(Int, Int), Int], Seq[Int])] = textToCoordinates(puzzleInput.getLines())
        val total: Int = numericFilter(coordinates).sum
        println(total)

    def numericPositions(line: String): Map[(Int, Int), Int] =
        "[0-9]+".r.findAllMatchIn(line).map(m => {
            (m.start, m.end - 1) -> m.matched.toInt
        }).toMap

    def symbolPositions(line: String): Seq[Int] =
        "[^0-9.]".r.findAllMatchIn(line).map(_.start).toSeq

    def gearPositions(line: String): Seq[Int] =
        "\\*".r.findAllMatchIn(line).map(_.start).toSeq

    def textToCoordinates(lines: Iterator[String]): Map[Int, (Map[(Int, Int), Int], Seq[Int])] =
        lines
          .zipWithIndex
          .map {
              (line, lineNumber) => {
                  lineNumber -> (numericPositions(line), symbolPositions(line))
              }
          }.toMap

    def textToGearCoordinates(lines: Iterator[String]): Map[Int, (Map[(Int, Int), Int], Seq[Int])] =
        lines
          .zipWithIndex
          .map {
              (line, lineNumber) => {
                  lineNumber -> (numericPositions(line), gearPositions(line))
              }
          }.toMap


    def symbolPosFromRows(coordinates: Map[Int, (Map[(Int, Int), Int], Seq[Int])], rows: Seq[Int]): Seq[Int] =
        val nearbyCoordinates: Map[Int, (Map[(Int, Int), Int], Seq[Int])] =
            coordinates.view.filterKeys(rowNumber => rows.contains(rowNumber)).toMap
        nearbyCoordinates.flatMap(_._2._2).toSeq

    def numericFilter(coordinates: Map[Int, (Map[(Int, Int), Int], Seq[Int])]): Seq[Int] =
        coordinates.map(
            linePair => {
                val y: Int = linePair._1
                val numericPositions: Map[(Int, Int), Int] = linePair._2._1
                numericPositions.filter(
                    posPair => {
                        val x: (Int, Int) = posPair._1
                        val symbolXs: Seq[Int] = symbolPosFromRows(coordinates, (y - 1) to (y + 1))
                        symbolXs.exists(symbolX => ((x._1 - 1) <= symbolX) & (symbolX <= (x._2 + 1)))
                    }
                ).values.toSeq
            }
        ).toSeq.flatten

    def gearFilterAndId(gearCoordinates: Map[Int, (Map[(Int, Int), Int], Seq[Int])]): Seq[Int] =
        gearCoordinates.flatMap(
            linePair => {
                val y: Int = linePair._1
                val gearPositions: Seq[Int] = linePair._2._2
                val nearbyNumbers: Seq[Map[(Int, Int), Int]] = gearCoordinates.view
                  .filterKeys(rowVal => (y - 1 <= rowVal) & (rowVal <= y + 1))
                  .toMap.values.map(_._1).toSeq
                gearPositions.map(
                    gearPos => {
                        val numbers: Seq[Int] = nearbyNumbers.flatMap(
                            _.view.filterKeys(
                                numericXs =>
                                    Seq(numericXs._1, numericXs._2).exists(x => (gearPos - 1 <= x) & (x <= gearPos + 1))
                            ).values
                        )
                        if (numbers.length == 2) {numbers.head * numbers.last} else {0}
                    }
                )
            }
        ).toSeq
}
