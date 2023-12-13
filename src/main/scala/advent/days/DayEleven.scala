package advent.days

import scala.io.{BufferedSource, Source}

object DayEleven {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayElevenInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val universe: Seq[Seq[String]] = getUniverse(lines.toSeq)
        val coordinates: Seq[(BigInt, BigInt)] = expandCoordinates(
            coordinates = galaxyCoordinates(universe),
            expandedRows = emptyRows(universe),
            expandedColumns = emptyColumns(universe),
            expansion = 1000000
        )
        println(coordinates)
        println(emptyRows(universe))
        println(emptyColumns(universe))
        println(distanceFromCoordinates(coordinates).sum)

    def partOne(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val expandedUniverse = expand(getUniverse(lines.toSeq))
        println(allDistances(expandedUniverse).sum)

    def getUniverse(lines: Seq[String]): Seq[Seq[String]] =
        lines.map(_.split(""))

    def expandRows(universe: Seq[Seq[String]]): Seq[Seq[String]] =
        if (universe.isEmpty) {
            Seq.empty
        } else if (universe.head.forall(_ == ".")) {
            Seq(universe.head, universe.head) ++ expandRows(universe.tail)
        } else {
            Seq(universe.head) ++ expandRows(universe.tail)
        }

    def emptyRows(universe: Seq[Seq[String]]): Seq[BigInt] =
        universe.zipWithIndex.flatMap((row, rowIndex) => if (row.forall(_ ==".")) Some(rowIndex) else None)

    def emptyColumns(universe: Seq[Seq[String]]): Seq[BigInt] =
        universe.head.indices.flatMap(
            i => if (universe.map(_(i)).forall(_ == ".")) Some(i) else None
        )

    def expand(universe: Seq[Seq[String]]): Seq[Seq[String]] =
        val expandedRowUniverse = expandRows(universe)
        val columnsToExpand: Seq[BigInt] = emptyColumns(expandedRowUniverse)
        expandedRowUniverse.map(
            row => {
                row.zipWithIndex.flatMap { case (s, i) => if (columnsToExpand.contains(i)) Seq(s, s) else Seq(s) }
            }
        )

    def galaxyCoordinates(universe: Seq[Seq[String]]): Seq[(BigInt, BigInt)] =
        universe.zipWithIndex.flatMap((row, rowIndex) => {
            row.zipWithIndex.flatMap((s, columnIndex) => if (s == "#") Some((rowIndex, columnIndex)) else None)
        })

    def distance(cordA: (BigInt, BigInt), cordB: (BigInt, BigInt)): BigInt =
        val yDistance: BigInt = (cordA._1 - cordB._1).abs
        val xDistance: BigInt = (cordA._2 - cordB._2).abs
        xDistance + yDistance

    def expandCoordinates(
                           coordinates: Seq[(BigInt, BigInt)],
                           expandedRows: Seq[BigInt],
                           expandedColumns: Seq[BigInt],
                           expansion: BigInt
                         ): Seq[(BigInt, BigInt)] =
        coordinates.map(
            galaxyPosition => {
                val rowCount: BigInt = expandedRows.count(_ < galaxyPosition._1)
                val columnCount: BigInt = expandedColumns.count(_ < galaxyPosition._2)
                (
                  galaxyPosition._1 + (rowCount * expansion) - rowCount,
                  galaxyPosition._2 + (columnCount * expansion) - columnCount
                )
            }
        )

    def allDistances(universe: Seq[Seq[String]]): Seq[BigInt] =
        val coordinates: Seq[(BigInt, BigInt)] = galaxyCoordinates(universe)
        distanceFromCoordinates(coordinates)

    def distanceFromCoordinates(coordinates: Seq[(BigInt, BigInt)]): Seq[BigInt] =
        coordinates.combinations(2).map(galaxyPair => distance(galaxyPair.head, galaxyPair.last)).toSeq

}
