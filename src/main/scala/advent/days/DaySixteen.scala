package advent.days

import scala.io.{BufferedSource, Source}

object DaySixteen {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DaySixteenInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val mirrorMatrix = getMatrix(lines)
        println(countCharged(mirrorMatrix))
        println(maxCharged(mirrorMatrix))


    def getMatrix(lines: Iterator[String]): Seq[Seq[String]] =
        lines.map(_.split("").toSeq).toSeq

    def reflect(c: String, inDirection: Int): Seq[Int] =
        // Directions
        //    1
        //  0 . 2
        //    3
        val characterMapping: Map[String, Map[Int, Seq[Int]]] = Map(
            "."  -> Map(0 -> Seq(2), 1 -> Seq(3), 2 -> Seq(0), 3 -> Seq(1)),
            "|"  -> Map(0 -> Seq(1, 3), 1 -> Seq(3), 2 -> Seq(1, 3), 3 -> Seq(1)),
            "-"  -> Map(0 -> Seq(2), 1 -> Seq(0, 2), 2 -> Seq(0), 3 -> Seq(0, 2)),
            "/"  -> Map(0 -> Seq(1), 1 -> Seq(0), 2 -> Seq(3), 3 -> Seq(2)),
            "\\" -> Map(0 -> Seq(3), 1 -> Seq(2), 2 -> Seq(1), 3 -> Seq(0))
        )
        characterMapping(c)(inDirection)

    def nextPositions(currentPos: (Int, Int), directions: Seq[Int]): Map[Int, (Int, Int)] =
        directions.map(
            outDirection => {
                val nextInDirection: Int = Map(0 -> 2, 1 -> 3, 2 -> 0, 3 -> 1)(outDirection)
                if (outDirection == 0) {
                    nextInDirection -> (currentPos._1, currentPos._2 - 1)
                } else if (outDirection == 1) {
                    nextInDirection -> (currentPos._1 - 1, currentPos._2)
                } else if (outDirection == 2) {
                    nextInDirection -> (currentPos._1, currentPos._2 + 1)
                } else { // outDirection == 3
                    nextInDirection -> (currentPos._1 + 1, currentPos._2)
                }
            }
        ).toMap

    def countCharged(
                      matrix: Seq[Seq[String]],
                      inDirection: Int = 0,
                      pos: (Int, Int) = (0, 0)
                    ): Int =

        var charged: Seq[(Seq[Int], Int, Int)] = Seq.empty

        def navigate(
                      matrix: Seq[Seq[String]],
                      inDirection: Int = 0,
                      pos: (Int, Int) = (0, 0)
                    ): Unit =
            val outOfBounds: Boolean = pos._1 < 0 | pos._1 == matrix.length | pos._2 < 0 | pos._2 == matrix.head.length
            if (!outOfBounds) {
                val outDirections: Seq[Int] = reflect(matrix(pos._1)(pos._2), inDirection)
                val mirrorKey = (outDirections, pos._1, pos._2)
                val alreadyVisited: Boolean = charged.contains(mirrorKey)
                if (!alreadyVisited) {
                    charged = charged :+ mirrorKey
                    val nextSteps: Map[Int, (Int, Int)] = nextPositions(pos, outDirections)
                    nextSteps.foreach(
                        nextDirectionPos =>
                            navigate(
                                matrix = matrix,
                                inDirection = nextDirectionPos._1,
                                pos = nextDirectionPos._2
                            )
                    )
                }
            }

        navigate(matrix, inDirection, pos)

        charged.map(k => (k._2, k._3)).distinct.length

    def maxCharged(matrix: Seq[Seq[String]]): Int =
        // row, column
        val startingStates: Seq[((Int, Int), Int)] =
            matrix.indices.map(i => ((i, 0), 0)) ++
              matrix.indices.map(i => ((i, matrix.head.length - 1), 2)) ++
              matrix.head.indices.map(i => ((0, i), 1)) ++
              matrix.head.indices.map(i => ((matrix.length - 1, i), 3))

        startingStates.map(
            state => countCharged(matrix = matrix, inDirection = state._2, pos = state._1)
        ).max
}
