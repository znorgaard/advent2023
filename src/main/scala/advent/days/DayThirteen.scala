package advent.days

import scala.io.{BufferedSource, Source}
import scala.collection.BufferedIterator

object DayThirteen {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayThirteenInput.txt")

    def main(): Unit =
        val lines: BufferedIterator[String] = puzzleInput
          .getLines().buffered
        val patternSeq = getPatterns(lines)
        println(patternSeq.map(patternValue).sum)
        println(patternSeq.map(newPatternValue).sum)

    val ash: String = "."
    val rock: String = "#"

    def reflectionIndices(patternRow: String): Seq[Int] =
        (1 until patternRow.length).flatMap(
            i => {
                val sliceSize = Seq(i, patternRow.length - i).min
                if (patternRow.slice(i - sliceSize, i) == patternRow.slice(i, i + i).reverse) Some(i) else None
            }
        )

    def findReflections(pattern: Seq[String]): Option[Int] =
        val rowReflections: Seq[Seq[Int]] = pattern.map(reflectionIndices)
        val allReflections: Seq[Int] = rowReflections.flatten.distinct
        rowReflections.foldLeft(allReflections)((x, y) => x.intersect(y)).headOption


    def transformPattern(pattern: Seq[String]): Seq[String] =
        pattern.head.indices.map(
            i => pattern.map(patternRow => patternRow(i)).mkString
        )

    def patternValue(pattern: Seq[String]): Int =
        val maybeRowReflection = findReflections(pattern)
        if (maybeRowReflection.nonEmpty) {
            maybeRowReflection.get
        } else {
            findReflections(transformPattern(pattern)).get * 100
        }

    def getPatterns(lines: BufferedIterator[String]): Seq[Seq[String]] =
        if (!lines.hasNext) {
            Seq.empty
        } else {
            Seq(lines.takeWhile(_.nonEmpty).toSeq) ++ getPatterns(lines)
        }

    // part two
    // for each row, switch each symbol in each position, see if the new reflections indices match a different value
    def getCandidateReflections(reflections: Seq[Seq[Int]], exclude: Option[Int]): Seq[Int] =
        val remainingReflections = if (exclude.isEmpty) {
            reflections
        } else {
            reflections.map(rowReflections => rowReflections.filter(_ != exclude.get))
        }
        remainingReflections
          .flatten
          .groupBy(identity)
          .view.mapValues(_.size)
          .filter(_._2 == (reflections.length - 1))
          .keys
          .toSeq

    def rowsToFix(reflections: Seq[Seq[Int]], candidateReflections: Seq[Int]): Seq[Int] =
        candidateReflections.flatMap(
                reflectionAxis => {
                    reflections.zipWithIndex.flatMap {
                        (rowReflections, i) =>
                        if (rowReflections.contains(reflectionAxis)) { None } else {Some(i)}
                    }
                }
            )

    def alternateReflectionIndices(rowPattern: String): Seq[Seq[Int]] =
        val alternateMap: Map[String, String] = Map("." -> "#", "#" -> ".")
        val alternateRows = rowPattern.indices.map(
            i => rowPattern.take(i) + alternateMap(rowPattern(i).toString) + rowPattern.drop(i+1)
        )
        alternateRows.map(reflectionIndices)

    def newReflection(pattern: Seq[String]): Option[Int] =
        val maybeReflection: Option[Int] = findReflections(pattern)
        val reflections: Seq[Seq[Int]] = pattern.map(reflectionIndices)
        val candidateReflections: Seq[Int] = getCandidateReflections(reflections, maybeReflection)
        val rowIndices: Seq[Int] = rowsToFix(reflections, candidateReflections)
        rowIndices.flatMap(
            i => {
                alternateReflectionIndices(pattern(i)).flatten.filter(r => candidateReflections.contains(r))
            }
        ).headOption


    def newPatternValue(pattern: Seq[String]): Int =
        val maybeRowReflection = newReflection(pattern)
        if (maybeRowReflection.nonEmpty) {
            maybeRowReflection.get
        } else {
            newReflection(transformPattern(pattern)).get * 100
        }

}
