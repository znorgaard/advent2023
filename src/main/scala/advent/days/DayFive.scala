package advent.days

import scala.io.{BufferedSource, Source}
import scala.collection.BufferedIterator

object DayFive {
    // Reflection, not happy with this solution but it works
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayFiveInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        //val part1: Long = partOne(lines)
        val part2: Long = partTwo(lines)
        println(part2)

    def mapOrElse(inputVal: Long, ranges: Seq[(Long, Long, Int)]): Long =
        val matched = ranges.flatMap(range => {
            val destination: Long = range.head
            val source: Long = range(1)
            val i: Int = range.last
            if ((source <= inputVal) & (inputVal <= source + i - 1)) {
                Some(destination + (inputVal - source))
            } else None
        })
        if (matched.isEmpty) inputVal else matched.head


    def partOne(lines: Iterator[String]): Long =
        val bufferedLines: BufferedIterator[String] = lines.buffered
        val seeds = lines.next.replace("seeds: ", "").split(" ").map(_.toLong).toSeq
        var numbers: Seq[Seq[(Long, Long, Int)]] = Seq[Seq[(Long, Long, Int)]]()
        while bufferedLines.hasNext do {
            while bufferedLines.head.isEmpty do bufferedLines.next // Skip blank lines
            val mapName: String = bufferedLines.next.replace(" map:", "")
            var ranges: Seq[(Long, Long, Int)] = Seq[(Long, Long, Int)]()
            while bufferedLines.headOption.getOrElse("").nonEmpty do {
                val splitLine = bufferedLines.next.split(" ").map(_.toLong)
                ranges = ranges :+ (splitLine.head, splitLine(1), splitLine.last.toInt)
            }
            numbers = numbers :+ ranges
        }
        seeds.map(
            seed => {
                var seedVal = seed
                numbers.indices.foreach( i =>
                    {
                        val ranges = numbers(i)
                        seedVal = mapOrElse(seedVal, ranges)
                    }
                )
                seedVal
            }
        ).min

    def partTwo(lines: Iterator[String]): Long =
        // Super slow and inefficient but gives the correct answer
        val bufferedLines: BufferedIterator[String] = lines.buffered
        val seeds = lines.next.replace("seeds: ", "").split(" ").map(_.toLong).toSeq
        var numbers: Seq[Seq[(Long, Long, Int)]] = Seq[Seq[(Long, Long, Int)]]()
        while bufferedLines.hasNext do {
            while bufferedLines.head.isEmpty do bufferedLines.next // Skip blank lines
            val mapName: String = bufferedLines.next.replace(" map:", "")
            var ranges: Seq[(Long, Long, Int)] = Seq[(Long, Long, Int)]()
            while bufferedLines.headOption.getOrElse("").nonEmpty do {
                val splitLine = bufferedLines.next.split(" ").map(_.toLong)
                ranges = ranges :+ (splitLine.head, splitLine(1), splitLine.last.toInt)
            }
            numbers = numbers :+ ranges
        }
        val seedGroups = seeds.grouped(2).toSeq
        seedGroups.map(
          seedPair =>
            val seedRange = seedPair.head to seedPair.head + seedPair.last - 1
            var seedCount = 0
            seedRange.map(
                seed => {
                    seedCount += 1
                    if (seedCount % 10000000 == 0) println(seedCount.toString + " seeds analyzed")
                    var seedVal = seed
                    numbers.indices.foreach(i => {
                        val ranges = numbers(i)
                        seedVal = mapOrElse(seedVal, ranges)
                    }
                    )
                    seedVal
                }
            ).min
        ).min
}
