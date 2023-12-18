package advent.days

import scala.io.{BufferedSource, Source}
object DayFifteen {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayFifteenInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        println(foldSteps(getInstructions(lines.next)))

    def partOne(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        val asciiSeqs = getInput(lines)
        println(asciiSeqs.map(asciiSeq => hash(asciiSeq)).sum)

    def getInput(lines: Iterator[String]): Seq[Seq[Int]] =
        lines.next.split(",").map(_.map(_.toInt))

    def getInstructions(line: String): Seq[String] =
        line.split(",")

    def parseStep(step: String): (String, String, Option[Int]) =
        val modification: String = if (step.contains("=")) "=" else "-"
        val splitStep: Seq[String] = step.split(modification)
        val lens: Option[Int] = if (splitStep.tail.nonEmpty) Some(splitStep.last.toInt) else None
        (splitStep.head, modification, lens)

    def updateBoxes(boxMap: Map[Int, Seq[(String, Int)]], step: String): Map[Int, Seq[(String, Int)]] =
            val (label, mod, lens) = parseStep(step)
            val labelHash: Int = hashLabel(label)
            if (mod == "-") {
                boxMap + (labelHash -> boxMap(labelHash).filter(_._1 != label))
            } else if (!boxMap(labelHash).exists(_._1 == label)) {
                boxMap + (labelHash -> (boxMap(labelHash) :+ (label, lens.get)))
            } else {
                val newBox = boxMap(labelHash).map(
                    (oldLabel, oldLens) => {
                        val newLens = if (oldLabel == label) lens.get else oldLens
                        (oldLabel, newLens)
                    }
                )
                boxMap + (labelHash -> newBox)
            }

    def boxPower(boxNumber: Int, lenses: Seq[(String, Int)]): Int =
        lenses.zipWithIndex.map(
            (labelLensPair, i) => (1 + boxNumber) * (1 + i) * labelLensPair._2
        ).sum

    def foldSteps(instructions: Seq[String]): Int =
        val startingMap: Map[Int, Seq[(String, Int)]] = (0 to 255).map(_ -> Seq.empty).toMap
        val boxes: Map[Int, Seq[(String, Int)]] = instructions.foldLeft(startingMap)(
            (boxMap, step) => updateBoxes(boxMap, step)
        )
        boxes.map((boxNumber, lenses) => boxPower(boxNumber, lenses)).sum



    def hash(asciiSeq: Seq[Int]): Int =
        asciiSeq.foldLeft(0)(
            (a, b) => {
                (a + b) * 17 % 256
            }
        )

    def hashLabel(label: String): Int =
        hash(label.map(_.toInt))
}
