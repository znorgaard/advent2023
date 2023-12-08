package advent.days

import scala.io.{BufferedSource, Source}
object DaySeven {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DaySevenInput.txt")

    def main(): Unit =
        val lines: Iterator[String] = puzzleInput
          .getLines()
        //val total: Int = partOne(lines)
        val total: Int = partTwo(lines)
        println(total)

    def partOne(lines: Iterator[String]): Int =
        val handBidPairs: Map[Hand, Int] = lines.map(readLine).toMap // Confirmed no duplicate hands in input
        val sortedHands: Seq[Hand] = handBidPairs.keys.toSeq.sorted(HandOrdering)
        sortedHands.zipWithIndex.map(
            handAndIndex => {
                handBidPairs(handAndIndex._1) * (handAndIndex._2 + 1)
            }
        ).sum

    def partTwo(lines: Iterator[String]): Int =
        val handBidPairs: Map[Hand, Int] = lines.map(readJokerLine).toMap // Confirmed no duplicate hands in input
        val sortedHands: Seq[Hand] = handBidPairs.keys.toSeq.sorted(JokerHandOrdering)
        sortedHands.zipWithIndex.map(
            handAndIndex => {
                handBidPairs(handAndIndex._1) * (handAndIndex._2 + 1)
            }
        ).sum

    private val cardMap: Map[String, Int] =
        Map("A" -> 14, "K" -> 13, "Q" -> 12, "J" -> 11, "T" -> 10) ++ (2 to 9).map(x => x.toString -> x)

    val jokerCardMap: Map[String, Int] =
        Map("A" -> 14, "K" -> 13, "Q" -> 12, "J" -> 0, "T" -> 10) ++ (2 to 9).map(x => x.toString -> x)
    def buildHand(handString: String, cMap: Map[String, Int] = cardMap): Seq[Int] =
        handString.split("").map(card => cMap(card))

    def rankHand(hand: Seq[Int]): Int =
        val cardCounts: Seq[Int] = hand.groupBy(identity).view.mapValues(_.size).values.toSeq
        cardCounts match {
            case fiveOfAKind if fiveOfAKind contains 5 => 7
            case fourOfAKind if fourOfAKind contains 4 => 6
            case fullHouse if Seq(2, 3).forall(fullHouse.contains) => 5
            case threeOfAKind if threeOfAKind contains 3 => 4
            case twoPair if twoPair.count(_ == 2) == 2 => 3
            case onePair if onePair contains 2 => 2
            case _ => 1
        }

    def rankJokerHand(hand: Seq[Int]): Int =
        val cardCountMap: Map[Int, Int] = hand.groupBy(identity).view.mapValues(_.size).toMap
        val nonZeroMap: Map[Int, Int] = cardCountMap.view.filterKeys(_ != 0).toMap
        val cardCounts = if (nonZeroMap.isEmpty) {
            Seq(cardCountMap(0))
        } else {
            val maxNonZeroCard: (Int, Int) = nonZeroMap.find(_._2 == nonZeroMap.values.max).get
            cardCountMap.view.filterKeys(key => !Seq(0, maxNonZeroCard._1).contains(key)).values.toSeq :+
              (maxNonZeroCard._2 + cardCountMap.getOrElse(0, 0))
        }
        cardCounts match {
            case fiveOfAKind if fiveOfAKind contains 5 => 7
            case fourOfAKind if fourOfAKind contains 4 => 6
            case fullHouse if Seq(2, 3).forall(fullHouse.contains) => 5
            case threeOfAKind if threeOfAKind contains 3 => 4
            case twoPair if twoPair.count(_ == 2) == 2 => 3
            case onePair if onePair contains 2 => 2
            case _ => 1
        }

    def readLine(line: String): (Seq[Int], Int) =
        val splitLine = line.split(" ")
        (buildHand(splitLine.head), splitLine.last.toInt)

    def readJokerLine(line: String): (Seq[Int], Int) =
        val splitLine = line.split(" ")
        (buildHand(splitLine.head, jokerCardMap), splitLine.last.toInt)

}

type Hand = Seq[Int]
object HandOrdering extends Ordering[Hand] {
    def compare(a: Hand, b: Hand): Int = {
        val aRank: Int = DaySeven.rankHand(a)
        val bRank: Int = DaySeven.rankHand(b)
        if (aRank != bRank) {
            aRank compare bRank
        } else {
            a.zipWithIndex.map(pair => pair._1 compare b(pair._2)).find(_ != 0).getOrElse(0)
        }
    }
}

object JokerHandOrdering extends Ordering[Hand] {
    def compare(a: Hand, b: Hand): Int = {
        val aRank: Int = DaySeven.rankJokerHand(a)
        val bRank: Int = DaySeven.rankJokerHand(b)
        if (aRank != bRank) {
            aRank compare bRank
        } else {
            a.zipWithIndex.map(pair => pair._1 compare b(pair._2)).find(_ != 0).getOrElse(0)
        }
    }
}