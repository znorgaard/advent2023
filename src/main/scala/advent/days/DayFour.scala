package advent.days

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}

object DayFour {
    val puzzleInput: BufferedSource = Source.fromFile("src/main/resources/DayFourInput.txt")
    def main(): Unit =
        val startingCards: Seq[Seq[Seq[Int]]] = puzzleInput
          .getLines()
          .map(parseLine)
          .toSeq
        val startingCounts = Seq.fill(startingCards.length){1}
        val total: Int = countCards(startingCards, startingCounts)
        println(total)

    def partOne(): Unit =
        val total: Double = puzzleInput
          .getLines()
          .map(parseLine)
          .map(lineSeq => DayFour.score(lineSeq(1), lineSeq(2)))
          .sum
        println(total)

    def parseLine(line: String): Seq[Seq[Int]] =
        "Card\\s+".r.replaceFirstIn(line, "")
          .replace("Card ", "")
          .split(":\\s+|\\s+\\|\\s+")
          .map(_.split("\\s+").map(_.toInt).toSeq).toSeq

    def score(winningNumbers: Seq[Int], elfNumbers: Seq[Int]): Double =
        val matchCount: Int = wins(winningNumbers, elfNumbers)
        if (matchCount == 0) {0} else {scala.math.pow(2, matchCount - 1)}

    def wins(winningNumbers: Seq[Int], elfNumbers: Seq[Int]): Int =
        winningNumbers.intersect(elfNumbers).length

    def addCards(card: Seq[Seq[Int]], counts: Seq[Int]): Seq[Int] =
        val cardIndex: Int = card.head.head - 1
        val currentCardCount: Int = counts(cardIndex)
        val cardWins: Int = wins(card(1), card(2))
        val indexSet = cardIndex + 1 to cardIndex + cardWins
        counts.zipWithIndex.map { case (currentCount, index) =>
            if (indexSet contains index) currentCount + currentCardCount else currentCount
        }

    @tailrec
    def countCards(cards: Seq[Seq[Seq[Int]]], counts: Seq[Int]): Int =
        val newCounts = addCards(cards.head, counts)
        if (cards.tail.isEmpty) newCounts.sum else countCards(cards.tail, newCounts)

}
