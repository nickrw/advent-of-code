package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.{numberChars, numberOrSpace}
import rw.nick.aoc.{IntSolution, Solution}

import scala.math.pow

object Dec4 {

  case class Card(id: Int, winningNumbers: Set[Int], myNumbers: Set[Int]) {
    lazy val winners: Int = winningNumbers.intersect(myNumbers).size
    lazy val points: Int = {
      if (winners == 1)
        1
      else
        1 * pow(2, winners-1).intValue
    }
  }

  def parseLine(line: String): Card = {
    val (cardIDPart, numbersPart) = line.splitAt(line.indexOf(":"))
    val (winningNumbersPart, myNumbersPart) = numbersPart.splitAt(numbersPart.indexOf("|"))
    def numbers(fromStr: String): Seq[Int] = {
      fromStr
        .filter(numberOrSpace)
        .replace("  ", " ")
        .strip()
        .split(' ')
        .map(_.toInt)
    }
    val cardID = cardIDPart.filter(numberChars.contains).toInt
    val winningNumbers = numbers(winningNumbersPart).toSet
    val myNumbers = numbers(myNumbersPart).toSet
    Card(id = cardID, winningNumbers = winningNumbers, myNumbers = myNumbers)
  }

}

object Dec4Part1 extends IntSolution {
  import Dec4.parseLine
  override def solution(input: Iterator[String]): Int = {
    input.map(parseLine(_).points).sum
  }
}

object Dec4Part2 extends IntSolution {
  import Dec4.{parseLine, Card}

  object Collector {
    val empty: Collector = Collector(0, Map.empty)
  }
  case class Collector(cards: Int, toDuplicate: Map[Int, Int]) {
    def mergeCard(card: Card): Collector = {
      val thisCardCount = toDuplicate.getOrElse(card.id, 0) + 1
      val mergedDuplicates = toDuplicate.removed(card.id) ++
        (1 to card.winners)
        .map(_+card.id)
        .map { dupeID =>
          dupeID -> (toDuplicate.getOrElse(dupeID, 0) + thisCardCount)
        }.toMap
      Collector(cards + thisCardCount, mergedDuplicates)
    }
  }

  override def solution(input: Iterator[String]): Int = {
    input
      .map(parseLine)
      .foldLeft(Collector.empty) { case (coll, card) =>
        coll.mergeCard(card)
      }
      .cards
  }
}