package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.{numberChars, numberOrSpace}
import rw.nick.aoc.Solution

import scala.math.pow

object Dec4 {

  case class Card(id: Int, winningNumbers: Set[Int], myNumbers: Set[Int]) {
    lazy val points: Int = {
      val myWinners = winningNumbers.intersect(myNumbers)
      if (myWinners.size == 1)
        1
      else
        1 * pow(2, myWinners.size-1).intValue
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

object Dec4Part1 extends Solution("inputs/2023/dec4.txt") {
  import Dec4.parseLine
  override def solution: Int = {
    input.map(parseLine(_).points).sum
  }
}