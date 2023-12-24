package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.numberChars
import rw.nick.aoc.{IntSolution, Solution}

import scala.util.matching.Regex


object Dec1Part1 extends IntSolution {
  override def solution(input: Iterator[String]): Int =
    (for line <- input
      yield sumConcatenatedFirstLastLetterDigitsInString(line)).sum

  def sumConcatenatedFirstLastLetterDigitsInString(value: String): Int =
    (for {
      first <- value.find(numberChars.contains)
      last <- value.findLast(numberChars.contains)
      } yield s"${first}${last}".toInt)
    .getOrElse(0)
}

object Dec1Part2 extends IntSolution {
  override def solution(input: Iterator[String]): Int =
    (for line <- input
    yield sumFirstLastNumberWordOrDigitInString(line)).sum

  val regex: Regex = """(\d|one|two|three|four|five|six|seven|eight|nine)(?:.*(\d|one|two|three|four|five|six|seven|eight|nine))?""".r

  val englishToDigit: Map[String, String] = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9",
  )

  def getNumericDigitFromEnglishWord(value: String): String =
    if (value.length == 1) {
      value
    } else {
      englishToDigit(value)
    }

  def sumFirstLastNumberWordOrDigitInString(value: String): Int = {
    regex.findFirstMatchIn(value)
      .map { mat =>
        val arrangedDigits = mat
          .subgroups
          .filter(_ != null)
          .map(getNumericDigitFromEnglishWord)
          .mkString
        if (arrangedDigits.length > 1) {
          arrangedDigits.toInt
        } else {
          (arrangedDigits + arrangedDigits).toInt
        }
      }
      .getOrElse(0)
  }
}