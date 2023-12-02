package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.numberChars
import rw.nick.aoc.Solution

object Dec2 {

  enum Colour:
    case Red, Green, Blue

  object Game {
    def apply(line: String): Game = {
      val (gameIDContainer, selectionContainer) = line.splitAt(line.indexOf(":") + 1)
      val gameID = gameIDContainer.filter(numberChars.contains).toInt
      val selections = selectionContainer
        .split(';')
        .map(_.split(',').map(singleSelectionParser))
        .map(_.reduce(_ ++ _))
      Game(gameID, selections.toList)
    }
  }

  case class Game(id: Int, selections: List[Map[Colour, Int]]) {
    def isPossibleWithMaxAvailable(setup: Map[Colour, Int]): Boolean =
      selections.forall { draw =>
        setup.forall { case (col, maximum) =>
          draw.getOrElse(col, 0) <= maximum
        }
      }

    lazy val minimumRequirements: Map[Colour, Int] =
      selections.fold(Map[Colour, Int]()) { case (collector, draw) =>
        (draw.toSeq ++ collector.toSeq)
          .groupMapReduce(_._1)(_._2)(math.max)
      }

    lazy val power: Int = minimumRequirements.values.product
  }

  def singleSelectionParser(selectionContainer: String): Map[Colour, Int] = {
    val (quantityStr, colourName) = selectionContainer.partition(numberChars.contains)
    Map {
      Colour.valueOf(colourName.strip.capitalize) -> quantityStr.toInt
    }
  }
}

abstract class Dec2 extends Solution("inputs/2023/dec2.txt")

object Dec2Part1 extends Dec2 {
  val Colour = Dec2.Colour
  override def solution: Int = {
    val gameSetup = Map(
      Colour.Red -> 12,
      Colour.Green -> 13,
      Colour.Blue -> 14,
    )
    (for line <- input
      yield Dec2.Game(line))
      .filter(_.isPossibleWithMaxAvailable(gameSetup))
      .map(_.id)
      .sum
  }
}

object Dec2Part2 extends Dec2 {
  override def solution: Int = {
    (for line <- input
      yield Dec2.Game(line))
      .map(_.power)
      .sum
  }
}
