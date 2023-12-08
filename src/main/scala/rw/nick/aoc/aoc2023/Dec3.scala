package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.numberChars
import rw.nick.aoc.Solution

object Dec3 {

  case object Symbol

  case class Address(absX: Int, relY: Int)
  case class NumberGroup(value: Int, neighbourCellAddresses: Seq[Address])

  abstract class Cell
  case class NumberCell(value: Char) extends Cell
  case class SymbolCell(symbol: Char) extends Cell
  case object EmptyCell extends Cell
  case class FixedCell(cell: Cell, address: Address)

  case class CellRow(cells: Seq[FixedCell], numberGroups: Seq[NumberGroup])

  def parseLine(line: String): CellRow = {
    val matchedTypes: IndexedSeq[FixedCell] = line.zipWithIndex.map {
      case (char, i) if numberChars.contains(char) => FixedCell(NumberCell(char), Address(i, 0))
      case (char, i) if char == '.' => FixedCell(EmptyCell, Address(i, 0))
      case (char, i) => FixedCell(SymbolCell(char), Address(i, 0))
    }
    val groups: List[NumberGroup] = matchedTypes
      .foldLeft(List[List[FixedCell]]()) {
        case (coll: List[List[FixedCell]], FixedCell(cell: NumberCell, address: Address)) =>
          val replacementPrevGroup = coll.lastOption.flatMap {
            case prevGroup if prevGroup.last.address.absX == address.absX-1 =>
              Some(prevGroup :+ FixedCell(cell, address))
            case _ => None
          }
          replacementPrevGroup.map { prevGroup =>
            coll.slice(0, coll.length-1) :+ prevGroup
          }.getOrElse(coll :+ List(FixedCell(cell, address)))
        case (coll: List[List[FixedCell]], _) => coll
      }
      .map(formGroup(line.length))

    CellRow(matchedTypes, groups)
  }

  def formGroup(maxIndex: Int)(group: List[FixedCell]): NumberGroup = {
    val groupNumber: Int = group.foldLeft("") { case (coll: String, FixedCell(cell: NumberCell, address: Address)) =>
      coll ++ cell.value.toString
    }.toInt
    val firstX = group.head.address.absX
    val lastX = group.last.address.absX
    NumberGroup(groupNumber, neighbours(firstX, lastX, maxIndex))
  }

  def neighbours(startIndex: Int, endIndex: Int, maxIndex: Int): Seq[Address] = {
    val startLeft = if (startIndex == 0)
      startIndex
    else
      startIndex - 1
    val endRight = if (endIndex == maxIndex - 1)
      endIndex
    else
      endIndex + 1
    val lineAbove = (startLeft to endRight).map { x => Address(x, -1) }
    val leftNeighb = if (startIndex != 0) Some(Seq(Address(startIndex - 1, 0))) else None
    val rightNeighb = if (endIndex < maxIndex - 1) Some(Seq(Address(endIndex + 1, 0))) else None
    val lrNeighbs = leftNeighb.getOrElse(Seq()) ++ rightNeighb.getOrElse(Seq())
    val lineBelow = (startLeft to endRight).map { x => Address(x, 1) }
    lineAbove ++ lrNeighbs ++ lineBelow
  }

}

object Dec3Part1 extends Solution(inputPath = "inputs/2023/dec3.txt") {

  override def solution: Int = {
    val peekLen = input.next().length
    val emptyLine = (0 until peekLen).map(_ => '.').mkString
    val rows =(for line <- (Seq(emptyLine).iterator ++ input ++ Seq(emptyLine.strip).iterator)
      yield Dec3.parseLine(line))
    val groupsWithSymbolNeighbours = rows.sliding(3).flatMap { relRows =>
      relRows(1).numberGroups.filter { group =>
        val neighbours = group.neighbourCellAddresses.map { addr =>
          relRows(addr.relY+1).cells(addr.absX)
        }
        neighbours.exists {
          case Dec3.FixedCell(_: Dec3.SymbolCell, _: Dec3.Address) => true
          case _ => false
        }
      }
    }
    groupsWithSymbolNeighbours.map(_.value).sum
  }

}
