package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.numberChars
import rw.nick.aoc.Solution

object Dec3 {

  case object Symbol

  case class Address(absX: Int, relY: Int)
  case class NumberGroup(value: Int, neighbourCellAddresses: Seq[Address])

  abstract class Cell
  case class NumberCell(value: Char) extends Cell
  case class FixedNumberCell(nc: NumberCell, address: Address)
  case object SymbolCell extends Cell
  case object EmptyCell extends Cell

  case class CellRow(cells: Seq[Cell], numberGroups: Seq[NumberGroup])

  def parseLine(line: String): CellRow = {
    val matchedTypes: IndexedSeq[Cell] = line.zipWithIndex.map {
      case (char, i) if numberChars.contains(char) => NumberCell(char)
      case (char, i) if char == '.' => EmptyCell
      case _ => SymbolCell
    }
    val groups: List[NumberGroup] = matchedTypes
      .zipWithIndex
      .foldLeft(List[List[FixedNumberCell]]()) {
        case (coll: List[List[FixedNumberCell]], (cell: NumberCell, i: Int)) =>
          val replacementPrevGroup = coll.lastOption.flatMap {
            case prevGroup if prevGroup.last.address.absX == i-1 =>
              Some(prevGroup :+ FixedNumberCell(cell, Address(i, 0)))
            case _ => None
          }
          replacementPrevGroup.map { prevGroup =>
            coll.slice(0, coll.length-1) :+ prevGroup
          }.getOrElse(coll :+ List(FixedNumberCell(cell, Address(i, 0))))
        case (coll: List[List[FixedNumberCell]], _) => coll
      }
      .map(formGroup(line.length))

    CellRow(matchedTypes, groups)
  }

  def formGroup(maxIndex: Int)(group: List[FixedNumberCell]): NumberGroup = {
    val groupNumber: Int = group.foldLeft("") { case (coll: String, cell: FixedNumberCell) =>
      coll ++ cell.nc.value.toString
    }.toInt
    val firstX = group.head.address.absX
    val lastX = group.last.address.absX
    val startLeft = if (firstX == 0)
      firstX
    else
      firstX - 1
    val endRight = if (lastX == maxIndex -1)
      lastX
    else
      lastX + 1
    val lineAbove = (startLeft to endRight).map { x => Address(x, -1) }
    val leftNeighb = if (firstX != 0) Some(Seq(Address(firstX - 1, 0))) else None
    val rightNeighb = if (lastX < maxIndex - 1) Some(Seq(Address(lastX + 1, 0))) else None
    val lrNeighbs = leftNeighb.getOrElse(Seq()) ++ rightNeighb.getOrElse(Seq())
    val lineBelow = (startLeft to endRight).map { x => Address(x, 1)}
    val neighbours = lineAbove ++ lrNeighbs ++ lineBelow
    NumberGroup(groupNumber, neighbours)
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
        neighbours.contains(Dec3.SymbolCell)
      }
    }
    groupsWithSymbolNeighbours.map(_.value).sum
  }

}
