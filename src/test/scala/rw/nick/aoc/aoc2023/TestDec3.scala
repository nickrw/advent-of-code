package rw.nick.aoc.aoc2023

import org.scalatest.funsuite.AnyFunSuite
import rw.nick.aoc.aoc2023.Dec3.{Address, CellRow, EmptyCell, FixedCell, NumberCell, NumberGroup}

class TestDec3 extends AnyFunSuite {

  test("Parse line") {
    val input = "..35..633."
    val expected = CellRow(
      cells = Seq(
        FixedCell(EmptyCell, Address(0, 0)),
        FixedCell(EmptyCell, Address(1, 0)),
        FixedCell(NumberCell('3'), Address(2, 0)),
        FixedCell(NumberCell('5'), Address(3, 0)),
        FixedCell(EmptyCell, Address(4, 0)),
        FixedCell(EmptyCell, Address(5, 0)),
        FixedCell(NumberCell('6'), Address(6, 0)),
        FixedCell(NumberCell('3'), Address(7, 0)),
        FixedCell(NumberCell('3'), Address(8, 0)),
        FixedCell(EmptyCell, Address(9, 0)),
      ),
      numberGroups = Seq(
        NumberGroup(
          value = 35,
          neighbourCellAddresses = Seq(
            Address(1, -1),
            Address(2, -1),
            Address(3, -1),
            Address(4, -1),
            Address(1, 0),
            Address(4, 0),
            Address(1, 1),
            Address(2, 1),
            Address(3, 1),
            Address(4, 1),
          ),
          xCoords = Seq(2, 3),
        ),
        NumberGroup(
          value = 633,
          neighbourCellAddresses = Seq(
            Address(5, -1),
            Address(6, -1),
            Address(7, -1),
            Address(8, -1),
            Address(9, -1),
            Address(5, 0),
            Address(9, 0),
            Address(5, 1),
            Address(6, 1),
            Address(7, 1),
            Address(8, 1),
            Address(9, 1),
          ),
          xCoords = Seq(6, 7, 8),
        ),
      )
    )
    assert(Dec3.parseLine(input) == expected)
  }

  test("Dec3 Form Groups - middle of line") {
    val input = List(
      FixedCell(NumberCell('1'), Address(12, 0)),
      FixedCell(NumberCell('2'), Address(13, 0)),
      FixedCell(NumberCell('3'), Address(14, 0)),
    )
    val expectedGroup = NumberGroup(
      value=123,
      neighbourCellAddresses = List(
        Address(11, -1),
        Address(12, -1),
        Address(13, -1),
        Address(14, -1),
        Address(15, -1),
        Address(11, 0),
        Address(15, 0),
        Address(11, 1),
        Address(12, 1),
        Address(13, 1),
        Address(14, 1),
        Address(15, 1),
      ),
      xCoords = Seq(12, 13, 14),
    )
    assert(Dec3.formGroup(20)(input) == expectedGroup)
  }

  test("Dec3 Form Groups - start of line") {
    val input = List(
      FixedCell(NumberCell('1'), Address(0, 0)),
      FixedCell(NumberCell('2'), Address(1, 0)),
      FixedCell(NumberCell('3'), Address(2, 0)),
    )
    val expectedGroup = NumberGroup(
      value = 123,
      neighbourCellAddresses = List(
        Address(0, -1),
        Address(1, -1),
        Address(2, -1),
        Address(3, -1),
        Address(3, 0),
        Address(0, 1),
        Address(1, 1),
        Address(2, 1),
        Address(3, 1),
      ),
      xCoords = Seq(0, 1, 2),
    )
    assert(Dec3.formGroup(20)(input) == expectedGroup)
  }

  test("Dec3 Form Groups - end of line") {
    val input = List(
      FixedCell(NumberCell('1'), Address(12, 0)),
      FixedCell(NumberCell('2'), Address(13, 0)),
      FixedCell(NumberCell('3'), Address(14, 0)),
    )
    val expectedGroup = NumberGroup(
      value = 123,
      neighbourCellAddresses = List(
        Address(11, -1),
        Address(12, -1),
        Address(13, -1),
        Address(14, -1),
        Address(11, 0),
        Address(11, 1),
        Address(12, 1),
        Address(13, 1),
        Address(14, 1),
      ),
      xCoords = Seq(12, 13, 14),
    )
    assert(Dec3.formGroup(15)(input) == expectedGroup)
  }

  test("Dec3 Part 1 Solution") {
    assert(Dec3Part1.solution == 551094)
  }

  test("Dec 3 Part 2 - neighbour refactor") {
    val actual = Dec3.neighboursForIndex(5, 5, 10)
    val expected = Seq(
      Address(4, -1),
      Address(5, -1),
      Address(6, -1),
      Address(4, 0),
      Address(6, 0),
      Address(4, 1),
      Address(5, 1),
      Address(6, 1),
    )
    assert(expected == actual)
  }

  test("Dec 3 Part 2 - solution") {
    assert(Dec3Part2.solution == 80179647)
  }

}
