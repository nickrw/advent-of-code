package rw.nick.aoc.aoc2023

import org.scalatest.funsuite.AnyFunSuite
import rw.nick.aoc.aoc2023.Dec3.{Address, Cell, CellRow, EmptyCell, FixedNumberCell, NumberCell, NumberGroup}
import rw.nick.aoc.aoc2023.Dec2Part1

class TestDec3 extends AnyFunSuite {

  test("Parse line") {
    val input = "..35..633."
    val expected = CellRow(
      cells = Seq(
        EmptyCell, EmptyCell, NumberCell('3'), NumberCell('5'), EmptyCell, EmptyCell, NumberCell('6'), NumberCell('3'), NumberCell('3'), EmptyCell,
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
        ),
      )
    )
    assert(Dec3.parseLine(input) == expected)
  }

  test("Dec3 Form Groups - middle of line") {
    val input = List(
      FixedNumberCell(NumberCell('1'), Address(12, 0)),
      FixedNumberCell(NumberCell('2'), Address(13, 0)),
      FixedNumberCell(NumberCell('3'), Address(14, 0)),
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
    )
    assert(Dec3.formGroup(20)(input) == expectedGroup)
  }

  test("Dec3 Form Groups - start of line") {
    val input = List(
      FixedNumberCell(NumberCell('1'), Address(0, 0)),
      FixedNumberCell(NumberCell('2'), Address(1, 0)),
      FixedNumberCell(NumberCell('3'), Address(2, 0)),
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
    )
    assert(Dec3.formGroup(20)(input) == expectedGroup)
  }

  test("Dec3 Form Groups - end of line") {
    val input = List(
      FixedNumberCell(NumberCell('1'), Address(12, 0)),
      FixedNumberCell(NumberCell('2'), Address(13, 0)),
      FixedNumberCell(NumberCell('3'), Address(14, 0)),
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
    )
    assert(Dec3.formGroup(15)(input) == expectedGroup)
  }

  test("Dec3 Part 1 Solution") {
    assert(Dec3Part1.solution == 551094)
  }

}
