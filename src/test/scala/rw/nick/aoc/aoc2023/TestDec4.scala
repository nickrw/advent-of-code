package rw.nick.aoc.aoc2023

import org.scalatest.funsuite.AnyFunSuite
import rw.nick.aoc.aoc2023.Dec4.{Card, parseLine}

class TestDec4 extends AnyFunSuite {

  test("Parse line") {
    val input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    val expected = Card(
      id = 1,
      winningNumbers = Set(41, 48, 83, 86, 17),
      myNumbers = Set(83, 86, 6, 31, 17, 9, 48, 53),
    )
    assert(parseLine(input) == expected)
  }

  test("Points - 8") {
    val input = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
    val expected = 8
    assert(parseLine(input).points == expected)
  }

  test("Points - 16") {
    val input = "Card 1: 41 48 83 86 17 | 83 86 41 6 31 17  9 48 53"
    val expected = 16
    assert(parseLine(input).points == expected)
  }

  test("Points - 1") {
    val input = "Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83"
    val expected = 1
    assert(parseLine(input).points == expected)
  }

  test("Points - 0") {
    val input = "Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36"
    val expected = 0
    assert(parseLine(input).points == expected)
  }

  test("Part 1 Solution") {
    assert(Dec4Part1.solution == 28538)
  }
  
  test("Part 2 Solution") {
    assert(Dec4Part2.solution == 9425061)
  }

}
