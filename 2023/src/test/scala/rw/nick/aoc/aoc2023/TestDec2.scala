package rw.nick.aoc.aoc2023

import org.scalatest.funsuite.AnyFunSuite

class TestDec2 extends AnyFunSuite {
  val Colour = Dec2.Colour
  val Red = Colour.Red
  val Green = Colour.Green
  val Blue = Colour.Blue
  val Game = Dec2.Game

  test("Test Day 2 single selection parser") {
    assert(Dec2.singleSelectionParser("7 blue") == Map { Colour.Blue -> 7 } )
  }

  test("Test Day 2 line parser") {
    val input: String = "Game 1: 2 red, 8 blue, 1 green; 9 green, 7 blue, 3 red"
    val expected = Game(1, List(
      Map(
        Red -> 2,
        Blue -> 8,
        Green -> 1,
      ),
      Map(
        Red -> 3,
        Blue -> 7,
        Green -> 9,
      ),
    ))
    assert(Game(input) == expected)
  }

  test("Day 2 game is possible") {
    val game = Game(1, List(
      Map(
        Red -> 2,
        Blue -> 8,
        Green -> 1,
      ),
      Map(
        Red -> 3,
        Blue -> 7,
        Green -> 9,
      ),
    ))
    val isPossible = game.isPossibleWithMaxAvailable(
      Map(
        Red -> 12,
        Blue -> 14,
        Green -> 13,
      ),
    )
    assert(isPossible)
    val isNotPossible = game.isPossibleWithMaxAvailable(
      Map(
        Red -> 12,
        Blue -> 14,
        Green -> 8,
      ),
    )
    assert(!isNotPossible)
  }

  test("Day 2 part 1 result") {
    assert(Dec2Part1.solution("inputs/2023/dec2.txt") == 2162)
  }

  test("Day 2 part 2 minimum requirements") {
    val game = Game(1, List(
      Map(
        Red -> 2,
        Blue -> 8,
        Green -> 1,
      ),
      Map(
        Red -> 12,
        Blue -> 7,
        Green -> 9,
      ),
      Map(
        Red -> 3,
        Blue -> 7,
        Green -> 9,
      ),
    ))
    val expectedMinimumRequirements = Map(
      Red -> 12,
      Blue -> 8,
      Green -> 9,
    )
    assert(game.minimumRequirements == expectedMinimumRequirements)
    assert(game.power == 12 * 8 * 9)
  }

  test("Day 2 Part 2 solution") {
    assert(Dec2Part2.solution("inputs/2023/dec2.txt") == 72513)
  }
}
