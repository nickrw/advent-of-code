package rw.nick.aoc.aoc2023

import org.scalatest.funsuite.AnyFunSuite

class TestDec1 extends AnyFunSuite {
  test("Day 1 Part 1 - first line") {
    assert(Dec1Part1.sumConcatenatedFirstLastLetterDigitsInString("29lzrxseven") == 29)
  }

  test("Day 1 Part 2") {
    assert(Dec1Part2.sumFirstLastNumberWordOrDigitInString("mbzmtbsvone7xbxvprdcjdonefourbtnkkgnmcr") == 14)
    assert(Dec1Part2.sumFirstLastNumberWordOrDigitInString("htn4tztkrq") == 44)
    assert(Dec1Part2.sumFirstLastNumberWordOrDigitInString("2kksixsix") == 26)
    assert(Dec1Part2.sumFirstLastNumberWordOrDigitInString("44three") == 43)
    assert(Dec1Part2.sumFirstLastNumberWordOrDigitInString("jsthree4p8427") == 37)
    assert(Dec1Part2.sumFirstLastNumberWordOrDigitInString("bb2") == 22)

    assert(Dec1Part2.solution == 53221)
  }
}
