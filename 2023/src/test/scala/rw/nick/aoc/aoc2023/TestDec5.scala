package rw.nick.aoc.aoc2023

import org.scalatest.funsuite.AnyFunSuite
import rw.nick.aoc.Input
import rw.nick.aoc.aoc2023.Dec5.{Almanac, LogContext, Mapper, MapperSet, SequentialMapperSets, Parser, Part1, Part2}

class TestDec5 extends AnyFunSuite {

  test("Test Mapper") {
    val mapper = Mapper(10, 30, 5)
    assert(mapper(1) == None)
    assert(mapper(9) == None)
    assert(mapper(10) == Some(30))
    assert(mapper(14) == Some(34))
    assert(mapper(15) == None)
  }

  test("Test Big Mapper") {
    val mapper = Mapper(BigInt("2211745924"), BigInt("1281207339"), BigInt("39747980"))
    assert(mapper.contains(BigInt("2231619914")))
    assert(!mapper.contains(BigInt("1105872962")))
  }

  test("Test Mapper Contains Range") {
    val mapper = Mapper(100, 1000, 100)
    assert(mapper.containsRange(BigInt("10") to BigInt("20")) == Mapper.RangeMatch.NotContained)
    assert(mapper.containsRange(BigInt("150") to BigInt("190")) == Mapper.RangeMatch.MapperContainsRange)
    assert(mapper.containsRange(BigInt("90") to BigInt("210")) == Mapper.RangeMatch.RangeContainsMapper)
    assert(mapper.containsRange(BigInt("110") to BigInt("220")) == Mapper.RangeMatch.Partial(90))
    assert(mapper.containsRange(BigInt("90") to BigInt("110")) == Mapper.RangeMatch.Partial(10))
  }

  test("Test mapper range splitter") {
    val mapper = Mapper(100, 1000, 100)
    assert(mapper.splitRange(BigInt("10") to BigInt("99")) == Seq(BigInt("10") to BigInt("99")))
    assert(mapper.splitRange(BigInt("10") to BigInt("110")) == Seq(
      BigInt("10") to BigInt("99"),
      BigInt("100") to BigInt("110"),
    ))
    assert(mapper.splitRange(BigInt("110") to BigInt("190")) == Seq(BigInt("110") to BigInt("190")))
    assert(mapper.splitRange(BigInt("110") to BigInt("199")) == Seq(BigInt("110") to BigInt("199")))
    assert(mapper.splitRange(BigInt("190") to BigInt("200")) == Seq(
      BigInt("190") to BigInt("199"),
      BigInt("200") to BigInt("200"),
    ))
    assert(mapper.splitRange(BigInt("190") to BigInt("210")) == Seq(
      BigInt("190") to BigInt("199"),
      BigInt("200") to BigInt("210"),
    ))
    assert(mapper.splitRange(BigInt("210") to BigInt("220")) == Seq(
      BigInt("210") to BigInt("220"),
    ))
    assert(mapper.splitRange(BigInt("10") to BigInt("210")) == Seq(
      BigInt("10") to BigInt("99"),
      BigInt("100") to BigInt("199"),
      BigInt("200") to BigInt("210"),
    ))
  }

  test("Test MapperSet") {
    val mapper = MapperSet(Seq(
      Mapper(10, 30, 5),
      Mapper(50, 70, 5),
    ))
    val ctx = LogContext(0, 1, 1, None, None, None, None)
    assert(mapper(ctx, 1) == 1)
    assert(mapper(ctx, 9) == 9)
    assert(mapper(ctx, 10) == 30)
    assert(mapper(ctx, 14) == 34)
    assert(mapper(ctx, 15) == 15)
    assert(mapper(ctx, 49) == 49)
    assert(mapper(ctx, 50) == 70)
    assert(mapper(ctx, 54) == 74)
    assert(mapper(ctx, 55) == 55)
  }

  test("Test Alamanac") {
    val mapper = Seq(
      Mapper(10, 30, 5),
      Mapper(50, 70, 5),
    )
    val almanac = Almanac(
      seeds = Seq.empty,
      mappers = Map(
        ("seed", "next") -> Seq(
          Mapper(10, 30, 5),
          Mapper(50, 70, 5),
        ),
        ("next", "location") -> Seq(
          Mapper(70, 120, 5),
          Mapper(30, 90, 5),
        ),
      )
    )
    assert(almanac.srcOrdering == Seq("seed", "next", "location"))
    assert(SequentialMapperSets(almanac.orderedMapperSets).transform(LogContext(12, 1, 1, None, None, None, None), 12) == 92)
    assert(SequentialMapperSets(almanac.orderedMapperSets).transform(LogContext(54, 1, 1, None, None, None, None), 54) == 124)
  }

  test("Test parser") {
    val input = Seq(
      "seeds: 3082872446 316680412 2769223903",
      " ",
      "seed-to-soil map:",
      "2211745924 1281207339 39747980",
      "3648083739 2564129012 145170114",
      " ",
      "soil-to-fertilizer map:",
      "2733576308 471599794 76965554",
      "1171423854 1329782324 37554133",
    ).iterator

    val parser = Parser(input)
    assert(parser.parsedLines == parser.parsed(
      state = parser.Complete,
      seeds = Seq(BigInt("3082872446"), BigInt("316680412"), BigInt("2769223903")),
      maps =  Map(
        ("seed", "soil") -> Seq(
          Mapper(BigInt("1281207339"), BigInt("2211745924"), BigInt("39747980")),
          Mapper(BigInt("2564129012"), BigInt("3648083739"), BigInt("145170114")),
        ),
        ("soil", "fertilizer") -> Seq(
          Mapper(BigInt("471599794"), BigInt("2733576308"), BigInt("76965554")),
          Mapper(BigInt("1329782324"), BigInt("1171423854"), BigInt("37554133")),
        ),
      )
    ))
  }

  test("test part 1") {
    assert(Part1.solution("inputs/2023/dec5.txt") == 84470622)
  }

  test("test part 2 from example") {
    val iterator = Input.lineIterator("inputs/2023/dec5_example.txt")
    val almanac = Parser(iterator).parsedLines.toAlmanac
    val orderedMappers = SequentialMapperSets(almanac.orderedMapperSets)
    val ctx = LogContext(82, 0, 0, None, None, None, None)
    assert(orderedMappers.transform(ctx, 82) == 46)
    assert(orderedMappers.transform(ctx, 79) == 82)
    assert(almanac.firstSeeds != List(55, 79).map(BigInt.apply))
    assert(Part2.solution("inputs/2023/dec5_example.txt") == 46)
  }

  test("test part 2 seed range expander") {
    val input = Seq(
      "seeds: 100 100 300 100 600 100",
      " ",
      "seed-to-location map:",
      "1000 100 50",
      "1000 350 50",
    ).iterator

    val almanac = Parser(input).parsedLines.toAlmanac
    assert(almanac.initialSeedRanges == Seq(
      BigInt("100") to BigInt("199"),
      BigInt("300") to BigInt("399"),
      BigInt("600") to BigInt("699"),
    ))
    assert(almanac.firstSeeds.sorted == Seq(100, 150, 300, 350, 600).map(BigInt.apply))
  }

  test("test part 2") {
    assert(Part2.solution("inputs/2023/dec5.txt") == 0)
  }

}
