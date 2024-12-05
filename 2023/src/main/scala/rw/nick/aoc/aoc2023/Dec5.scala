package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.numberOrSpace
import rw.nick.aoc.aoc2023.Dec5.Mapper.RangeMatch
import rw.nick.aoc.{BigIntSolution, Solution}

import scala.collection.immutable.NumericRange
import scala.util.matching.Regex


object Dec5 {

  class Parser(lines: Iterator[String]) {

    trait ParserState
    case object Initial extends ParserState
    case class MapBlock(src: String, dst: String) extends ParserState
    case object Complete extends ParserState

    case class parsed(state: ParserState, seeds: Seq[BigInt], maps: Map[(String, String), Seq[Mapper]]) {
      def toAlmanac: Almanac =
        state match {
          case Complete => Almanac(seeds = seeds, mappers = maps)
          case _ => throw new Exception("Can't convert parsed to Almanac until parsing is complete")
        }
    }
    object parsed {
      val empty = parsed(Initial, Seq.empty, Map.empty)
    }

    def parseSeeds(line: String): Seq[BigInt] =
      line.strip().filter(numberOrSpace).split(" ").filter(_.nonEmpty).map(BigInt.apply)

    val mapNameRegex: Regex = "^([a-z]+)-to-([a-z]+) map:".r
    val mapNumbersRegex: Regex = """^([0-9]+)\s+([0-9]+)\s+([0-9]+)$""".r

    val parsedLines = lines.foldLeft(parsed.empty) { case (coll: parsed, fullLine: String) =>
      (fullLine.strip(), coll.state) match {
        case ("", _) => coll
        case (line, Initial) if line.startsWith("seeds: ") => coll.copy(seeds = parseSeeds(line))
        case (mapNameRegex(srcName, dstName), _) =>
          val newState = MapBlock(srcName, dstName)
          coll.copy(state = newState)
        case (mapNumbersRegex(dstNumStart, srcNumStart, rangeLen), MapBlock(srcName, dstName)) =>
          val newMaps = coll.maps + ((srcName, dstName) -> (coll.maps.getOrElse((srcName, dstName), Seq.empty) :+ Mapper(
            srcStart = BigInt(srcNumStart),
            dstStart = BigInt(dstNumStart),
            length = BigInt(rangeLen),
          )))
          coll.copy(maps = newMaps)
        case (line, _) =>
          throw new Exception(s"Failed to parse line: $line")
      }
    }.copy(state = Complete)
  }

  case class LogContext(seedValue: BigInt, seedIndex: Int, seedMaxIndex: Int, mapperSetIndex: Option[Int], mapperSetMaxIndex: Option[Int], mapperIndex: Option[Int], mapperMaxIndex: Option[Int]) {
    private lazy val prefix = s"Working seed [$seedIndex/$seedMaxIndex] $seedValue"
    private lazy val mapperSetPart: Option[String] = for {
      msi <- mapperSetIndex
      msmi <- mapperSetMaxIndex
    } yield s" MapperSet [$msi/$msmi]"
    private lazy val mapperPart: Option[String] = for {
      mi <- mapperIndex
      mmi <- mapperMaxIndex
    } yield s" Mapper [$mi/$mmi]"
    lazy val logLine = s"Seed [$seedIndex/$seedMaxIndex] $seedValue${mapperSetPart.getOrElse("")}${mapperPart.getOrElse("")}"
    def log(msg: String) = println(s"$logLine - $msg")
  }


  object Mapper {
    trait RangeMatch
    object RangeMatch {
      case object NotContained extends RangeMatch
      case object MapperContainsRange extends RangeMatch
      case object RangeContainsMapper extends RangeMatch
      case class Partial(splitRangeAt: Int) extends RangeMatch
    }
  }

  case class Mapper(srcStart: BigInt, dstStart: BigInt, length: BigInt) {
    import Mapper._
    val srcRange = srcStart to srcStart + (length-1)
    val dstRange = dstStart to dstStart + (length-1)
    def contains(value: BigInt): Boolean = srcRange.contains(value)
    def containsRange(range: NumericRange.Inclusive[BigInt]): RangeMatch =
      if (srcRange.contains(range.start) && srcRange.contains(range.end))
        RangeMatch.MapperContainsRange
      else if (range.contains(srcRange.start) && range.contains(srcRange.end))
        RangeMatch.RangeContainsMapper
      else if (srcRange.contains(range.start) && !srcRange.contains(range.end))
        /*
              | Mapper (srcRange)   |
                   <------  x  -----> // srcRange.length - y
              <-y->|    Range (range)      | // y = range.start - srcRange.start
         */
        RangeMatch.Partial((srcRange.length - (range.start - srcRange.start).toInt))
      else if (!srcRange.contains(range.start) && srcRange.contains(range.end))
        /*
                             |  Mapper (srcRange)    |
                    <--- x --> // x = srcRange.start - range.start
                    |    Range (range)    |
         */
        RangeMatch.Partial((srcRange.start - range.start).toInt)
      else
        RangeMatch.NotContained

    def splitRange(candidateRange: NumericRange.Inclusive[BigInt]): Seq[NumericRange.Inclusive[BigInt]] = {
      containsRange(candidateRange) match
        case RangeMatch.Partial(splitI) =>
          val splits = candidateRange.splitAt(splitI)
          splits match {
            case (left: NumericRange.Inclusive[BigInt], right: NumericRange.Inclusive[BigInt]) => Seq(left, right)
            case _ => throw new Exception("Expected NumericRange.Inclusive[BigInt]")
          }
        case RangeMatch.RangeContainsMapper =>
          val leftSplits = candidateRange.splitAt((srcStart - candidateRange.start).toInt)
          val rightSplits = leftSplits._2.splitAt(length.toInt)
          val result = Seq(leftSplits._1, rightSplits._1, rightSplits._2)
            .filterNot(_.isEmpty)
          result match {
            case r: Seq[NumericRange.Inclusive[BigInt]] => r
            case _ => throw new Exception("expected NumericRange.Inclusive[BigInt]")
          }
        case _ => Seq(candidateRange)
    }

    def apply(value: BigInt): Option[BigInt] = {
      if (contains(value)) {
        Some((value - srcStart) + dstStart)
      } else {
        None
      }
    }
  }

  case class MapperSet(mappers: Seq[Mapper]) {
    def apply(ctx: LogContext, initialValue: BigInt): BigInt = mappers.zipWithIndex.foldLeft((initialValue, false)) { case ((coll: BigInt, shortCircuit: Boolean), (mapper: Mapper, i: Int)) =>
      val mapperCtx = ctx.copy(mapperIndex = Some(i), mapperMaxIndex = Some(mappers.length))
      if (shortCircuit) {
        mapperCtx.log("Short Circuited")
        (coll, shortCircuit)
      } else {
        mapper(coll) match {
          case Some(v) =>
            mapperCtx.log(s"Transformed: $coll -> $v")
            (v, true)
          case None =>
            mapperCtx.log(s"Value $coll not in range")
            (coll, false)
        }
      }
    }._1
  }

  class SequentialMapperSets(mapperSets: Seq[MapperSet]) {
    def transform(ctx: LogContext, initialValue: BigInt): BigInt = {
      mapperSets.zipWithIndex.foldLeft(initialValue) { case (value: BigInt, (transformer: MapperSet, i: Int)) =>
        val mapperSetCtx = ctx.copy(mapperSetIndex = Some(i), mapperSetMaxIndex = Some(mapperSets.length))
        transformer(mapperSetCtx, value)
      }
    }
    lazy val truncatedMappers: MapperSet = {
      MapperSet(
        mapperSets.head.mappers
        .sortBy(_.srcStart)
        .map { mapperFromHead =>
          val start = mapperFromHead.srcStart
          val end = mapperFromHead.srcRange.end
          val ctx = LogContext(0, 0, 0, None, None, None, None)
          val newDstStart = transform(ctx, start)
          Mapper(start, newDstStart, mapperFromHead.length)
        }
      )
    }
  }

  class Almanac(seeds: Seq[BigInt], mappers: Map[(String, String), Seq[Mapper]]) {
    protected lazy val srcMap: Map[String, MapperSet] = mappers.map(_._1 -> MapperSet(_)) // assumes that source is always unique
    lazy val srcOrdering: Seq[String] = {
      val links = mappers.keys.toMap
      val destinations = links.values.toSet
      val headName = links.keys.filterNot(destinations.contains).headOption
      def linker(name: Option[String]): Seq[String] = {
        name match {
          case Some(n) => Seq(n) ++ linker(links.get(n))
          case None => Seq.empty
        }
      }
      linker(headName)
    }
    lazy val orderedMapperSets: Seq[MapperSet] = srcOrdering.init.map(srcMap.apply)
    lazy val sequentialMapperSets = SequentialMapperSets(orderedMapperSets)

    def seedLocations: Seq[BigInt] = seeds.zipWithIndex.map { case (seed, i) =>
      val ctx = LogContext(seed, i, seeds.length, Some(1), Some(1), None, None)
      sequentialMapperSets.truncatedMappers(ctx, seed)
    }

    lazy val initialSeedRanges: Seq[NumericRange.Inclusive[BigInt]] = seeds
      .grouped(2)
      .map(s => s.head to s.head + (s.last-1))
      .toSeq
      .sortBy(_.start)

    lazy val seedRanges: Seq[NumericRange.Inclusive[BigInt]] = initialSeedRanges.foldLeft(Seq.empty) { case (coll: Seq[NumericRange.Inclusive[BigInt]], seedRange) =>
      val newRanges: Seq[NumericRange.Inclusive[BigInt]] = sequentialMapperSets.truncatedMappers.mappers.flatMap(_.splitRange(seedRange))
      coll ++ newRanges
    }

    lazy val firstSeeds: Seq[BigInt] = seedRanges.map(_.start).toSet.toSeq

    def firstSeedLocations: Seq[BigInt] = firstSeeds.zipWithIndex.map { case (seed, i) =>
      val ctx = LogContext(seed, i, firstSeeds.length, Some(1), Some(1), None, None)
      sequentialMapperSets.transform(ctx, seed)
    }
  }

  object Part1 extends BigIntSolution {
    override def solution(input: Iterator[String]): BigInt = {
      val almanac = Parser(input).parsedLines.toAlmanac
      almanac.seedLocations.min
    }
  }

  object Part2 extends BigIntSolution {
    override def solution(input: Iterator[String]): BigInt = {
      val almanac = Parser(input).parsedLines.toAlmanac
      almanac.firstSeedLocations.min
    }
  }

}
