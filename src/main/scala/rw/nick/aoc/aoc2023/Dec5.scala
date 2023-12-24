package rw.nick.aoc.aoc2023

import rw.nick.aoc.ParserUtils.numberOrSpace
import rw.nick.aoc.{BigIntSolution, Solution}

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
          throw new Exception(s"Failed to parse line: {line}")
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

  case class Mapper(srcStart: BigInt, dstStart: BigInt, length: BigInt) {
    val srcRange = srcStart to srcStart + (length-1)
    val dstRange = dstStart to dstStart + (length-1)
    def contains(value: BigInt): Boolean = srcRange.contains(value)
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
    lazy val orderedMappers: Seq[MapperSet] = srcOrdering.init.map(srcMap.apply)
    def transform(ctx: LogContext, initialValue: BigInt): BigInt = {
      orderedMappers.zipWithIndex.foldLeft(initialValue) { case (value: BigInt, (transformer: MapperSet, i: Int)) =>
        val mapperSetCtx = ctx.copy(mapperSetIndex = Some(i), mapperSetMaxIndex = Some(orderedMappers.length))
        transformer(mapperSetCtx, value)
      }
    }
    def seedLocations: Seq[BigInt] = seeds.zipWithIndex.map { case (seed, i) =>
      val ctx = LogContext(seed, i, seeds.length, None, None, None, None)
      transform(ctx, seed)
    }
  }

  object Part1 extends BigIntSolution {
    override def solution(input: Iterator[String]): BigInt = {
      val almanac = Parser(input).parsedLines.toAlmanac
      almanac.seedLocations.min.toInt
    }
  }

}
