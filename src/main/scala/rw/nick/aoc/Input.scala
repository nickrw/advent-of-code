package rw.nick.aoc

import scala.io.Source

object Input {
  def lineIterator(filePath: String): Iterator[String] = {
    val src = Source.fromFile(filePath)
    src.getLines()
  }
}
