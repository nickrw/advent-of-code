package rw.nick.aoc

import scala.io.Source

trait Solution(inputPath: String) {
  def input: Iterator[String] = {
    val src = Source.fromFile(inputPath)
    src.getLines()
  }

  def solution: Int
}
