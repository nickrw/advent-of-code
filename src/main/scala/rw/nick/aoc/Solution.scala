package rw.nick.aoc

import scala.io.Source

trait Solution[Input, Output] {
  def solution(input: Input): Output
}

trait StringIteratorInputSolution[Output] extends Solution[Iterator[String], Output] {
  def solution(inputfilePath: String): Output = solution(Input.lineIterator(inputfilePath))
}

trait IntSolution extends StringIteratorInputSolution[Int]
trait BigIntSolution extends StringIteratorInputSolution[BigInt]
