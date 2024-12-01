package fr.sncf.itnovem.adventofcode2024

import scala.math.abs

object Day01 extends AdventOfCode {
  override def fileName: String = "aoc01.txt"
  override def execute(): (Any, Any) = {
    val parsedInput = input
      .map(_.split(" {3}"))
      .map(x => (x(0).toInt, x(1).toInt))

    val left = parsedInput.map(_._1).sorted
    val right = parsedInput.map(_._2).sorted

    val star1 = left.zip(right).map { case (l, r) => abs(l - r) }.sum

    val star2 = left.map(x => right.count(_ == x) * x).sum

    (star1, star2)
  }
}
