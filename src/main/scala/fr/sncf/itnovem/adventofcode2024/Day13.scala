package fr.sncf.itnovem.adventofcode2024

import scala.math.{max, min}

object Day13 extends AdventOfCode {

  case class Button(X: Long, Y: Long)

  case class Instructions(buttonA: Button, buttonB: Button, prize: Button)

  private def extractButtonValues(input: String): Button = {
    val pattern = """[-+]?\d+""".r
    val numbers = pattern.findAllIn(input).map(_.toInt).toList
    Button(numbers.head, numbers(1))
  }

  private def findCombinations(a: Long, b: Long, c: Long, range: Range): Seq[(Int, Int)] = {
    for {
      m <- range
      n <- range
      if a * m + b * n == c
    } yield (m, n)
  }

  val parsedInput: Array[Instructions] = input
    .grouped(4)
    .map { instructions =>
      Instructions(
        extractButtonValues(instructions(0)),
        extractButtonValues(instructions(1)),
        extractButtonValues(instructions(2))
      )
    }.toArray

  override def fileName: String = "aoc13.txt"
  override def execute(): (Long, Long) = {

    val star1 = parsedInput.map { ins =>
      val xCombinations = findCombinations(ins.buttonA.X, ins.buttonB.X, ins.prize.X, 1 to (ins.prize.X / min(ins.buttonA.X, ins.buttonA.Y) + max(ins.buttonA.X, ins.buttonA.Y)).toInt)

      xCombinations
        .filter { case (m, n) => m * ins.buttonA.Y + n * ins.buttonB.Y == ins.prize.Y }
        .map { case (m, n) => 3*m + n }
        .sorted
        .headOption
        .getOrElse(0)
    }.sum

    val star2 = 0

    (star1, star2)
  }
}
