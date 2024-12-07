package fr.sncf.itnovem.adventofcode2024

object Day07 extends AdventOfCode {

  private def computeNumbersStar1(numbers: List[Long]): List[Long] = {
    def helper(current: Long, rest: List[Long]): List[Long] = {
      if (rest.isEmpty) List(current)
      else {
        val next = rest.head
        val addResult = helper(current + next, rest.tail)
        val multiplyResult = helper(current * next, rest.tail)
        addResult ++ multiplyResult
      }
    }

    helper(numbers.head, numbers.tail)
  }

  private def computeNumbersStar2(numbers: List[Long]): List[Long] = {
    def helper(current: Long, rest: List[Long]): List[Long] = {
      if (rest.isEmpty) List(current)
      else {
        val next = rest.head
        val addResult = helper(current + next, rest.tail)
        val multiplyResult = helper(current * next, rest.tail)
        val concatResult = helper((current.toString + next.toString).toLong, rest.tail)
        addResult ++ multiplyResult ++ concatResult
      }
    }

    helper(numbers.head, numbers.tail)
  }

  override def fileName: String = "aoc07.txt"
  override def execute(): (Long, Long) = {

    val parsedInput = input
      .map(_.split(":"))
      .map(l => (l.head.toLong, l(1).trim.split(" ").map(_.toLong).toList))

    val star1 = parsedInput
      .filter { case (finalValues, numbers) => computeNumbersStar1(numbers).contains(finalValues)}
      .map(_._1)
      .sum

    val star2 = parsedInput
      .filter { case (finalValues, numbers) => computeNumbersStar2(numbers).contains(finalValues) }
      .map(_._1)
      .sum

    (star1, star2)
  }
}
