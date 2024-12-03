package fr.sncf.itnovem.adventofcode2024

object Day03 extends AdventOfCode {

  private val patternStar1 = """mul\((\d{1,3}),(\d{1,3})\)""".r
  private val patternStar2 = """mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)""".r

  private def computeMul(sumInstruction: String): Int = {
    sumInstruction match {
      case patternStar1(x, y) => x.toInt * y.toInt
    }
  }

  override def fileName: String = "aoc03.txt"

  override def execute(): (Long, Long) = {
    val star1 = input
      .flatMap(patternStar1.findAllIn)
      .map(computeMul)
      .sum

    val star2 = input
      .flatMap(patternStar2.findAllIn)
      .foldLeft((true, 0)) { case ((isEnabled, count), instruction) => (instruction, isEnabled) match {
        case ("do()", _)    => (true, count)
        case ("don't()", _) => (false, count)
        case (_, false)     => (isEnabled, count)
        case (_, true)      => (isEnabled, count + computeMul(instruction))
      }}._2

    (star1, star2)
  }
}
