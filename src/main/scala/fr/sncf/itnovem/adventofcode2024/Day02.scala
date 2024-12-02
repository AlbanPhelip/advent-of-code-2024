package fr.sncf.itnovem.adventofcode2024

import scala.math.abs

object Day02 extends AdventOfCode {

  private def isReportValidStar1(report: Array[Int]): Boolean = {
    val levelsDiff = report.zip(report.tail).map { case (x, y) => y - x }

    val levelsDifferByAtLeast1AndAtMost3 = levelsDiff.map(abs).count(x => x >= 1 && x <= 3) == levelsDiff.length
    val levelsAreAllIncreasingOrAllDecreasing = levelsDiff.forall(_ > 0) || levelsDiff.forall(_ < 0)

    levelsDifferByAtLeast1AndAtMost3 && levelsAreAllIncreasingOrAllDecreasing
  }

  private def isReportValidStar2(report: Array[Int]): Boolean = {
    report.indices
      .map(i => report.zipWithIndex.filter(_._2 != i).map(_._1))
      .map(isReportValidStar1)
      .reduce(_ || _)
  }

  override def fileName: String = "aoc02.txt"

  override def execute(): (Long, Long) = {
    val parsedInput = input.map(_.split(" ").map(_.toInt))

    val star1 = parsedInput.count(isReportValidStar1)
    val star2 = parsedInput.count(isReportValidStar2)

    (star1, star2)
  }
}
