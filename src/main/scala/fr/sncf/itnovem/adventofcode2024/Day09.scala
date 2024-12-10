package fr.sncf.itnovem.adventofcode2024

/**
 * This is not done.
 * The code works for the example for star 1 but not for the input.
 * Did not start the star 2.
 */
object Day09 extends AdventOfCode {
  override def fileName: String = "aoc09.txt"

  override def execute(): (Long, Long) = {

    val parsedInput = input.head.split("")

    val res = parsedInput
      .zipWithIndex
      .flatMap { case (value, index) =>
        val valueToRepeat = if (index % 2 != 0) "." else (index / 2).toString
        if (value.toInt == 0) Array[String]() else (valueToRepeat * value.toInt).split("")
      }

    val length = res.length
    val numberOfFile = res.count(_ != ".")

    val leftPart = res.slice(0, numberOfFile).zipWithIndex
    val rightPart = res.slice(numberOfFile, length).filter(_ != ".").reverse

    val baseIndex = leftPart.filter(_._1 != ".")
    val moveIndex = rightPart.zip(leftPart.filter(_._1 == ".").map(_._2))

    val longs = (baseIndex ++ moveIndex).map { case (value, index) => value.toLong * index }

    val star1 = longs.sum

    val star2 = 0

    (star1, star2)
  }
}
