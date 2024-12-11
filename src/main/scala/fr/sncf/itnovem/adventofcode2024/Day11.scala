package fr.sncf.itnovem.adventofcode2024

/**
 * I kept the implementation I did for star 1 even though it could have been done with the star 2 implementation
 */
object Day11 extends AdventOfCode {

  case class Stone(value: String, count: Long)

  private def splitStoneStar1(stone: String): Array[String] = {
    if (stone == "0") Array("1")
    else if (stone.length % 2 == 0) Array(stone.take(stone.length / 2), stone.takeRight(stone.length / 2).toLong.toString)
    else Array((stone.toLong * 2024).toString)
  }

  private def splitStoneStar2(stone: Stone): Array[Stone] = {
    val stoneValue = stone.value
    val newStoneValues = if (stoneValue == "0") Array("1")
      else if (stoneValue.length % 2 == 0) Array(stoneValue.take(stoneValue.length / 2), stoneValue.takeRight(stoneValue.length / 2).toLong.toString)
      else Array((stoneValue.toLong * 2024).toString)

    newStoneValues.map(newStoneValue => stone.copy(value = newStoneValue))
  }

  private def reduceStones(stones: Array[Stone]): Array[Stone] = {
    stones
      .groupBy(_.value)
      .map { case (value, stonesList) => Stone(value, stonesList.map(_.count).sum)}
      .toArray
  }

  override def fileName: String = "aoc11.txt"

  override def execute(): (Long, Long) = {

    val parsedInput = input.head.split(" ")

    val star1 = (1 to 25).foldLeft(parsedInput) { case (stones, _) =>
      stones.flatMap(splitStoneStar1)
    }.length

    val star2 = (1 to 75).foldLeft(parsedInput.map(Stone(_, 1L))) { case (stones, _) =>
      reduceStones(stones.flatMap(splitStoneStar2))
    }.map(_.count).sum

    (star1, star2)
  }
}
