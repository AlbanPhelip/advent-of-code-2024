package fr.sncf.itnovem.adventofcode2024

import scala.util.{Success, Try}

object Day10 extends AdventOfCode {

  val parsedInput: Array[Array[Int]] = input.map(_.split("").map(_.toInt))

  case class Point(x: Int, y: Int) {
    def up(): Point = Point(x, y - 1)
    def down(): Point = Point(x, y + 1)
    def left(): Point = Point(x - 1, y)
    def right(): Point = Point(x + 1, y)
  }

  private def findStar1(count: Set[Point], value: Int, coord: Point): Set[Point] = {
    val northCoord = coord.up()
    val southCoord = coord.down()
    val eastCoord = coord.left()
    val westCoord = coord.right()

    val northValue = Try(parsedInput(northCoord.y)(northCoord.x))
    val southValue = Try(parsedInput(southCoord.y)(southCoord.x))
    val eastValue = Try(parsedInput(eastCoord.y)(eastCoord.x))
    val westValue = Try(parsedInput(westCoord.y)(westCoord.x))

    handleNewPointStar1(northValue, value, count, northCoord) ++
    handleNewPointStar1(southValue, value, count, southCoord) ++
    handleNewPointStar1(eastValue, value, count, eastCoord) ++
    handleNewPointStar1(westValue, value, count, westCoord)
  }

  private def handleNewPointStar1(newValue: Try[Int], value: Int, count: Set[Point], newCoord: Point): Set[Point] = {
    if (newValue == Success(value + 1)) {
      if (newValue == Success(9)) count + newCoord
      else findStar1(count, value + 1, newCoord)
    }
    else count
  }

  private def findStar2(count: Int, value: Int, coord: Point): Int = {
    val northCoord = coord.up()
    val southCoord = coord.down()
    val eastCoord = coord.left()
    val westCoord = coord.right()

    val northValue = Try(parsedInput(northCoord.y)(northCoord.x))
    val southValue = Try(parsedInput(southCoord.y)(southCoord.x))
    val eastValue = Try(parsedInput(eastCoord.y)(eastCoord.x))
    val westValue = Try(parsedInput(westCoord.y)(westCoord.x))

    handleNewPointStar2(northValue, value, count, northCoord) +
      handleNewPointStar2(southValue, value, count, southCoord) +
      handleNewPointStar2(eastValue, value, count, eastCoord) +
      handleNewPointStar2(westValue, value, count, westCoord)
  }
  private def handleNewPointStar2(newValue: Try[Int], value: Int, count: Int, newCoord: Point): Int = {
    if (newValue == Success(value + 1)) {
      if (newValue == Success(9)) count + 1
      else findStar2(count, value + 1, newCoord)
    }
    else count
  }

  override def fileName: String = "aoc10.txt"
  override def execute(): (Long, Long) = {

    val zeros = parsedInput
      .zipWithIndex
      .map { case (v, i) => (v.zipWithIndex.filter(_._1 == 0).map(_._2), i)}
      .filter(_._1.nonEmpty)
      .flatMap { case (line, y) => line.map(x => (x, y))}

    val star1 = zeros
      .map { case (x, y) => findStar1(Set[Point](), 0, Point(x, y)).size }
      .sum

    val star2 = zeros
      .map { case (x, y) => findStar2(0, 0, Point(x, y)) }
      .sum

    (star1, star2)
  }
}
