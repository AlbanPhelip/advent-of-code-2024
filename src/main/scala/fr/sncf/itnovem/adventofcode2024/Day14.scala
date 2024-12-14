package fr.sncf.itnovem.adventofcode2024

object Day14 extends AdventOfCode {

  override def fileName: String = "aoc14.txt"

  private val wide = 101
  private val tall = 103

  case class Point(x: Long, y: Long) {
    def +(other: Point): Point = {
      Point(x + other.x, y + other.y)
    }

    def *(i: Long): Point = {
      Point(x * i, y * i)
    }

    def modulo(): Point = {
      Point((wide + (x % wide)) % wide, (tall + (y % tall)) % tall)
    }
  }

  private def extractPointValues(input: String): Point = {
    val pattern = """[-+]?\d+""".r
    val numbers = pattern.findAllIn(input).map(_.toInt).toList
    Point(numbers.head, numbers(1))
  }

  override def execute(): (Long, Long) = {
    val iterations = 100

    val parsedInput = input
      .map(_.split(" "))
      .map(x => (extractPointValues(x.head), extractPointValues(x(1))))

    val newPositions = parsedInput
      .map { case (position, velocity) => (position + (velocity * iterations)).modulo() }

    val star1 = newPositions.count(p => p.x < wide / 2 && p.y < tall / 2) *
      newPositions.count(p => p.x > wide /2 && p.y < tall / 2) *
      newPositions.count(p => p.x < wide /2 && p.y > tall / 2) *
      newPositions.count(p => p.x > wide /2 && p.y > tall / 2)

    val star2 = 0

    (star1, star2)
  }
}
