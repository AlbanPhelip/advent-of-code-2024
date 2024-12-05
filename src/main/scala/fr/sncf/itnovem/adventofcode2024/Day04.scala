package fr.sncf.itnovem.adventofcode2024

import scala.util.{Success, Try}

object Day04 extends AdventOfCode {

  override def fileName: String = "aoc04.txt"

  override def execute(): (Long, Long) = {
    val data: Array[Array[String]] = input.map(_.split(""))

    val star1 = data.indices.map { y =>
      data(y).indices.map { x =>
        if (data(y)(x) == "X") {
          Array(
            Try(data(y - 1)(x) + data(y - 2)(x) + data(y - 3)(x)),
            Try(data(y + 1)(x) + data(y + 2)(x) + data(y + 3)(x)),
            Try(data(y)(x - 1) + data(y)(x - 2) + data(y)(x - 3)),
            Try(data(y)(x + 1) + data(y)(x + 2) + data(y)(x + 3)),
            Try(data(y + 1)(x + 1) + data(y + 2)(x + 2) + data(y + 3)(x + 3)),
            Try(data(y - 1)(x + 1) + data(y - 2)(x + 2) + data(y - 3)(x + 3)),
            Try(data(y - 1)(x - 1) + data(y - 2)(x - 2) + data(y - 3)(x - 3)),
            Try(data(y + 1)(x - 1) + data(y + 2)(x - 2) + data(y + 3)(x - 3))
          ).count(_ == Success("MAS"))
        } else 0
      }.sum
    }.sum

    val star2 = data.indices.map { y =>
      data(y).indices.count { x =>
        (data(y)(x) == "A") && {
          val diagonalNorthEast = Try(Array(data(y - 1)(x + 1), data(y + 1)(x - 1))).map(_.sorted.mkString(""))
          val diagonalSouthEast = Try(Array(data(y - 1)(x - 1), data(y + 1)(x + 1))).map(_.sorted.mkString(""))

          diagonalNorthEast == Success("MS") && diagonalSouthEast == Success("MS")
        }
      }
    }.sum

    (star1, star2)
  }
}
