package fr.sncf.itnovem.adventofcode2024

object Day06 extends AdventOfCode {

  val map: Array[Array[Char]] = input.map(_.toCharArray)

  case class Point(x: Int, y: Int) {
    def move(orientation: String): Point = orientation match {
      case "N" => Point(x, y - 1)
      case "E" => Point(x + 1, y)
      case "S" => Point(x, y + 1)
      case "W" => Point(x - 1, y)
    }
  }

  private def nextOrientation(currentOrientation: String): String = currentOrientation match {
    case "N" => "E"
    case "E" => "S"
    case "S" => "W"
    case "W" => "N"
  }

  private def isOutside(point: Point): Boolean = {
    val maxY: Int = map.length
    val maxX: Int = map.head.length

    point.x < 0 || point.x >= maxX || point.y < 0 || point.y >= maxY
  }

  override def fileName: String = "aoc06.txt"

  override def execute(): (Long, Long) = {

    val initialPosition = map.zipWithIndex
      .map { case (line, i) => (line.indexOf('^'), i)  }
      .filter(_._1 != -1)
      .map { case (x, y) => Point(x, y) }
      .head

    // Star 1
    var isOut = false
    var currentPosition = initialPosition
    var currentOrientation = "N"
    val visitedPositions = scala.collection.mutable.Set[Point](initialPosition)

    while (!isOut) {
      val nextPosition = currentPosition.move(currentOrientation)

      if (isOutside(nextPosition)) {
        isOut = true
      } else {
        val positionValue = map(nextPosition.y)(nextPosition.x)
        if (positionValue == '#') {
          currentOrientation = nextOrientation(currentOrientation)
        } else {
          visitedPositions += nextPosition
          currentPosition = nextPosition
        }
      }
    }

    val star1 = visitedPositions.size

    // Star 2
    var count = 0

    map.indices.map { y =>
      map(y).indices.map { x =>
        val newObstruction = Point(x, y)

        if (map(y)(x) != '#') {
          var isOut = false
          var isLooped = false
          var currentPosition = initialPosition
          var currentOrientation = "N"
          val visitedPositions = scala.collection.mutable.Set[(Point, String)]((initialPosition, currentOrientation))

          while (!isOut && !isLooped) {
            val nextPosition = currentPosition.move(currentOrientation)

            if (isOutside(nextPosition)) {
              isOut = true
            } else if(visitedPositions.contains((nextPosition, currentOrientation))) {
              count += 1
              isLooped = true
            } else {
              val positionValue = map(nextPosition.y)(nextPosition.x)
              if (positionValue == '#' || nextPosition == newObstruction) {
                currentOrientation = nextOrientation(currentOrientation)
              } else {
                val visitedPosition = (nextPosition, currentOrientation)
                visitedPositions += visitedPosition
                currentPosition = nextPosition
              }
            }
          }
        }
      }
    }

    val star2 = count

    (star1, star2)
  }

}
