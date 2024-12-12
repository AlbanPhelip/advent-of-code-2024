package fr.sncf.itnovem.adventofcode2024

import scala.collection.mutable
import scala.util.{Success, Try}

object Day12 extends AdventOfCode {

  val parsedInput: Array[Array[String]] = input.map(_.split(""))

  case class Point(x: Int, y: Int) {
    def value: Try[String] = Try(parsedInput(y)(x))

    def up(): Point = Point(x, y - 1)
    def down(): Point = Point(x, y + 1)
    def left(): Point = Point(x - 1, y)
    def right(): Point = Point(x + 1, y)
    private def upRight(): Point = Point(x + 1, y - 1)
    private def upLeft(): Point = Point(x - 1, y - 1)
    private def downRight(): Point = Point(x + 1, y + 1)
    private def downLeft(): Point = Point(x - 1, y + 1)

    def isOutsideCornerUpLeft: Boolean = Try(parsedInput(up().y)(up().x)) != value && Try(parsedInput(left().y)(left().x)) != value
    def isOutsideCornerDownLeft: Boolean = Try(parsedInput(down().y)(down().x)) != value && Try(parsedInput(left().y)(left().x)) != value
    def isOutsideCornerUpRight: Boolean = Try(parsedInput(up().y)(up().x)) != value && Try(parsedInput(right().y)(right().x)) != value
    def isOutsideCornerDownRight: Boolean = Try(parsedInput(down().y)(down().x)) != value && Try(parsedInput(right().y)(right().x)) != value
    def isInsideCornerUpLeft: Boolean = Try(parsedInput(up().y)(up().x)) == value && Try(parsedInput(left().y)(left().x)) == value && Try(parsedInput(upLeft().y)(upLeft().x)) != value
    def isInsideCornerDownLeft: Boolean = Try(parsedInput(down().y)(down().x)) == value && Try(parsedInput(left().y)(left().x)) == value && Try(parsedInput(downLeft().y)(downLeft().x)) != value
    def isInsideCornerUpRight: Boolean = Try(parsedInput(up().y)(up().x)) == value && Try(parsedInput(right().y)(right().x)) == value && Try(parsedInput(upRight().y)(upRight().x)) != value
    def isInsideCornerDownRight: Boolean = Try(parsedInput(down().y)(down().x)) == value && Try(parsedInput(right().y)(right().x)) == value && Try(parsedInput(downRight().y)(downRight().x)) != value
  }
  override def fileName: String = "aoc12.txt"

  private def findArea(value: String, point: Point, area: Set[Point]): Set[Point] = {
    val upPoint = point.up()
    val downPoint = point.down()
    val leftPoint = point.left()
    val rightPoint = point.right()

    val upValue = Try(parsedInput(upPoint.y)(upPoint.x))
    val downValue = Try(parsedInput(downPoint.y)(downPoint.x))
    val leftValue = Try(parsedInput(leftPoint.y)(leftPoint.x))
    val rightValue = Try(parsedInput(rightPoint.y)(rightPoint.x))

    val up = findNewSpots(upValue, value, upPoint, area)
    val down = findNewSpots(downValue, value, downPoint, up)
    val left = findNewSpots(leftValue, value, leftPoint, down)
    val right = findNewSpots(rightValue, value, rightPoint, left)

    up ++ down ++ left ++ right
  }

  private def findNewSpots(newValue: Try[String], value: String, newPoint: Point, area: Set[Point]): Set[Point] = {
    if (newValue == Success(value) && !area.contains(newPoint)) {
      findArea(value, newPoint, area + newPoint)
    } else {
      area
    }
  }
  private def computeCostStar1(areas: Array[Set[Point]]): Long = {
    areas.map { area =>
      val surface = area.size

      val perimeter = area.toArray.map { point =>
        val value = parsedInput(point.y)(point.x)

        Array(
          Try(parsedInput(point.up().y)(point.up().x)) != Success(value),
          Try(parsedInput(point.down().y)(point.down().x)) != Success(value),
          Try(parsedInput(point.left().y)(point.left().x)) != Success(value),
          Try(parsedInput(point.right().y)(point.right().x)) != Success(value),
        ).count(_ == true)
      }.sum

      surface.toLong * perimeter
    }.sum
  }

  private def computeCostStar2(areas: Array[Set[Point]]): Long = {
    areas.map { area =>
      val surface = area.size

      val numberOfCorners = area.toArray.map { point =>
        Array(
          point.isOutsideCornerUpLeft,
          point.isOutsideCornerDownLeft,
          point.isOutsideCornerUpRight,
          point.isOutsideCornerDownRight,
          point.isInsideCornerUpLeft,
          point.isInsideCornerDownLeft,
          point.isInsideCornerUpRight,
          point.isInsideCornerDownRight
        ).count(_ == true)
      }.sum

      surface.toLong * numberOfCorners
    }.sum
  }

  override def execute(): (Long, Long) = {

    val areas = mutable.ArrayBuffer.empty[Set[Point]]

    parsedInput.indices.map { y =>
      parsedInput(y).indices.map { x =>
        if (!areas.toArray.flatMap(_.toArray).contains(Point(x, y))) {
          val initialArea: Set[Point] = Set(Point(x, y))
          areas += findArea(parsedInput(y)(x), Point(x, y), initialArea)
        }
      }
    }

    val star1 = computeCostStar1(areas.toArray)

    val star2 = computeCostStar2(areas.toArray)

    (star1, star2)
  }
}
