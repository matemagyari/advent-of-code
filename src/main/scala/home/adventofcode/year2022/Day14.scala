package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.collection.mutable

object Day14 extends App {

  case class Position(x: Int, y: Int) {
    def down: Position = copy(x, y + 1)
    def downAndLeft: Position = copy(x - 1, y + 1)
    def downAndRight: Position = copy(x + 1, y + 1)
  }

  sealed trait Thing

  case object Sand extends Thing

  case object Rock extends Thing

  def parse(text: String): Set[Position] = {
    val lines: Array[Position] = text
      .split(" -> ")
      .map { pair =>
        val Array(x, y) = pair.split(",").map(_.toInt)
        Position(x, y)
      }

    (0 until lines.length - 1).flatMap { idx =>
      val start: Position = lines(idx)
      val end: Position = lines(idx + 1)

      val (smallerX, greaterX) = if (start.x < end.x) (start.x, end.x) else (end.x, start.x)
      val (smallerY, greaterY) = if (start.y < end.y) (start.y, end.y) else (end.y, start.y)

      for {
        x <- smallerX to greaterX
        y <- smallerY to greaterY
      } yield Position(x, y)
    }
      .toSet
  }

  val rocks: Set[Position] =
      InputLoader.loadAsLines("inputs/day14.txt")
//    """
//      |498,4 -> 498,6 -> 496,6
//      |503,4 -> 502,4 -> 502,9 -> 494,9"""
//      .stripMargin
//      .split("\n")
//      .filterNot(_.isBlank)
      .toSet
      .flatMap(parse)

  val area: mutable.Map[Position, Thing] = {
    val rockMap: Map[Position, Thing] = rocks.map { p => p -> Rock }.toMap
    mutable.Map.from(rockMap)
  }

  def drop(position: Position): Position =
      if (!area.contains(position.down))
        position.down
      else if (!area.contains(position.downAndLeft))
        position.downAndLeft
      else if (!area.contains(position.downAndRight))
        position.downAndRight
      else
        position

  val maxX = rocks.map(_.x).max
  val maxY = rocks.map(_.y).max

  def draw(): Unit = {


    (0 to maxY).foreach { y =>
      val line = (0 to maxX)
        .map { x =>
          if (rocks.contains(Position(x, y)))
            "#"
          else
            "."
        }
        .mkString
      println(line)
    }
  }

  draw()

  def task1(): Int = {

    var isLost = false
    var restCounter = 0
    while(!isLost) {

      var sandCornPosition = Position(500, 0)
      var isResting = false

      while(!isLost && !isResting) {
        val next = drop(sandCornPosition)
        isResting = next == sandCornPosition

        if (isResting) {
          restCounter += 1
        }
        else {
            area -= sandCornPosition
            area += next -> Sand
        }

        isLost = sandCornPosition.y > maxY
        sandCornPosition = next
      }

    }
    restCounter
  }


  def task2(): Int = {

    (-1000 to 1000).foreach { x =>
      area += Position(x, maxY + 2) -> Rock
    }

    var isFilled = false
    var restCounter = 0
    while(!isFilled) {

      var sandCornPosition = Position(500, 0)
      var isResting = false

      while(!isFilled && !isResting) {
        val next = drop(sandCornPosition)
        isResting = next == sandCornPosition

        if (isResting) {
          restCounter += 1
        }
        else {
          area -= sandCornPosition
          area += next -> Sand
        }

        isFilled = next == Position(500,0)
        sandCornPosition = next
      }

    }
    restCounter
  }

//  println(task1()) // 698
  println(task2())
  println("done")


}
