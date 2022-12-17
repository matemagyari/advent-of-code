package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.collection.mutable.ListBuffer

sealed trait Direction

case object Left extends Direction

case object Right extends Direction

case object Up extends Direction

case object Down extends Direction

case class Move(dir: Direction, steps: Int)

case class Position(x: Int, y: Int) {
  def move(direction: Direction): Position = direction match {
    case Right => copy(x + 1, y)
    case Left => copy(x - 1, y)
    case Up => copy(x, y + 1)
    case Down => copy(x, y - 1)
  }

  def moves(direction1: Direction*): Position =
    direction1.foldLeft(this) { (currPos, dir) => currPos.move(dir) }

  def touching(other: Position): Boolean = {
    val xDiff = Math.abs(x - other.x)
    val yDiff = Math.abs(y - other.y)
    yDiff <= 1 && xDiff <= 1
  }
}

object Day9 extends App {

  def dirsForTail(headPosition: Position, tailPosition: Position): List[Direction] = {
    if (tailPosition.touching(headPosition))
      List.empty
    else {
      val vertical: Option[Direction] =
        if (headPosition.x > tailPosition.x)
          Some(Right)
        else if (headPosition.x < tailPosition.x)
          Some(Left)
        else
          None

      val horizontal: Option[Direction] =
        if (headPosition.y > tailPosition.y)
          Some(Up)
        else if (headPosition.y < tailPosition.y)
          Some(Down)
        else
          None

      vertical.toList ++ horizontal.toList
    }
  }

  val input1 = InputLoader
    .loadAsLines("inputs/day9.txt")
    .toList

  val input2 =
    """
      |R 4
      |U 4
      |L 3
      |D 1
      |R 4
      |D 1
      |L 5
      |R 2""".stripMargin.split("\n").filterNot(_.isBlank).toList


  def parseLine(line: String): Move = {
    val Array(d, steps) = line.split(" ")
    val dir = d match {
      case "R" => Right
      case "L" => Left
      case "U" => Up
      case "D" => Down
    }
    Move(dir, steps.toInt)
  }

  val moves = input1.map(parseLine)

  def task1(): Int = {

    var headPosition = Position(0, 0)
    var tailPosition = Position(0, 0)

    val trace: ListBuffer[Position] = ListBuffer(tailPosition)
    moves.foreach { case Move(dir, steps) =>
      (1 to steps).foreach { _ =>
        headPosition = headPosition.move(dir)
        tailPosition = tailPosition.moves(dirsForTail(headPosition, tailPosition): _*)
        trace += tailPosition
      }
    }

    trace.toSet.size
  }

  def task2(): Int = {

    var headPosition = Position(0, 0)
    var tailPositions = List.fill(9)(Position(0, 0))

    val trace: ListBuffer[Position] = ListBuffer(tailPositions.last)
    moves.foreach { case Move(dir, steps) =>
      (1 to steps).foreach { _ =>
        headPosition = headPosition.move(dir)

        tailPositions = tailPositions
          .foldLeft(List(headPosition)) { (currHead, tail) =>
            currHead ++ List(tail.moves(dirsForTail(currHead.last, tail): _*))
          }
          .drop(1)

        trace += tailPositions.last
      }
    }

    trace.toSet.size
  }

  println(task1()) // 6067
  println(task2()) // 2471 too low


}
