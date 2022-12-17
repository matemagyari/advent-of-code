package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.collection.mutable

object Day12 extends App {

  case class Position(x: Int, y: Int)

  case class Cell(position: Position, height: Int)

  case class Step(cell: Cell, step: Int)

  type Height = Int

  def calculateHeight(c: Char): Int = c match {
    case 'S' => 'a'.toInt
    case 'E' => 'z'.toInt
    case _ => c.toInt
  }

  var target: Cell = null
  var start: Cell = null

  val grid: Map[Position, Height] =
    InputLoader
      .loadAsLines("inputs/day12.txt")
      .zipWithIndex
      .flatMap { case (line, y) =>
        line
          .toCharArray
          .toList
          .zipWithIndex
          .map { case (char, x) =>
            val position = Position(x, y)
            val height = calculateHeight(char)
            if (char == 'S') {
              start = Cell(position, height)
            } else if (char == 'E') {
              target = Cell(position, height)
            }
            position -> height
          }
      }
      .toMap

  def distance(p1: Position, p2: Position): Int =
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  def heightDiff(cell: Cell): Int =
    Math.abs(cell.height - target.height)

  def accessibleNeighbours(position: Position): Set[Position] =
    Set(
      (-1, 0),
      (1, 0),
      (0, 1),
      (0, -1)
    ).map { case (dx, dy) =>
      position.copy(x = position.x + dx, y = position.y + dy)
    }

  def accessibleNeighbours(cell: Cell): Set[Cell] =
    for {
      neighbour <- accessibleNeighbours(cell.position)
      height <- grid.get(neighbour)
      if height <= cell.height + 1
    } yield Cell(neighbour, height)

  implicit val cellOrdering: Ordering[Step] = new Ordering[Step] {
    override def compare(x: Step, y: Step): Height =
      heightDiff(x.cell).compare(heightDiff(y.cell))
  }

  def findPathFrom(startCell: Cell): Option[Int] = {
    val pq = collection.mutable.PriorityQueue[Step](Step(startCell, 0))

    val shortest: mutable.Map[Position, Int] = mutable.Map.empty

    shortest += startCell.position -> 0

    var current: Step = null
    while (!pq.isEmpty) {
      current = pq.dequeue()
      accessibleNeighbours(current.cell).foreach { next =>
        val nextStep = current.step + 1
        // if we got there already quicker, then don't enqueue
        if (!shortest.get(next.position).exists(_ <= nextStep)) {
          shortest += next.position -> nextStep
          pq.enqueue(Step(next, nextStep))
        }
      }
    }

    shortest.get(target.position)
  }

  def task1(): Int = findPathFrom(start).get


  def task2(): Int = {
    val paths = grid
      .collect { case (position, height) if height == calculateHeight('a') => position }
      .flatMap { pos => findPathFrom(Cell(pos, calculateHeight('a') )) }
    paths.min
  }

  println(task1()) // 339
    println(task2()) // 332
  println("done")


}
