package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.util.concurrent.atomic.{AtomicInteger, AtomicLong}
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day17 extends App {

  val startingDY = 4
  val width = 7
  val drawingEnabled = false

  sealed trait Push
  case object Left extends Push
  case object Right extends Push

  case class Position(x: Int, y: Int) {
    def plusX(dx: Int): Position = copy(x = x + dx)
    def plusY(dy: Int): Position = copy(y = y + dy)
  }

  sealed trait Shape {
    def filledPositions(leftBottomCorner: Position): Set[Position]
  }
  case object HorizontalLine extends Shape {
    override def filledPositions(leftBottomCorner: Position): Set[Position] =
      (0 to 3).map { dx => leftBottomCorner.plusX(dx) }.toSet
  }
  case object VerticalLine extends Shape {
    override def filledPositions(leftBottomCorner: Position): Set[Position] =
      (0 to 3).map { dy => leftBottomCorner.plusY(dy) }.toSet
  }
  case object L extends Shape {
    override def filledPositions(leftBottomCorner: Position): Set[Position] =
      Set(
        leftBottomCorner,
        leftBottomCorner.plusX(1),
        leftBottomCorner.plusX(2),
        leftBottomCorner.plusX(2).plusY(1),
        leftBottomCorner.plusX(2).plusY(2)
      )
  }
  case object Square extends Shape {
    override def filledPositions(leftBottomCorner: Position): Set[Position] =
      Set(
        leftBottomCorner,
        leftBottomCorner.plusX(1),
        leftBottomCorner.plusY(1),
        leftBottomCorner.plusX(1).plusY(1)
      )
  }
  case object Plus extends Shape {
    override def filledPositions(leftBottomCorner: Position): Set[Position] =
      Set(
        leftBottomCorner.plusX(1),
        leftBottomCorner.plusY(1),
        leftBottomCorner.plusX(1).plusY(1),
        leftBottomCorner.plusX(1).plusY(2),
        leftBottomCorner.plusX(2).plusY(1)
      )
  }

  case class Rock(leftBottomCorner: Position, shape: Shape) {
    def drop(): Rock = copy(leftBottomCorner = leftBottomCorner.plusY(-1))
    def move(push: Push): Rock = {
      val dx = push match {
        case Left => -1
        case Right => 1
      }
      copy(leftBottomCorner = leftBottomCorner.plusX(dx))
    }
    def filledPositions(): Set[Position] = shape.filledPositions(leftBottomCorner)
  }

  val jet: List[Push] = InputLoader
//    .load("inputs/day17test.txt")
    .load("inputs/day17.txt")
    .toCharArray
    .map {
      case '<' => Left
      case '>' => Right
    }
    .toList

  val fallOrder: List[Shape] = List(HorizontalLine, Plus, L,VerticalLine , Square)

  def draw(filledPositions: Set[Position]): Unit = {
    if (drawingEnabled) {
      val maxY = if (filledPositions.isEmpty) startingDY - 1 else Math.max(filledPositions.map(_.y).max, 7)

      for (y <- maxY to 0 by -1) {
        val line = (0 to width - 1).map { x =>
          if (filledPositions.contains(Position(x, y))) "#" else "."
        }.mkString
        println(line)
      }
      println("")
    }
  }

  def task1(): Long = {

    var highestPoint = -1
    var restingRocks = 0

    def nextShape(): Rock = {
      val shape = fallOrder(restingRocks % fallOrder.size)
      Rock(Position(2, highestPoint + startingDY), shape)
    }

    var current: Rock = nextShape()

    val idx = new AtomicInteger()

    val filledPositions = mutable.Set.empty[Position]

    def hitSomething(rock: Rock): Boolean = {
      def hitTheWall: Boolean =
        rock.filledPositions().exists(p => p.x < 0 || p.x >= 7)

      def hitAnotherRock: Boolean =
        rock.filledPositions().exists(filledPositions.contains)

      rock.leftBottomCorner.y < 0 || rock.leftBottomCorner.x < 0 || hitTheWall || hitAnotherRock
    }

    draw(filledPositions.toSet ++ current.filledPositions())

    while(restingRocks < 2022) {

      val index = idx.getAndIncrement()

      if (index % 1000 == 0) {
        println(s"index $index restingRocks $restingRocks highestPoint $highestPoint")
      }

      val push = jet(index % jet.size)
      val afterPushedSideways = current.move(push)
      if (!hitSomething(afterPushedSideways)) {
        current = afterPushedSideways
      }
      log(s"$index Pushed $push")
      draw(filledPositions.toSet ++ current.filledPositions())

      val afterDrop = current.drop()

      if (hitSomething(afterDrop)) {
        log(s"$index Stuck")
        // reached resting place
        restingRocks += 1
        highestPoint = Math.max(highestPoint, current.filledPositions().map(_.y).max)
        filledPositions ++= current.filledPositions()
        draw(filledPositions.toSet)
        current = nextShape()
        log(s"$index New appeared")
        draw(filledPositions.toSet ++ current.filledPositions())
      }
      else {
        current = afterDrop
        log(s"$index Dropped")
        draw(filledPositions.toSet ++ current.filledPositions())
      }

    }

    println(s"restingRocks $restingRocks highestPoint $highestPoint")

    // +1 because highestPoint is index starting with zero
    highestPoint + 1

  }


  def task2(): Long = {

    1
  }

  println(task1()) // 3076

  private def log(str: String): Unit = {
    if (drawingEnabled) {
      println(str)
    }
  }


}
