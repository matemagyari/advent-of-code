package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.collection.mutable.ListBuffer

object Day10 extends App {

  val input = InputLoader
    .loadAsLines("inputs/day10test.txt")
    .toList

  sealed trait Instruction

  case object Noop extends Instruction

  case class AddX(value: Int) extends Instruction

  def parseLine(text: String): Instruction =
    text.split(" ") match {
      case Array(_) => Noop
      case Array(_, num) => AddX(num.toInt)
    }

  val xValues: List[Int] = {
    val xs = ListBuffer(1)
    input.map(parseLine).foreach {
      case Noop =>
        xs += xs.last
      case AddX(value) =>
        xs += xs.last
        xs += xs.last + value
    }
    // add a first element so the cycle nums will correspond to the list index
    0 +: xs.toList
  }

  def task1(): Int = {
    val cycleNums: List[Int] = List.iterate(20, 6)(_ + 40)
    cycleNums.map { cycle => xValues(cycle) * cycle }.sum
  }

  def display(pixels: Array[Char]): Unit = {
    pixels.grouped(40).foreach { row =>
      println(row.mkString)
    }
  }

  def task2(): Int = {
    val pixels: Array[Char] = Array.fill(240)('.')
    (1 to 240).toList.foreach { cycle =>
      val spriteMiddle = xValues(cycle)
      val idxInRow = (cycle - 1) % 40
      pixels(cycle - 1) =
        if (Math.abs(idxInRow - spriteMiddle) <= 1) '#' else '.'

      display(pixels)
      println("\n\n")
    }


    0
  }

  println(task1()) // 6067
  println(task2()) // 2471 too low


}
