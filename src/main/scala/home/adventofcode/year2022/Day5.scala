package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.util

object Day5 extends App {

  val commandPattern = """move (\d+) from (\d+) to (\d+)""".r
  case class Command(from: Int, to: Int, num: Int)
  type Crate = String

  val lines = InputLoader
    .loadAsLines("inputs/day5.txt")
    .toList

  val stacks: Map[Int, util.Stack[Crate]] = parseCrates(lines.take(9))

  val commands: List[Command] = lines
    .drop(10)
    .map(parseCommand)

//  println(task1()) // WSFTMRHPP
    println(task2()) // GSLCMFBRP

  //move 1 from 5 to 6
  def parseCommand(text: String): Command = text match {
    case commandPattern(num, from, to) => Command(from.toInt, to.toInt, num.toInt)
  }

  def parseCrates(lines: List[String]): Map[Int, util.Stack[Crate]] = {
    val crates: Map[Int, util.Stack[Crate]] = (1 to 9).map { i => (i, new util.Stack[Crate]) }.toMap
    lines
      .take(8)
      .reverse
      .foreach { line =>
        (1 to 9).foreach { idx =>
          val crate: Crate = line.charAt((idx - 1) * 4 + 1).toString
          if (!crate.isBlank) {
            crates(idx).push(crate)
          }

        }
      }
    crates
  }

  def tops(): String =
    stacks
      .map { case (idx, stack) => (idx, stack.peek()) }
      .toList
      .sortBy(_._1)
      .map(_._2)
      .mkString

  def executeWith9000(stack: Map[Int, util.Stack[Crate]], command: Command): Unit = {
    val fromStack = stack(command.from)
    val toStack = stack(command.to)
    for (_ <- 1 to command.num) {
      toStack.push(fromStack.pop())
    }
  }

  def executeWith9001(stack: Map[Int, util.Stack[Crate]], command: Command): Unit = {
    val fromStack = stack(command.from)
    val toStack = stack(command.to)

    val cratesToMove: Seq[Crate] = (1 to command.num).map { _ => fromStack.pop() }
    cratesToMove.reverse.foreach(toStack.push)
  }

  def task1(): String = {
    commands.foreach { command =>
      executeWith9000(stacks, command)
      }
    tops()
  }

  // Task 2
  def task2(): String = {
    commands.foreach { command =>
      executeWith9001(stacks, command)
    }
    tops()
  }
}
