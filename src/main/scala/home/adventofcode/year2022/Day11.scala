package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.mutable.ListBuffer

class Monkey(
              val id: Int,
              startingItems: List[Long],
              val operation: Long => Long,
              val divisibleBy: Int,
              val monkeyTrue: Int,
              val monkeyFalse: Int,
            ) {

  val inspectCounter = new AtomicInteger()

  val items = ListBuffer.empty[Long]

  startingItems.foreach(items += _)

}

object Day11 extends App {

  val input = InputLoader
    .loadAsLines("inputs/day11.txt")
    .toList

  val itemsRegex = """.*Starting items: (.+)""".r
  val opRegex = """.*Operation: new = old (.+) (.+)""".r
  val divisibleRegex = """  Test: divisible by (\d+)""".r
  val ifTrueRegex = """    If true: throw to monkey (\d+)""".r
  val ifFalseRegex = """    If false: throw to monkey (\d+)""".r

  val monkeys: List[Monkey] = input
    .grouped(7)
    .map { monkeyLines =>

      new Monkey(
        id = monkeyLines(0).drop(7).dropRight(1).mkString.toInt,
        startingItems = monkeyLines(1) match {
          case itemsRegex(str) => str.split(", ").map(_.toLong).toList
        },
        operation = {
          val (op, other) = monkeyLines(2) match {
            case opRegex(op, other) => (op, other)
          }

          (op, other) match {
            case ("+", "old") => (n: Long) => n + n
            case ("+", x) => (n: Long) => n + x.toInt
            case ("*", "old") => (n: Long) => n * n
            case ("*", x) => (n: Long) => n * x.toInt
          }
        },
        divisibleBy = monkeyLines(3) match {
          case divisibleRegex(num) => num.toInt
        },
        monkeyTrue = monkeyLines(4) match {
          case ifTrueRegex(num) => num.toInt
        },
        monkeyFalse = monkeyLines(5) match {
          case ifFalseRegex(num) => num.toInt
        }
      )
    }
    .toList

  val lcm = monkeys.map(_.divisibleBy).foldLeft(1)(_ * _)

  def inspectAndThrow1(monkey: Monkey): Unit = {
    monkey.items.foreach { item =>
      monkey.inspectCounter.incrementAndGet()
      val newLevel = monkey.operation(item) / 3
      val monkeyToThrow = if (newLevel % monkey.divisibleBy == 0) monkey.monkeyTrue else monkey.monkeyFalse
      monkeys.find(_.id == monkeyToThrow).foreach(_.items += newLevel)
    }
    monkey.items.clear()
  }

  def inspectAndThrow2(monkey: Monkey): Unit = {
    monkey.items.foreach { item =>
      monkey.inspectCounter.incrementAndGet()
      val newLevel = monkey.operation(item % lcm)
      val monkeyToThrow = if (newLevel % monkey.divisibleBy == 0) monkey.monkeyTrue else monkey.monkeyFalse
      monkeys.find(_.id == monkeyToThrow).foreach(_.items += newLevel)
    }
    monkey.items.clear()
  }

  def task1(): Int = {

    (1 to 20).foreach { _ =>
      monkeys.foreach(inspectAndThrow1)
    }

    val inspectionNums: List[Int] = monkeys.map(_.inspectCounter.get()).sorted

    val List(x1, x2) = inspectionNums.takeRight(2)

    x1 * x2
  }


  def task2(): Int = {
    (1 to 10000).foreach { round =>
      monkeys.foreach(inspectAndThrow2)
      val inspectionNums: List[Int] = monkeys.map(_.inspectCounter.get())
      //println(s"$round: ${inspectionNums}")
    }

    val inspectionNums: List[Int] = monkeys.map(_.inspectCounter.get()).sorted

    val List(x1, x2) = inspectionNums.takeRight(2)

    x1 * x2
  }

//  println(task1()) // 10605
  println(task2()) //
  println("done")


}
