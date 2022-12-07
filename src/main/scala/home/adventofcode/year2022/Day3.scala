package home.adventofcode.year2022

import home.adventofcode.InputLoader

object Day3 extends App {

  println(task1()) // 7817
  println(task2())


  def priority(c: Char): Int = c.toInt match {
    case x if 97 <= x && x <= 122 => x - 96
    case x if 65 <= x && x <= 90 => x - 38
  }


  def task1(): Int = {

    InputLoader
      .loadAsLines("inputs/day3.txt")
      .map { line =>

        val items: Array[Char] = line.toArray
        val size = items.size / 2
        val compartment1 = items.take(size)
        val compartment2 = items.drop(size)

        compartment1.toSet
          .intersect(compartment2.toSet)
          .map(priority)
          .sum
      }
      .sum
  }

  // Task 2
  def task2(): Int = {
    InputLoader
      .loadAsLines("inputs/day3.txt")
      .grouped(3)
      .map { case Seq(items1, items2, items3) =>

        val typesInSack1: Set[Char] = items1.toSet
        val typesInSack2: Set[Char] = items2.toSet
        val typesInSack3: Set[Char] = items3.toSet

        typesInSack1
          .intersect(typesInSack2)
          .intersect(typesInSack3)
          .map(priority)
          .sum

      }
      .sum
  }
}
