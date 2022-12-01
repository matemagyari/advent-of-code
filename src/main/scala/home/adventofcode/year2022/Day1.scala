package home.adventofcode.year2022

import home.adventofcode.InputLoader

object Day1 extends App {

//  println(task1())
  println(task2())

  def task1(): Int = {
    var max = 0
    var current = 0
    InputLoader
      .loadAsLines("inputs/day1.txt")
      .foreach { line =>
        if (line.isBlank) {
          max = Math.max(max, current)
          current = 0
        }
        else {
          current += line.trim.toInt
        }
      }

    max
  }

  // Task 2
  def task2(): Int = {
    val topN = 3
    var maxes = List.fill(topN)(0)
    var current = 0
    InputLoader
      .loadAsLines("inputs/day1.txt")
      .foreach { line =>
        if (line.isBlank) {
          maxes = (current +: maxes).sorted.drop(1)
          current = 0
        }
        else {
          current += line.trim.toInt
        }
      }

    println(maxes)

    maxes.sum
  }

}
