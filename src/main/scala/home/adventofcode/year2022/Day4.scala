package home.adventofcode.year2022

import home.adventofcode.InputLoader

object Day4 extends App {

  println(task1()) // 588
  println(task2()) // 911

  def parseRange(text: String): Range = {
    val Array(start, end) = text.split("-")
    Range.inclusive(start.toInt, end.toInt)
  }

  def parseLine(line: String): (Range, Range) = {
    val Array(range1, range2) = line.split(",").map(parseRange)
    (range1, range2)
  }

  def includes(r1: Range, r2: Range): Boolean =
    r1.start <= r2.start && r1.end >= r2.end

  def task1(): Int = {

    InputLoader
      .loadAsLines("inputs/day4.txt")
      .count { line =>
        val (range1, range2) = parseLine(line)
        includes(range1, range2) || includes(range2, range1)
      }
  }

  // Task 2
  def task2(): Int = {
    InputLoader
      .loadAsLines("inputs/day4.txt")
      .count { line =>
        val (range1, range2) = parseLine(line)
        range1.intersect(range2).nonEmpty
      }
  }
}
