package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.util

object Day6 extends App {

//      val characters: Array[Char] = "nppdvjthqldpwncqszvftbrmjlhg".toCharArray
//      val characters: Array[Char] = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw".toCharArray
  val characters: Array[Char] = InputLoader
    .load("inputs/day6.txt")
    .toCharArray

  println(task1()) // 1480
  println(task2()) // 2746

  def findEndOfFirstMessage(bufferLen: Int): Int = {
    var lastXChars: List[Char] = List.empty
    var index = 0
    while (lastXChars.toSet.size < bufferLen) {
      lastXChars = lastXChars :+ characters(index)
      lastXChars = if (lastXChars.size > bufferLen) lastXChars.drop(1) else lastXChars
      index += 1
    }
    index
  }

  def task1(): Int = findEndOfFirstMessage(4)

  // Task 2
  def task2(): Int = findEndOfFirstMessage(14)
}
