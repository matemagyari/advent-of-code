package home.adventofcode.year2022

import home.adventofcode.InputLoader
import home.adventofcode.year2022.Planner._

import java.time.LocalDate
import scala.collection.mutable

object Day20 extends App {

    val file = "inputs/day20.txt"
//  val file = "inputs/day20test.txt"

  val logEnabled = false


  val nums: List[Int] = InputLoader
    .loadAsLines(file)
    .map(_.toInt)
    .toList

  case class Elem(num: Int, idx: Int)


  def mix(nums: List[Int]): List[Int] = {
    val positions: mutable.Map[Elem, Int] = mutable.Map.empty

    positions ++= nums.zipWithIndex.map { case (num, idx) => Elem(num, idx) -> idx }

    def toList(): List[Int] =
      positions.toList.sortBy(_._2).map(_._1.num)

    def isValid(): Unit = {
      assert(positions.size == nums.size)
      assert(positions.values.toSet.size == nums.size)
    }

    def print(): Unit = {
      log(s"Current: ${toList().mkString(",")}")
    }

    println(s"Initial ${nums.size}")
    print()
    nums
      .zipWithIndex
      .map { case (num, idx) => Elem(num, idx) }
      .foreach { numToMove =>

        if (numToMove.idx % 500 == 0) {
          println(numToMove.idx)
        }

        //        isValid()

        val oldPosition: Int = positions(numToMove)
        val newPosition: Int = {
          val np = (oldPosition + numToMove.num) % nums.size
          val changed = oldPosition != np
          if (changed)
            if (np <= 0)
              nums.size + np - 1
            else if (np == nums.size - 1)
              0
            else if (np < oldPosition)
              np + 1
            else np
          else
            np
        }

        log(s"$numToMove moves from $oldPosition to $newPosition")

        positions += numToMove -> newPosition

        positions
          .filterNot(_._1 == numToMove)
          .map { case (num, idx) =>

            val newIdx = {

              val offset: Int =
                if (newPosition >= oldPosition && idx >= oldPosition && idx <= newPosition)
                  -1
                else if (newPosition < oldPosition && newPosition <= idx && idx < oldPosition)
                  1
                else
                  0

              (idx + offset) % nums.size
            }

            positions += num -> newIdx
          }

        print()
      }
    toList()
  }


  def task1(): Long = {

    val mixedList: List[Int] = mix(nums)

    def findNum(offset: Int): Int = {
      val position0 = mixedList.zipWithIndex.collectFirst { case (num, idx) if num == 0 => idx }.get
      val position = (position0 + offset) % mixedList.size
      mixedList(position)
    }

    Set(1000, 2000, 3000).map(findNum).sum
  }


  def task2(): Long = {

    1
  }

  println(task1()) // 3234 - too low
  //  println(task2()) //

  def log(text: String): Unit = {
    if (logEnabled) {
      println(s"${LocalDate.now()} $text")
    }
  }


}


