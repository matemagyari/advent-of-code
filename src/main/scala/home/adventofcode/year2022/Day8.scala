package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.collection.mutable.ListBuffer


object Day8 extends App {

  val input1 = InputLoader
    .loadAsLines("inputs/day8.txt")
    .toArray

  val input2 =
    """
      |30373
      |25512
      |65332
      |33549
      |35390""".stripMargin.split("\n").filterNot(_.isBlank)

  val input: Array[Array[Int]] = input1
    .map { line =>
      line.toCharArray.map(_.asDigit)
    }

  val rowNum = input.length
  val colNum = input.head.length

  def task1(): Int = {

    val visibilities: Array[Array[Boolean]] = input.map { row => Array.fill(row.length)(false) }

    def processRow(rowIdx: Int): Unit = {
      visibilities(rowIdx)(0) = true
      visibilities(rowIdx)(colNum - 1) = true
      def traverse(indices: List[Int]): Unit = {
        var max = input(rowIdx)(indices.head)
        indices.foreach { colIdx =>
          val tree = input(rowIdx)(colIdx)
          if (!visibilities(rowIdx)(colIdx)) { // if it's already visible, leave it
            visibilities(rowIdx)(colIdx) = tree > max
          }
          max = Math.max(max, tree)
        }
      }

      // from left
      traverse((0 to colNum - 1).toList)
      // from right
      traverse((colNum - 1 to 0 by - 1).toList)
    }

    def processColumn(colIdx: Int): Unit = {
      val col: Array[Int] = input.map { row => row(colIdx) }
      visibilities(0)(colIdx) = true
      visibilities(rowNum - 1)(colIdx) = true

      def traverse(indices: List[Int]): Unit = {
        var max = input(indices.head)(colIdx)
        indices.foreach { rowIdx =>
          val tree = col(rowIdx)
          if (!visibilities(rowIdx)(colIdx)) { // if it's already visible, leave it
            visibilities(rowIdx)(colIdx) = tree > max
          }
          max = Math.max(max, tree)
        }
      }

      // from top
      traverse((0 to rowNum - 1).toList)
      // from bottom
      traverse((rowNum - 1 to 0 by - 1).toList)
    }

    (0 to rowNum - 1).foreach(processRow)
    (0 to colNum - 1).foreach(processColumn)

    visibilities.map { row => row.count(identity) }.sum
  }

  def task2(): Int = {

    val scenicScores: Array[Array[Int]] = input.map { row => Array.fill(row.length)(0) }

    def seeingDistance(treesAhead: List[Int], tree: Int): Int = {
      treesAhead.zipWithIndex
        .collectFirst { case (otherTree, idx) if otherTree >= tree => idx + 1 }
        .getOrElse(treesAhead.size)
    }

    (0 to rowNum - 1).foreach { rowIdx =>
      (0 to colNum - 1).foreach { colIdx =>
        if (rowIdx == 0 || colIdx == 0 || rowIdx == rowNum - 1 || colIdx == colNum - 1)
          scenicScores(rowIdx)(colIdx) = 0
        else {
          val treeHeight = input(rowIdx)(colIdx)
          val col: List[Int] = input.map { row => row(colIdx) }.toList
          val row = input(rowIdx).toList
          val leftScore = seeingDistance(row.take(colIdx).reverse, treeHeight)
          val rightScore = seeingDistance(row.drop(colIdx + 1), treeHeight)
          val topScore = seeingDistance(col.take(rowIdx).reverse, treeHeight)
          val bottomScore = seeingDistance(col.drop(rowIdx + 1), treeHeight)

          scenicScores(rowIdx)(colIdx) = leftScore * rightScore * topScore * bottomScore
        }
      }
    }

    scenicScores.map(_.max).max
  }

//  println(task1()) // 1736
  println(task2()) // 268800


}
