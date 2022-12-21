package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.math.Ordering.Implicits.seqOrdering

object Day18 extends App {

  case class Cube(x: Int, y: Int, z: Int)

  val cubes: List[Cube] = InputLoader
    .loadAsLines("inputs/day18test.txt")
//    .loadAsLines("inputs/day18.txt")
    .map { line =>
      val Array(x, y, z) = line.split(",").map(_.toInt)
      Cube(x, y, z)
    }
    .toList

  val sortedByXY: List[Cube] = cubes.map { c => (List(c.x, c.y, c.z), c) }.sortBy(_._1).map(_._2)
  val sortedByXZ: List[Cube] = cubes.map { c => (List(c.x, c.z, c.y), c) }.sortBy(_._1).map(_._2)
  val sortedByYZ: List[Cube] = cubes.map { c => (List(c.y, c.z, c.x), c) }.sortBy(_._1).map(_._2)

  println("")

  def isAdjacent(p1: Cube, p2: Cube): Boolean =
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y) + Math.abs(p1.z - p2.z) == 1

  def isAdjacent(idx: Int, cubes: List[Cube]): Boolean =
    isAdjacent(cubes(idx), cubes(idx + 1))

  def processList(cubes: List[Cube]): Set[Set[Cube]] = {
    (0 to cubes.length - 2).flatMap { idx =>
      val c1 = cubes(idx)
      val c2 = cubes(idx + 1)
      if (isAdjacent(c1, c2)) {
        //println(s"Adjacent $c1 $c2")
        Some(Set(c1, c2))
      }
      else
        None
    }
      .toSet
  }

  //case class NeighbourCollector(c: Cube, n1: Cube, n2: Cube)

  def findWith2Neighbours(cubes: List[Cube]): Set[Cube] = {
    (1 to cubes.length - 2).flatMap { idx =>
      val c0 = cubes(idx-1)
      val c1 = cubes(idx)
      val c2 = cubes(idx + 1)
      if (isAdjacent(c1, c2) && isAdjacent(c0, c1)) {
        // it has 2 neighbours
        Some(c1)
      }
      else
        None
    }
      .toSet
  }

  val r1 = processList(sortedByXY)
  val r2 = processList(sortedByXZ)
  val r3 = processList(sortedByYZ)
  val allPairs = r1 ++ r2 ++ r3

  def task1(): Long = {

    println("All pairs")
    allPairs.foreach(println)
    val covered = allPairs.size * 2
    cubes.size * 6 - covered
  }


  def task2(): Long = {

    val n1 = findWith2Neighbours(sortedByXY)
    println("neighbours found xy")
    val n2 = findWith2Neighbours(sortedByXZ)
    println("neighbours found xz")
    val n3 = findWith2Neighbours(sortedByYZ)
    println("neighbours found yz")

    val inside = n1.intersect(n2).intersect(n3)


    val covered = allPairs.size * 2
    cubes.size * 6 - inside.size * 6 - covered
  }

  println(task1()) // 4332
  println(task2()) // 4332


}


