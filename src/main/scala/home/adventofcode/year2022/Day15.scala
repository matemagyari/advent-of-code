package home.adventofcode.year2022

import home.adventofcode.InputLoader

object Day15 extends App {

  case class Position(x: Int, y: Int)

  case class Item(sensor: Position, closestBeacon: Position)

  val lineRegex = """Sensor at x=(\d+), y=(\d+): closest beacon is at x=(.*), y=(\d+)""".r

  def parseLine(line: String): Item =
    line match {
      case lineRegex(sX, sY, bX, bY) => Item(Position(sX.toInt, sY.toInt), Position(bX.toInt, bY.toInt))
    }

  val logItems: Set[Item] = InputLoader
        .loadAsLines("inputs/day15.txt")
//    .loadAsLines("inputs/day15test.txt")
    .map(parseLine)
    .toSet

  def distanceBetween(p1: Position, p2: Position): Int =
    Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y)

  val sensors: Set[Position] = logItems.map(_.sensor)
  val beacons: Set[Position] = logItems.map(_.closestBeacon)
  val sensorsWithDistances: Map[Position, Int] = logItems
    .map { item =>
      val distance = distanceBetween(item.sensor, item.closestBeacon)
      (item.sensor, distance)
    }
    .toMap

  def coveredBySensors(position: Position): Boolean =
    sensorsWithDistances.exists { case (sensor, distance) =>
      distanceBetween(position, sensor) <= distance
    }

  val (minX, maxX) = {
    val xs: List[Int] = sensorsWithDistances
      .flatMap { case (p, distance) => List(p.x - distance, p.x + distance) }
      .toList ++ logItems.map(_.closestBeacon.x)

    (xs.min, xs.max)
  }

  def findCoveredPositionsInLine(y: Int, minX: Int, maxX: Int): List[Position] =
    (minX to maxX)
      .filter { x =>
        val position = Position(x, y)
        coveredBySensors(position) && !beacons.contains(position) && !sensors.contains(position)
      }
      .map(Position(_, y))
      .toList

  def task1(): Long = {
    val yInQuestion = 10
    //    val yInQuestion = 2000000
    findCoveredPositionsInLine(yInQuestion, minX, maxX).size
  }

  def merge(ranges: List[Range.Inclusive]): List[Range.Inclusive] =
    ranges.drop(1).foldLeft(ranges.take(1)) { (acc, range) =>
      val last = acc.last
      if (last.end >= range.start - 1)
        acc.dropRight(1) :+ Range.inclusive(
          Math.min(last.start, range.start),
          Math.max(last.end, range.end))
      else
        acc :+ range
    }

  def task2(): Long = {

    val value: Map[Int, List[Range.Inclusive]] = sensorsWithDistances
      .toList
      .flatMap { case (sensor, distance) =>
        println("Sensor calculated")
        (Math.max(0, sensor.y - distance) to Math.min(4000000, sensor.y + distance))
          .map { y =>

            val distanceY = Math.abs(sensor.y - y)
            val distanceX = distance - distanceY
            y -> Range.inclusive(sensor.x - distanceX, sensor.x + distanceX)
          }
      }
      .groupBy(_._1)
      .map { case (y, ranges) =>
        val rangeList: List[Range.Inclusive] = ranges.map(_._2)
        y -> rangeList.sortBy(_.start)
      }

    println("Ranges calculated")

    val positions: Set[Position] = value.flatMap { case (y, ranges) =>

      val fullRanges: List[Range.Inclusive] = //merge(ranges)
        merge(
          List(Range.inclusive(0, ranges.map(_.start).min)) ++
            ranges ++
            List(Range.inclusive(ranges.map(_.end).max, 4000000)))

      (0 to fullRanges.length - 2).flatMap { idx =>
        val start = fullRanges(idx).end + 1
        val end = fullRanges(idx + 1).start - 1
        (start to end).map { x =>
          Position(x, y)
        }
      }
    }
      .toSet

    val position = positions.head
    position.x.toLong * 4000000 + position.y.toLong
  }

  println(task1()) // 5688618
  val start = System.currentTimeMillis()
  println(task2()) // Position(3156345,3204261) = 12625383204261
  println(s"done ${System.currentTimeMillis() - start}")


}
