package home.adventofcode.year2022

import home.adventofcode.InputLoader
import home.adventofcode.year2022.Planner._

import scala.collection.mutable

object Planner {
  sealed trait Material

  case object Ore extends Material

  case object Clay extends Material

  case object Obsidian extends Material

  case object Geode extends Material

  case class Blueprint(
                        id: Int,
                        robotCost: Map[Material, Map[Material, Int]])
}

class Planner(blueprint: Blueprint) {

  private val minutes: Int = 24
  private val resources = mutable.Map.empty[Material, Int]
  private val robots = mutable.Map.empty[Material, Int]

  robots += Ore -> 1

  private def addRobot(material: Material): Unit = {
    robots += material -> (robots.getOrElse(material, 0) + 1)
  }

  private def depleteResources(robot: Material): Unit = {
    val robotCost: Map[Material, Int] = blueprint.robotCost(robot)
    robotCost.foreach { case (material, amount) =>
      val newAmount: Int = resources.getOrElse(material, 0) - amount
      if (newAmount < 0) sys.error(s"$material run out!")
      resources += material -> newAmount
    }
  }

  private def nextRobot(minutesLeft: Int): Option[Material] = {

    def buildRobotIfPossible(material: Material): Option[Material] = {
      val robotCost: Map[Material, Int] = blueprint.robotCost(material)
      val resourcesAvailable = robotCost.forall { case (material, cost) => cost <= resources.getOrElse(material, 0) }

      if (resourcesAvailable)
        Some(material)
      else
        None
    }

    List(Geode, Obsidian, Clay, Ore).foldLeft(None: Option[Material]) { (acc, material) =>
      acc.orElse(buildRobotIfPossible(material))
    }

  }

  def valueOfBlueprint(): Int = {

    (1 to minutes).foreach { minute =>

      println(s"\n== Minute $minute ==")

      val robotToBuild: Option[Material] = nextRobot(minutes - minute)

      robotToBuild.foreach { material =>
        val cost = blueprint.robotCost(material)
          .map { case (material, unit) => s"$unit $material" }
          .mkString(" and ")

        println(s"Spend $cost to start building a $material-collecting robot.")
        depleteResources(material)
      }

      // each robot collects a one unit of its type of material
      robots.foreach { case (material, collectedUnits) =>
        val newAmount: Int = resources.getOrElse(material, 0) + collectedUnits
        resources += material -> newAmount
      }

      List(Ore, Clay, Obsidian, Geode).foreach { material =>
        robots.get(material).foreach { robotNum =>
          val units = resources.getOrElse(material, 0)
          println(s"$robotNum $material-collecting robot collects $robotNum $material; you now have $units $material.")
        }
      }

      robotToBuild.foreach { material =>
        addRobot(material)
      }

      // create new robots and deplete the resources
    }

    1
  }

}

object Day19 extends App {

  //  val file = "inputs/day19.txt"
  val file = "inputs/day19test.txt"

  val lineRegex = """Blueprint (\d+): Each ore robot costs (\d+) ore. Each clay robot costs (\d+) ore. Each obsidian robot costs (\d+) ore and (\d+) clay. Each geode robot costs (\d+) ore and (\d+) obsidian.""".r

  def parse(line: String): Blueprint =
    line match {
      case lineRegex(id, oreCost, clayCost, obsidianCostOre, obsidianCostClay, geodeCostOre, geodeCostObsidian) =>
        Blueprint(
          id = id.toInt,
          robotCost = Map(
            Ore -> Map(Ore -> oreCost.toInt),
            Clay -> Map(Ore -> clayCost.toInt),
            Obsidian -> Map(
              Ore -> obsidianCostOre.toInt,
              Clay -> obsidianCostClay.toInt),
            Geode -> Map(
              Ore -> geodeCostOre.toInt,
              Obsidian -> geodeCostObsidian.toInt)
          ))
    }

  val blueprints: List[Blueprint] = InputLoader
    .loadAsLines(file)
    .map(parse)
    .toList


  def task1(): Long = {
    blueprints.map { blueprint => new Planner(blueprint).valueOfBlueprint() }.max
  }


  def task2(): Long = {

    1
  }

  println(task1()) //
  println(task2()) //


}


