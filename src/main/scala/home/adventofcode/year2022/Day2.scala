package home.adventofcode.year2022

import home.adventofcode.InputLoader

object Day2 extends App {

  println(task1()) // 13809
  println(task2())

  sealed trait Shape
  case object Rock extends Shape
  case object Paper extends Shape
  case object Scissors extends Shape

  sealed trait Outcome
  case object Win extends Outcome
  case object Lose extends Outcome
  case object Draw extends Outcome

  def valueOf(shape: Shape): Int = shape match {
    case Rock => 1
    case Paper => 2
    case Scissors => 3
  }

  def valueOfOutcome(outcome: Outcome): Int = outcome match {
    case Win => 6
    case Lose => 0
    case Draw => 3
  }

  def parseShape(s: String): Shape = s match {
    case "A" | "X" => Rock
    case "B" | "Y" => Paper
    case "C" | "Z" => Scissors
  }

  def task1(): Int = {

    def getOutcome(opponent: Shape, me: Shape): Outcome = (opponent, me) match {
      case (a, b) if a == b => Draw
      case (Rock, Paper) => Win
      case (Rock, Scissors) => Lose
      case (Paper, Rock) => Lose
      case (Paper, Scissors) => Win
      case (Scissors, Rock) => Win
      case (Scissors, Paper) => Lose
    }

    def evaluate(opponent: Shape, me: Shape): Int = {
      val outcome = getOutcome(opponent, me)
      valueOfOutcome(outcome) + valueOf(me)
    }

    InputLoader
      .loadAsLines("inputs/day2.txt")
      .map { line =>
        val parts: Array[Shape] = line.split(" ").map(parseShape)
        evaluate(parts(0), parts(1))
      }
      .sum
  }

  // Task 2
  def task2(): Int = {

    def parseOutcome(s: String): Outcome = s match {
      case "X" => Lose
      case "Y" => Draw
      case "Z" => Win
    }

    def getMyShape(opponent: Shape, outcome: Outcome): Shape = (opponent, outcome) match {
      case (x, Draw) => x
      case (Rock, Win) => Paper
      case (Rock, Lose) => Scissors
      case (Paper, Win) => Scissors
      case (Paper, Lose) => Rock
      case (Scissors, Win) => Rock
      case (Scissors, Lose) => Paper
    }

    def evaluate(opponent: Shape, outcome: Outcome): Int = {
      val me = getMyShape(opponent, outcome)
      valueOfOutcome(outcome) + valueOf(me)
    }

    InputLoader
      .loadAsLines("inputs/day2.txt")
      .map { line =>
        val parts: Array[String] = line.split(" ")
        evaluate(parseShape(parts(0)), parseOutcome(parts(1)))
      }
      .sum
  }

}
