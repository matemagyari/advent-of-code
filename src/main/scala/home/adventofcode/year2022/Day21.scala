package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.time.LocalDate
import scala.collection.mutable

object Day21 extends App {

      val file = "inputs/day21.txt"
//  val file = "inputs/day21test.txt"

  val logEnabled = false

  sealed trait Operation

  case object Plus extends Operation

  case object Minus extends Operation

  case object Multiply extends Operation

  case object Divide extends Operation

  def parseOperator(text: String): Operation = text match {
    case "*" => Multiply
    case "/" => Divide
    case "+" => Plus
    case "-" => Minus
  }

  def applyOp(num1: Long, num2: Long, operation: Operation): Long = operation match {
    case Plus => num1 + num2
    case Minus => num1 - num2
    case Multiply => num1 * num2
    case Divide => num1 / num2
  }


  val numLine = """(.+): (\d+)""".r
  val formLine = """(.+): (.+) (.) (.+)""".r

  sealed trait Yell {
    def code: String
  }

  case class Num(code: String, number: Long) extends Yell

  case class Form(code: String, code1: String, code2: String, op: Operation) extends Yell

  def parse(text: String): Yell = text match {
    case numLine(code, value) => Num(code, value.toInt)
    case formLine(code, code1, op, code2) => Form(code, code1, code2, parseOperator(op))
  }

  val yells: List[Yell] = InputLoader
    .loadAsLines(file)
    .map(parse)
    .toList

  val yellMap: Map[String, Yell] = yells.map { yell => yell.code -> yell }.toMap

  val root = yells.find(_.code == "root").get

  def task1(): Long = {

    val values = mutable.Map.empty[String, Long]

    def evaluate(yell: Yell): Long = {
      yell match {
        case Num(code, num) =>
          values += code -> num
          num
        case Form(code, code1, code2, operation) =>
          val code1Value: Long = values.get(code1).getOrElse {
            val yell: Yell = yellMap(code1)
            evaluate(yell)
          }
          values += code1 -> code1Value

          val code2Value: Long = values.get(code2).getOrElse {
            val yell: Yell = yellMap(code2)
            evaluate(yell)
          }

          values += code2 -> code2Value

          val result = applyOp(code1Value, code2Value, operation)

          values += code -> result

          result
      }
    }

    evaluate(root)
  }


  def task2(): Long = {

    def opTotoString(operation: Operation) = operation match {
      case Plus => "+"
      case Minus => "-"
      case Multiply => "*"
      case Divide => "/"
    }

    sealed trait Formula
    case object X extends Formula
    case class Number(value: Long) extends Formula {
      override def toString: String = value.toString
    }
    case class ComplexFormula(f1: Formula, f2: Formula, operation: Operation) extends Formula {
      override def toString: String = s"($f1 ${opTotoString(operation)} $f2)"
    }

    val forms = mutable.Map.empty[String, Formula]

    forms += "humn" -> X

    def build(yell: Yell): Formula = {
      yell match {
        case Num(code, value) =>
          forms.get(code).getOrElse {
            forms += code -> Number(value)
            Number(value)
          }
        case Form(code, code1, code2, operation) =>
          forms.get(code).getOrElse {
            val f1 = forms.get(code1).getOrElse {
              build(yellMap(code1))
            }
            forms += code1 -> f1
            val f2 = forms.get(code2).getOrElse {
              build(yellMap(code2))
            }
            forms += code2 -> f2
            val result = (f1, f2) match {
              case (Number(n1), Number(n2)) => Number(applyOp(n1, n2, operation))
              case _ => ComplexFormula(f1, f2, operation)
            }
            forms += code -> result
            result
          }
      }
    }

    val form: Form = yellMap("root").asInstanceOf[Form]
    val yell1 = yellMap(form.code1)
    val yell2 = yellMap(form.code2)

    val formula1 = build(yell1)
    val formula2 = build(yell2)

    val (number, formula) = (formula1, formula2) match {
      case (Number(num), f) => (num, f)
      case (f, Number(num)) => (num, f)
    }

    var currentNumber: Long = number
    var currentFormula: Formula = formula
    while (currentFormula != X) {
      currentFormula match {
        // currentNumber = n op f2 =>
        case ComplexFormula(Number(n), f2, operation) =>
          currentFormula = f2
          operation match {
            case Plus => currentNumber -= n
            case Minus => currentNumber = n - currentNumber
            case Multiply => currentNumber = currentNumber / n
            case Divide => currentNumber = n / currentNumber
          }
        // currentNumber = f1 op n =>
        case ComplexFormula(f1, Number(n), operation) =>
          currentFormula = f1
          operation match {
            case Plus => currentNumber -= n
            case Minus => currentNumber = n + currentNumber
            case Multiply => currentNumber = currentNumber / n
            case Divide => currentNumber = n * currentNumber
          }

      }
    }

    currentNumber
  }

  println(task1()) // 276156919469632
  println(task2()) // 276156919469632


}


