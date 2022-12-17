package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.util
import scala.collection.mutable.ListBuffer

object Day13 extends App {

  sealed trait Elem

  class ElemList(start: List[Elem] = List.empty) extends Elem {

    private val elems: ListBuffer[Elem] = ListBuffer.empty
    elems ++= start

    def add(elem: Elem): Unit = {
      elems += elem
    }

    def list(): List[Elem] = elems.toList

    override def toString(): String = s"[${elems.map(_.toString).mkString(",")}]"
  }

  case class Num(value: Int) extends Elem {
    override def toString(): String = value.toString
  }

  def compareTo(left: Elem, right: Elem): Int = {
    (left, right) match {
      case (Num(l), Num(r)) => r.compareTo(l)
      case (l: ElemList, r: ElemList) if l.list().isEmpty && r.list().isEmpty => 0
      case (l: ElemList, _) if l.list().isEmpty => 1
      case (_, r: ElemList) if r.list().isEmpty => -1
      case (l: ElemList, n: Num) => compareTo(l, new ElemList(List(n)))
      case (n: Num, _) => compareTo(new ElemList(List(n)), right)
      case (l: ElemList, r: ElemList) =>
        compareTo(l.list().head, r.list().head) match {
          case 1 => 1
          case -1 => -1
          case 0 => compareTo(new ElemList(l.list().drop(1)), new ElemList(r.list().drop(1)))
        }
    }

  }

  def parseElem(text: String): Elem = {
    val stack = new util.Stack[ElemList]
    stack.push(new ElemList())
    val numBuffer: ListBuffer[Char] = ListBuffer.empty
    text.toCharArray.foreach { c =>
      c match {
        case '[' =>
          val newOne = new ElemList()
          stack.peek().add(newOne)
          stack.push(newOne)
        case c if c.isDigit =>
          numBuffer += c
        case ',' =>
          if (numBuffer.nonEmpty) {
            stack.peek().add(Num(numBuffer.toList.mkString.toInt))
            numBuffer.clear()
          }
        case ']' =>
          if (numBuffer.nonEmpty) {
            stack.peek().add(Num(numBuffer.toList.mkString.toInt))
          }
          numBuffer.clear()
          stack.pop()
      }
    }
    val result = stack.pop().list().head

    if (result.toString != text) {
      sys.error(s"No match ${result} vs $text")
    }
    result
  }


  private val input: List[(Seq[String], Int)] = InputLoader
//    .loadAsLines("inputs/day13test.txt")
        .loadAsLines("inputs/day13.txt")
    .grouped(3)
    .zipWithIndex
    .toList

  def task1(): Int = {

    val idxs = input
      .flatMap { case (lines, idx) =>
        val left = parseElem(lines(0))
        val right = parseElem(lines(1))
        val orderOk = compareTo(left, right)
        if (orderOk > -1) Some(idx + 1) else None
      }

    idxs.sum
  }

  implicit val ElemOrdering: Ordering[Elem] = new Ordering[Elem] {
    override def compare(x: Elem, y: Elem): Int = -compareTo(x, y)
  }


  def task2(): Int = {

    val elems = input
      .flatMap { case (lines, idx) =>
        val left = parseElem(lines(0))
        val right = parseElem(lines(1))
        List(left, right)
      }

    val div2 = new ElemList(List(new ElemList(List(Num(2)))))
    val div6 = new ElemList(List(new ElemList(List(Num(6)))))
    val sortedElems = (elems ++ List(div2, div6)).sorted

    val div2Idx = sortedElems.zipWithIndex.collectFirst { case (e, idx) if e == div2 => idx + 1 }.get
    val div6Idx = sortedElems.zipWithIndex.collectFirst { case (e, idx) if e == div6 => idx + 1 }.get

    div2Idx * div6Idx
  }

  println(task1()) // 5938
  println(task2()) //
  println("done")


}
