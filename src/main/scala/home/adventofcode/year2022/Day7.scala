package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.collection.mutable.ListBuffer


sealed trait Node {
  def name: String
}

class Directory(override val name: String) extends Node {
  private val childrenList: ListBuffer[Node] = ListBuffer.empty
  def addChild(node: Node): Unit = {
    childrenList += node
  }

  def children(): Set[Node] = childrenList.toSet[Node]

  override def toString: String = name
}

case class File(override val name: String, size: Int) extends Node

object Day7 extends App {

  def size(node: Node): Int = node match {
    case d: Directory => d.children().map(size).sum
    case File(_, size) => size
  }

  def findDirs(dir: Directory): Set[Directory] = {
    Set(dir) ++ dir.children().flatMap {
      case d: Directory => findDirs(d)
      case _ => Set.empty
    }
  }

  def findParent(root: Directory, dir: Directory): Option[Directory] = {
    val children: Set[Node] = root.children()
    if (children.contains(dir))
      Some(root)
    else
      children
        .flatMap {
          case d: Directory => findParent(d, dir)
          case _: File => None
        }
        .headOption
  }

  val dirPattern = """dir (.+)""".r
  val filePattern = """(\d+) (.+)""".r
  val cdDirPattern = """\$ cd (.+)""".r

  val input: List[String] = InputLoader
    .loadAsLines("inputs/day7.txt")
    .toList

//  val input: List[String] = """
//    |$ cd /
//    |$ ls
//    |dir a
//    |14848514 b.txt
//    |8504156 c.dat
//    |dir d
//    |$ cd a
//    |$ ls
//    |dir e
//    |29116 f
//    |2557 g
//    |62596 h.lst
//    |$ cd e
//    |$ ls
//    |584 i
//    |$ cd ..
//    |$ cd ..
//    |$ cd d
//    |$ ls
//    |4060174 j
//    |8033020 d.log
//    |5626152 d.ext
//    |7214296 k""".stripMargin.split("\n").toList


  println(task1()) // 1027500 -- too low!!!
  println(task2()) // 2746

  def task1(): Int = {

    val root: Directory = new Directory("/")
    var currentFolder: Directory = root

    def parseLine(line: String): Unit = {
      line match {
        case "$ cd /" => currentFolder = root
        case "$ cd .." =>
          findParent(root, currentFolder) match {
            case Some(parent) => currentFolder = parent
            case _ => sys.error(s"Parent of ${currentFolder.name} not found")
          }
        case filePattern(size, name) => currentFolder.addChild(File(name, size.toInt))
        case cdDirPattern(name) =>
          if (currentFolder.children().map(_.name).contains(name)) {
            sys.error(s"hey I have it already [${currentFolder.name}] [$name]")
          }
          val d = new Directory(name)
          currentFolder.addChild(d)
          currentFolder = d
        case _ => // ignore
      }
    }

    input.foreach(parseLine)

    findDirs(root).map(size).filter(_ <= 100000).sum
  }

  // Task 2
  def task2(): Int = {
    0
  }
}
