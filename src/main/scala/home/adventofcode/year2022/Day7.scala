package home.adventofcode.year2022

import home.adventofcode.InputLoader

import scala.collection.mutable.ListBuffer


sealed trait Node {
  def name: String
}

class Directory(val path: List[String]) extends Node {

  private val childrenList: ListBuffer[Node] = ListBuffer.empty

  def addChild(node: Node): Unit = {
    childrenList += node
  }

  def children(): Set[Node] = childrenList.toSet[Node]

  override def toString: String = name

  override def name: String = if (path.isEmpty) "/" else s"/${path.mkString("/")}"
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

  def find(root: Directory, path: List[String]): Option[Directory] = {
    if (root.path == path)
      Some(root)
    else {
      root.children()
        .flatMap {
          case d: Directory => find(d, path)
          case _: File => None
        }
        .headOption
    }
  }

  val dirPattern = """dir (.+)""".r
  val filePattern = """(\d+) (.+)""".r
  val cdDirPattern = """\$ cd (.+)""".r

  val input: List[String] = InputLoader
    .loadAsLines("inputs/day7.txt")
    .toList

  val input2: List[String] = """
    |$ cd /
    |$ ls
    |dir a
    |14848514 b.txt
    |8504156 c.dat
    |dir d
    |$ cd a
    |$ ls
    |dir e
    |29116 f
    |2557 g
    |62596 h.lst
    |$ cd e
    |$ ls
    |584 i
    |$ cd ..
    |$ cd ..
    |$ cd d
    |$ ls
    |4060174 j
    |8033020 d.log
    |5626152 d.ext
    |7214296 k
    |""".stripMargin.split("\n").filterNot(_.isBlank).toList

  val root: Directory = new Directory(List.empty)
  var currentFolder: Directory = root

  def parseLine(line: String): Unit = {
    line match {
      case "$ ls" => // ignore
      case "$ cd /" => currentFolder = root
      case "$ cd .." =>
        find(root, currentFolder.path.dropRight(1)) match {
          case Some(parent) => currentFolder = parent
          case _ => sys.error(s"Parent of ${currentFolder.name} not found")
        }
      case filePattern(size, name) => currentFolder.addChild(File(name, size.toInt))
      case dirPattern(name) =>
        val path = currentFolder.path ++ List(name)
        currentFolder.addChild(new Directory(path))
      case cdDirPattern(name) =>
        currentFolder.children()
          .toList
          .collect { case dir: Directory if dir.path.lastOption.exists(_ == name) => dir } match {
          case List(dir) => currentFolder = dir
          case _: List[Node] =>
            sys.error(s"Multiple dirs [$name] found in [${currentFolder.name}] ")
          case _ =>
            sys.error(s"Dir [$name] not found in [${currentFolder.name}] ")
        }
      case _ => println(s"Ignored [$line]")
    }
  }

  input.foreach(parseLine)

  val dirs = findDirs(root)


  println(task1()) // 1077191
  println(task2()) // 2746

  def task1(): Long = {
    dirs
      .toList
      .map(size)
      .filter(_ <= 100000)
      .sum
  }

  // Task 2
  def task2(): Int = {
    val freeSpace = 70000000 - size(root)
    val neededSpace = 30000000 - freeSpace

    dirs
      .toList
      .map(size)
      .sorted
      .find(_ >= neededSpace)
      .get


  }
}
