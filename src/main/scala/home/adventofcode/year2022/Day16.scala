package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.util.concurrent.atomic.AtomicLong
import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Day16 extends App {

  val timeWindow = 30

  case class Valve(code: String)

  case class Elem(valves: List[Valve])

  implicit val ordering: Ordering[Elem] = new Ordering[Elem] {
    override def compare(x: Elem, y: Elem): Int = x.valves.size.compare(y.valves.size)
  }


  val (start: Valve, graph: Map[Valve, Set[Valve]], rates: Map[Valve, Int]) = {

    val lineRegex = """Valve (.*) has flow rate=(\d+); tunnel.? lead.? to valve.? (.*)""".r

    val graphMap = mutable.Map.empty[Valve, Set[Valve]]
    val ratesMap = mutable.Map.empty[Valve, Int]

    def parseLine(line: String): Unit = {
      line match {
        case lineRegex(from, rate, to) =>
          val toTunnels: Set[Valve] = to.split(",").map(_.trim).map(Valve(_)).toSet
          graphMap += Valve(from) -> toTunnels
          ratesMap += Valve(from) -> rate.toInt
      }
    }

    val lines: List[String] = InputLoader
//      .loadAsLines("inputs/day16.txt")
                  .loadAsLines("inputs/day16test.txt")
      .toList

    val start: Valve = lines.head match {
      case lineRegex(from, _, _) => Valve(from)
    }

    lines.foreach(parseLine)

    (start, graphMap.toMap, ratesMap.toMap)
  }

  val shortestPaths: Map[Valve, Map[Valve, List[Valve]]] =
    graph.keySet
      .map { v =>
        v -> calculateShortestPaths(v)
      }
      .toMap

  val nonZeroValves: Set[Valve] = rates.collect { case (v, r) if r > 0 => v }.toSet

  sealed trait Action

  case class Move(to: Valve) extends Action

  case object Open extends Action

  object Path {
    def create(): Path = Path(
      actions = List(Move(start)),
      lastValve = Some(start))
  }

  case class Path(
                   actions: List[Action],
                   openValves: Set[Valve] = Set.empty,
                   lastValve: Option[Valve] = None) {

    def add(action: Action): Path = action match {
      case Open => copy(
        actions = actions :+ action,
        openValves = openValves ++ actions.lastOption.collect { case Move(valve) => valve }.toSet)
      case Move(valve) => copy(actions = actions :+ action, lastValve = Some(valve))
    }
  }

  case class Path2(path1: Path, path2: Path)


  implicit val path2Ordering: Ordering[Path2] = new Ordering[Path2] {
    override def compare(x: Path2, y: Path2): Int =
      y.path1.actions.size.compare(x.path1.actions.size) // take short first
  }


  implicit val pathOrdering: Ordering[Path] = new Ordering[Path] {
    override def compare(x: Path, y: Path): Int =
      y.actions.size.compare(x.actions.size) // take short first
  }

  def children(path: Path): List[Path] = {

    val nextSubPathOptions: List[List[Action]] = path.lastValve
      .map { lastValve =>

        def shouldOpen(v: Valve): Boolean = rates(v) > 0 && !path.openValves.contains(v)

        // find the first steps towards closed valves
        val shortestPathsToClosedValves: List[List[Valve]] = {

          val remainingTime = timeWindow - path.actions.size + 1

          shortestPaths(lastValve)
            .collect { case (v, steps) if shouldOpen(v) => steps }
            .filter(_.size <= remainingTime) // which are still reachable
            .toList
        }

        shortestPathsToClosedValves.map { steps => steps.map(Move(_)) :+ Open }
      }
      .getOrElse(List.empty)
      .filterNot(_.isEmpty)

    nextSubPathOptions.map { steps =>
      steps.foldLeft(path) { (p, action) => p.add(action) }
    }
  }

  def children2(path: Path2): List[Path2] = {
    val c1 = children(path.path1)
    val c2 = children(path.path2)
    //println(s"Next paths ${c1.size} ${c2.size}")

    val possibilities: List[(Path, Path)] = for {
      p1 <- c1
      p2 <- c2
      if p1.lastValve != p2.lastValve
    } yield (p1, p2)

    possibilities
      .minByOption { case (path1, path2) => path1.actions.size + path2.actions.size }
      .map { case (path1, path2) => Path2(path1, path2) }
      .toList
  }


  def valueOf(path: Path2): Int = {
    valueOf(path.path1) + valueOf(path.path2)
  }

  def valueOf(path: Path): Int = {
    var releasedPressure: Int = 0
    val openValves = ListBuffer.empty[Valve]
    var current = start
    path.actions.foreach { action =>
      releasedPressure += openValves.map(rates).sum
      action match {
        case Open => openValves += current
        case Move(to) => current = to
      }
    }

    // remainingTime
    releasedPressure += openValves.map(rates).sum * (timeWindow - path.actions.length + 1)

    if (path.actions.lastOption.exists(_ == Open)) {
      releasedPressure += openValves.map(rates).sum
    }

    releasedPressure
  }

  def findBestPath(): Path = {
    val queue = new mutable.PriorityQueue[Path]()

    queue.enqueue(Path.create())

    val completed = ListBuffer.empty[Path]
    val contender = ListBuffer.empty[Path]

    val counter = new AtomicLong()

    var maxLen = 0
    var start = System.currentTimeMillis()

    while (!queue.isEmpty) {
      counter.incrementAndGet()
      val path = queue.dequeue()
      if (path.actions.size > maxLen) {
        maxLen = path.actions.size
        println(s"Level $maxLen ${System.currentTimeMillis() - start}. Num: ${queue.size}")
        start = System.currentTimeMillis()
      }
      if (path.openValves == nonZeroValves) {
        println(s"completed! ${completed.size}")
        completed += path
      }
      else if (path.actions.length == 31) {
        contender += path
        //println(s"Throwing out $path")
        // should have opened all valves, bye
      }
      else {
        val ps = children(path)
        if (ps.isEmpty) {
          contender += path
        }
        queue.enqueue(ps: _*)
      }
    }

    println(s"iterations: ${counter.get} contender ${contender.size} completed ${completed.size}")
    if (completed.nonEmpty)
      completed.maxBy(valueOf)
    else if (contender.nonEmpty)
      contender.maxBy(valueOf)
    else
      queue.toList.maxBy(valueOf)

  }

 def findBestPath2(): Path2 = {
    val queue = new mutable.PriorityQueue[Path2]()

    queue.enqueue(Path2(Path.create(), Path.create()))

    val completed = ListBuffer.empty[Path2]
    val contender = ListBuffer.empty[Path2]

    val counter = new AtomicLong()

    var maxLen = 0
    var start = System.currentTimeMillis()

    while (!queue.isEmpty) {
      counter.incrementAndGet()
      val path = queue.dequeue()
      if (path.path1.actions.size > maxLen) {
        maxLen = path.path1.actions.size
        println(s"Level $maxLen ${System.currentTimeMillis() - start}. Num: ${queue.size}")
        start = System.currentTimeMillis()
      }
      if ((path.path1.openValves ++ path.path2.openValves)  == nonZeroValves) {
        println(s"completed! ${completed.size}")
        completed += path
      }
      else if (path.path1.actions.length == 27) {
        contender += path
        //println(s"Throwing out $path")
        // should have opened all valves, bye
      }
      else {
        val ps = children2(path)
        if (ps.isEmpty) {
          contender += path
        }
        queue.enqueue(ps: _*)
      }
    }

    println(s"iterations: ${counter.get} contender ${contender.size} completed ${completed.size}")
    if (completed.nonEmpty)
      completed.maxBy(valueOf)
    else if (contender.nonEmpty)
      contender.maxBy(valueOf)
    else
      queue.toList.maxBy(valueOf)

  }

  def calculateShortestPaths(start: Valve): Map[Valve, List[Valve]] = {

    val queue = new mutable.PriorityQueue[Elem]()

    val paths = mutable.Map.empty[Valve, List[Valve]]
    paths += start -> List.empty
    queue.enqueue(Elem(List.empty))

    while (!queue.isEmpty) {
      val current = queue.dequeue()
      graph(current.valves.lastOption.getOrElse(start)).foreach { neighbour =>
        val oldDistance: Int = paths.get(neighbour).map(_.size).getOrElse(Int.MaxValue)
        if (oldDistance > current.valves.size + 1) {
          val newPath = current.valves :+ neighbour
          paths += neighbour -> newPath
          queue.enqueue(Elem(newPath))
        }
      }

    }

    paths.toMap

  }


  def task1(): Long = {
    println("Graph created")
    val bestPath = findBestPath()
    valueOf(bestPath)
  }


  def task2(): Long = {
    println("Graph created")
    val bestPath = findBestPath2()
    valueOf(bestPath)
  }

//  println(task1()) // 1724
  println(task2()) // 1724


}
