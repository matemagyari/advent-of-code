package home.adventofcode.year2022

import home.adventofcode.InputLoader

import java.util.concurrent.atomic.AtomicLong
import scala.collection.{immutable, mutable}
import scala.collection.mutable.ListBuffer

object Day16 extends App {

  val timeWindow = 30

  val start = Valve("AA")

  case class Valve(code: String)

  case class Elem(valves: List[Valve])

  implicit val ordering: Ordering[Elem] = new Ordering[Elem] {
    override def compare(x: Elem, y: Elem): Int = x.valves.size.compare(y.valves.size)
  }

  val (graph: Map[Valve, Set[Valve]], rates: Map[Valve, Int]) = {

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

    InputLoader
                  .loadAsLines("inputs/day16.txt")
//      .loadAsLines("inputs/day16test.txt")
      .map(parseLine)
      .toSet

    (graphMap.toMap, ratesMap.toMap)
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
                   lastValve: Option[Valve] = None,
                   lastOpenedValveIdx: Option[Int] = None) {

    lazy val walkSinceLastOpening: List[Valve] =
      actions.drop(lastOpenedValveIdx.getOrElse(0)).collect { case Move(valve) => valve }

    def add(action: Action): Path = action match {
      case Open => copy(
        actions = actions :+ action,
        openValves = openValves ++ actions.lastOption.collect { case Move(valve) => valve }.toSet,
        lastOpenedValveIdx = Some(actions.length - 1))
      case Move(valve) => copy(actions = actions :+ action, lastValve = Some(valve))
    }
  }

  implicit val pathOrdering: Ordering[Path] = new Ordering[Path] {
    override def compare(x: Path, y: Path): Int =
      y.actions.size.compare(x.actions.size) // take short first
//          x.actions.size.compare(y.actions.size)
  }

  def children(path: Path): List[Path] = {

    val nextSubPathOptions: List[List[Action]] = path.lastValve
      .map { lastValve =>

        def shouldOpen(v: Valve): Boolean = rates(v) > 0 && !path.openValves.contains(v)

        val openAction: Option[Action] = if (shouldOpen(lastValve)) Some(Open) else None

        val moveActions: List[List[Action]] = {

          // find the first steps towards closed valves
          val shortestPathsToClosedValves: List[List[Valve]] = {

            val alreadyVisitedSinceLastOpen = path.walkSinceLastOpening.toSet
            val remainingTime = timeWindow - path.actions.size + 1

            shortestPaths(lastValve)
              .collect { case (v, steps) if shouldOpen(v) => steps }
              .filter(_.size <= remainingTime) // which are still reachable
              .toList
              .filterNot { steps => steps.headOption.exists(alreadyVisitedSinceLastOpen.contains) }
          }

          shortestPathsToClosedValves.map { steps => steps.map(Move(_)) }
        }

        List(openAction.toList) ++ moveActions
      }
      .getOrElse(List.empty)
      .filterNot(_.isEmpty)

    nextSubPathOptions.map { steps =>
      steps.foldLeft(path) { (p, action) => p.add(action) }
    }
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

    releasedPressure
  }

  def findBestPath(): Path = {
    val queue = new mutable.PriorityQueue[Path]()

    queue.enqueue(Path.create())

    val completed = ListBuffer.empty[Path]

    val counter = new AtomicLong()

    var maxLen = 0
    var start = System.currentTimeMillis()

    var maxSoFar = 0

    while (!queue.isEmpty) {
      counter.incrementAndGet()
      val path = queue.dequeue()
      if (path.actions.size > maxLen) {
        maxLen = path.actions.size
        println(s"Level $maxLen ${System.currentTimeMillis() - start}. Num: ${queue.size}")
        counter.set(0)
        start = System.currentTimeMillis()
      }
      if (path.openValves == nonZeroValves) {
        println(s"completed! ${completed.size}")
        completed += path
      } else if (path.actions.length == 31) {
        val w = valueOf(path)
        if (w > maxSoFar) {
          maxSoFar = w
          println(s"Max: $maxSoFar")
        }
        //println(s"Throwing out $path")
        // should have opened all valves, bye
      }
      else {
        val ps = children(path)
        queue.enqueue(ps: _*)
      }
    }

    println(s"iterations: ${counter.get}")
    if (completed.nonEmpty)
      completed.maxBy(valueOf)
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

    1
  }

  println(task1()) //
  //  println(task2()) // Position(3156345,3204261) = 12625383204261


}
