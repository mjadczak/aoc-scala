package uk.co.mjdk.aoc21.day12

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.collection.immutable.Queue

enum Cave {
  case Start
  case End
  case Small(name: String)
  case Big(name: String)
}

object Cave {
  def parse(name: String): Cave = name match {
    case "start"                                    => Cave.Start
    case "end"                                      => Cave.End
    case name if name.toCharArray.forall(_.isUpper) => Cave.Big(name)
    case name if name.toCharArray.forall(_.isLower) => Cave.Small(name)
    case n => throw new IllegalArgumentException(s"Unexpected cave name $n")
  }
}

// All we need is an adjacency map - undirected
def parseGraph: Map[Cave, Set[Cave]] = {
  // This is why monoids are useful
  def updatedWithAdjacency(
      other: Cave
  ): Option[Set[Cave]] => Option[Set[Cave]] = {
    case None    => Some(Set(other))
    case Some(s) => Some(s + other)
  }

  inputLines(21)(12)
    .map { line =>
      val Array(leftName, rightName) = line.split('-')
      (Cave.parse(leftName), Cave.parse(rightName))
    }
    .foldLeft(Map.empty[Cave, Set[Cave]]) { case (map, (left, right)) =>
      map
        .updatedWith(left)(updatedWithAdjacency(right))
        .updatedWith(right)(updatedWithAdjacency(left))
    }
}

trait Path[P <: Path[P]] {
  def finalPath: Vector[Cave]
  def currentCave: Cave
  def canVisit(cave: Cave): Boolean
  def isFinished: Boolean
  def withNext(cave: Cave): P
}

case class Path1(path: List[Cave], visitedSmallCaves: Set[Cave])
    extends Path[Path1] {
  def finalPath: Vector[Cave] = path.reverseIterator.toVector
  def currentCave: Cave = path.head
  def canVisit(cave: Cave): Boolean =
    cave != Cave.Start && !visitedSmallCaves.contains(cave)
  def isFinished: Boolean = currentCave == Cave.End
  def withNext(cave: Cave): Path1 = cave match {
    case c @ Cave.Small(_) =>
      copy(path = c :: path, visitedSmallCaves = visitedSmallCaves + c)
    case c => copy(path = c :: path)
  }
}

object Path1 {
  def initial: Path1 = Path1(List(Cave.Start), Set.empty)
}

@tailrec
def calculatePaths[P <: Path[P]](
    graph: Map[Cave, Set[Cave]],
    pathsInProgress: Queue[P],
    finishedPaths: Set[P] = Set.empty[P]
): Set[P] = pathsInProgress.dequeueOption match {
  case None => finishedPaths
  case Some((path, queueRest)) =>
    val (finished, ongoing) = graph(path.currentCave)
      .filter(path.canVisit)
      .map(path.withNext)
      .partition(_.isFinished)

    calculatePaths(
      graph,
      queueRest.enqueueAll(ongoing),
      finishedPaths ++ finished
    )
}

object Part1 {
  def main(args: Array[String]): Unit = {
    println(calculatePaths(parseGraph, Queue(Path1.initial)).size)
  }
}

case class Path2(
    path: List[Cave],
    visitedSmallCaves: Set[Cave],
    extraSmallCave: Option[Cave]
) extends Path[Path2] {
  def finalPath: Vector[Cave] = path.reverseIterator.toVector
  def currentCave: Cave = path.head
  def canVisit(cave: Cave): Boolean =
    cave != Cave.Start &&
      (!visitedSmallCaves.contains(cave) || extraSmallCave.isEmpty)
  def isFinished: Boolean = currentCave == Cave.End
  def withNext(cave: Cave): Path2 = cave match {
    case c @ Cave.Small(_) if !visitedSmallCaves.contains(c) =>
      copy(path = c :: path, visitedSmallCaves = visitedSmallCaves + c)
    case c @ Cave.Small(_)
        if visitedSmallCaves.contains(c) && extraSmallCave.isEmpty =>
      copy(path = c :: path, extraSmallCave = Some(c))
    case c @ Cave.Small(_) =>
      throw new IllegalStateException(s"Cannot visit $c")
    case c => copy(path = c :: path)
  }
}

object Path2 {
  def initial: Path2 = Path2(List(Cave.Start), Set.empty, None)
}

object Part2 {
  def main(args: Array[String]): Unit = {
    println(calculatePaths(parseGraph, Queue(Path2.initial)).size)
  }
}
