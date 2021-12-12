package uk.co.mjdk.aoc21.day12

import uk.co.mjdk.aoc21.inputLines

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

  inputLines(12)
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

case class Path(path: List[Cave], visitedSmallCaves: Set[Cave]) {
  def finalPath: Vector[Cave] = path.reverseIterator.toVector
  def currentCave: Cave = path.head
  def isFinished: Boolean = currentCave == Cave.End
  def withNext(cave: Cave): Path = cave match {
    case c @ Cave.Small(_) =>
      copy(path = c :: path, visitedSmallCaves = visitedSmallCaves + c)
    case c => copy(path = c :: path)
  }
}

@tailrec
def calculatePaths(
    graph: Map[Cave, Set[Cave]],
    pathsInProgress: Queue[Path] = Queue(Path(List(Cave.Start), Set.empty)),
    finishedPaths: Set[Path] = Set.empty
): Set[Path] = pathsInProgress.dequeueOption match {
  case None => finishedPaths
  case Some((path, queueRest)) =>
    val (finished, ongoing) = graph(path.currentCave)
      .excl(Cave.Start)
      .filterNot(path.visitedSmallCaves)
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
    println(calculatePaths(parseGraph).size)
  }
}
