package uk.co.mjdk.aoc15.day09

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// TSP! We want the exact answer, so no shortcuts and heuristics - thankfully the input is small

// Could be more efficient with a matrix - but efficiency isn't really the name of the game here anyway
case class Graph(distances: Map[(String, String), Int], nodes: Set[String]) {
  def distanceBetween(l: String, r: String): Option[Int] = {
    val Vector(min, max) = Vector(l, r).sorted
    distances.get((min, max))
  }

  def withDistance(l: String, r: String, distance: Int): Graph = {
    val Vector(min, max) = Vector(l, r).sorted
    copy(
      distances = distances + ((min, max) -> distance),
      nodes = nodes + l + r
    )
  }
}

object Graph {
  val empty: Graph = Graph(Map(), Set())
}

case class Path(currentNode: String, usedNodes: Set[String], distance: Int) {
  def withNext(node: String, nextDistance: Int): Path = copy(
    currentNode = node,
    usedNodes = usedNodes + node,
    distance = distance + nextDistance
  )
}

def parseInput: Graph = {
  val InputPat = """(\w+) to (\w+) = (\d+)""".r

  inputLines(15)(9)
    .foldLeft(Graph.empty) { case (graph, line) =>
      val InputPat(from, to, dist) = line
      graph.withDistance(from, to, dist.toInt)
    }
}

object Day09 {
  def main(args: Array[String]): Unit = {
    val graph = parseInput
    val initialPaths =
      graph.nodes.iterator.map(node => Path(node, Set(node), 0)).to(Queue)

    // we need to traverse the whole space, so DFS vs BFS doesn't matter
    @tailrec
    def process(
        inProgressPaths: Queue[Path],
        finishedPaths: Vector[Path] = Vector.empty
    ): Vector[Path] = inProgressPaths.dequeueOption match {
      case None => finishedPaths
      case Some((path, queueRest)) if path.usedNodes == graph.nodes =>
        process(
          queueRest,
          finishedPaths :+ path
        )
      case Some((path, queueRest)) =>
        // find all possible next steps
        val nextPaths =
          graph.nodes
            .diff(path.usedNodes)
            .iterator
            .flatMap { node =>
              graph
                .distanceBetween(path.currentNode, node)
                .map(dist => path.withNext(node, dist))
            }
            .toVector
        process(
          queueRest.enqueueAll(nextPaths),
          finishedPaths
        )
    }

    val allPaths = process(initialPaths)
    println(s"MIN = ${allPaths.minBy(_.distance).distance}")
    println(s"MAX = ${allPaths.maxBy(_.distance).distance}")
  }
}
