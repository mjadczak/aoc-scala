package uk.co.mjdk.aoc21.day15

import uk.co.mjdk.aoc.inputLines

import java.util
import java.util.{PriorityQueue, Comparator}
import scala.jdk.javaapi.OptionConverters._

// Dijkstra time! We need a priority queue, and there is no immutable one in Scala's stdlib.
// There is a mutable one, but it does not support changing priority or removing a specific element.
// We could write our own immutable priority queue (maybe a Bucket Queue for this case?) but let's be practical and
// just use the Java PriorityQueue. In general Scala's mutable collections are not great and one usually is
// better off using Java ones.

case class Pos(row: Int, col: Int) {
  def adjacentPositions: Set[Pos] = {
    (-1 to 1)
      .map(r => copy(row = row + r))
      .concat((-1 to 1).map(c => copy(col = col + c)))
      .toSet
      .excl(this)
  }
}

def parseGrid: (util.Map[Pos, Int], Pos) = {
  val map = new util.HashMap[Pos, Int]()
  var maxCol = 0
  var maxRow = 0
  inputLines(21)(15).zipWithIndex
    .foreach { case (line, row) =>
      maxRow = maxRow.max(row)
      line.iterator.zipWithIndex.foreach { case (digit, col) =>
        maxCol = maxCol.max(col)
        map.put(Pos(row, col), digit.asDigit)
      }
    }
  (map, Pos(maxRow, maxCol))
}

object Day15 {
  def main(args: Array[String]): Unit = {
    val startPos = Pos(0, 0)
    val (grid, targetPos) = parseGrid
    val distances = new util.HashMap[Pos, Int]()
    val pq =
      new PriorityQueue[(Pos, Int)](
        grid.size(),
        Comparator.comparingInt[(Pos, Int)](_._2)
      )
    grid.keySet().forEach { pos =>
      if (pos != startPos) {
        pq.add((pos, Int.MaxValue))
        distances.put(pos, Int.MaxValue)
      }
    }
    pq.add((startPos, 0))
    distances.put(startPos, 0)
    val visitedNodes = new util.HashSet[Pos]()
    while (!visitedNodes.contains(targetPos)) {
      val (curNode, curMinDistance) = pq.poll()
      curNode.adjacentPositions
        .filter(grid.containsKey)
        .filterNot(visitedNodes.contains)
        .foreach { adjacentNode =>
          val tentativeDistance = distances.get(adjacentNode)
          val candidateDistance = curMinDistance + grid.get(adjacentNode)
          if (candidateDistance < tentativeDistance) {
            distances.put(adjacentNode, candidateDistance)
            pq.remove(
              (adjacentNode, tentativeDistance)
            ) // This is not great - a linear scan
            pq.add((adjacentNode, candidateDistance))
          }
        }
      visitedNodes.add(curNode)
    }
    println(distances.get(targetPos))
  }
}
