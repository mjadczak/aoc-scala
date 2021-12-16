package uk.co.mjdk.aoc21.day15

import uk.co.mjdk.aoc.inputLines

import java.util
import java.util.{Comparator, PriorityQueue}
import scala.annotation.tailrec
import scala.jdk.javaapi.OptionConverters.*

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

def parseGrid: (Map[Pos, Int], Pos) = {
  val (map, maxRow, maxCol) = inputLines(21)(15).zipWithIndex
    .foldLeft((Map.empty[Pos, Int], 0, 0)) {
      case ((map, maxRow, maxCol), (line, row)) =>
        val (map2, maxCol2) = line.iterator.zipWithIndex.foldLeft((map, 0)) {
          case ((map, maxCol), (digit, col)) =>
            (map + (Pos(row, col) -> digit.asDigit), maxCol.max(col))
        }
        (map2, maxRow.max(row), maxCol2.max(maxCol))
    }
  (map, Pos(maxRow, maxCol))
}

class VirtualGrid(
    val rawGrid: Map[Pos, Int],
    val rawMaxPos: Pos,
    val expansionFactor: Int = 1
) {
  require(expansionFactor >= 1)
  private val rawWidth = rawMaxPos.row + 1
  private val rawHeight = rawMaxPos.col + 1

  @tailrec
  private def wrapValue(value: Int): Int = {
    if (value <= 9) {
      value
    } else {
      wrapValue(value - 9)
    }
  }

  def contains(pos: Pos): Boolean =
    pos.row <= maxPos.row && pos.col <= maxPos.col && pos.row >= 0 && pos.col >= 0
  def apply(pos: Pos): Int = {
    if (!contains(pos)) {
      throw new NoSuchElementException(f"No entry at $pos")
    }
    val rowFactor = pos.row / rawWidth
    val rowCoord = pos.row % rawWidth
    val colFactor = pos.col / rawHeight
    val colCoord = pos.col % rawHeight
    wrapValue(rawGrid(Pos(rowCoord, colCoord)) + rowFactor + colFactor)
  }
  def positions: Iterator[Pos] = 0.to(maxPos.row).iterator.flatMap { row =>
    0.to(maxPos.col).iterator.map { col =>
      Pos(row, col)
    }
  }
  val maxPos: Pos =
    Pos(rawWidth * expansionFactor - 1, rawHeight * expansionFactor - 1)
}

def findLowestCost(grid: VirtualGrid): Int = {
  val startPos = Pos(0, 0)
  val distances = new util.HashMap[Pos, Int]()
  val targetPos = grid.maxPos
  val pq =
    new PriorityQueue[(Pos, Int)](
      Comparator.comparingInt[(Pos, Int)](_._2)
    )
  grid.positions.foreach { pos =>
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
      .filter(grid.contains)
      .filterNot(visitedNodes.contains)
      .foreach { adjacentNode =>
        val tentativeDistance = distances.get(adjacentNode)
        val candidateDistance = curMinDistance + grid(adjacentNode)
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
  distances.get(targetPos)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val (rawGrid, maxPos) = parseGrid
    val grid = VirtualGrid(rawGrid, maxPos)
    println(findLowestCost(grid))
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val (rawGrid, maxPos) = parseGrid
    val grid = VirtualGrid(rawGrid, maxPos, 5)
    println(findLowestCost(grid))
  }
}
