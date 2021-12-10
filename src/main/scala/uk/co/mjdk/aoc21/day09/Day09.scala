package uk.co.mjdk.aoc21.day09

import uk.co.mjdk.aoc21.inputLines

import scala.util.chaining._

// One could have an argument about using two Vectors rather than a map, but this is more convenient in this case
case class Pos(row: Int, col: Int) {
  def adjacentPositions: Set[Pos] = {
    def iter = Iterator.range(-1, 2)
    iter
      .map(r => copy(row = row + r))
      .concat(iter.map(c => copy(col = col + c)))
      .toSet
      .excl(this)
  }
}

def parseGrid: Map[Pos, Int] =
  inputLines(9).zipWithIndex
    .foldLeft(Map.empty[Pos, Int]) { case (map, (line, row)) =>
      line.iterator.zipWithIndex.foldLeft(map) { case (map, (digit, col)) =>
        map + (Pos(row, col) -> digit.asDigit)
      }
    }

def iterLowPoints(grid: Map[Pos, Int]): Iterator[(Pos, Int)] = grid.iterator
  .filter { case (pos, height) =>
    pos.adjacentPositions.iterator.flatMap(grid.get).forall(_ > height)
  }

object Part1 {
  def main(args: Array[String]): Unit = {
    val res = iterLowPoints(parseGrid).map { case (_, height) =>
      height + 1
    }.sum

    println(res)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val grid = parseGrid
    val lowPoints = iterLowPoints(grid).map(_._1).toSet

    // With the way the question is defined, we have a basin per low point, but also we don't need to worry
    // about ridges of less than 9 height, which makes things simpler. Also, we don't need to worry about tracking
    // visited cells between different basins.

    def countNeighbours(
        origPoint: Pos,
        visited: Set[Pos] = Set.empty
    ): (Int, Set[Pos]) =
      // DFS - we need to visit all points so type of search should not matter
      origPoint.adjacentPositions.iterator
        .flatMap(p => grid.get(p).map(p -> _))
        .filter(_._2 < 9)
        .foldLeft((1, visited + origPoint)) {
          case ((count, visited), (point, _)) if visited.contains(point) =>
            (count, visited)
          case ((count, visited), (point, _)) =>
            val (nCount, newVisited) =
              countNeighbours(point, visited)
            (count + nCount, newVisited)
        }

    val res = lowPoints.iterator
      .map(countNeighbours(_)._1)
      .toVector
      .sorted(Ordering[Int].reverse)
      .take(3)
      .product

    println(res)
  }
}
