package uk.co.mjdk.aoc21.day09

import uk.co.mjdk.aoc21.inputLines

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
    val grid = parseGrid

    val res = iterLowPoints(grid).map { case (_, height) =>
      height + 1
    }.sum

    println(res)
  }
}
