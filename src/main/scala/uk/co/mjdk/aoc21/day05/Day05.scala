package uk.co.mjdk.aoc21.day05

import uk.co.mjdk.aoc.inputLines

import math.Ordered.orderingToOrdered

case class Pos(x: Int, y: Int)

given Ordering[Pos] = Ordering.by((p: Pos) => p.x).orElseBy(_.y)

case class Line(start: Pos, end: Pos) {

  val isAxisAligned: Boolean = start.x == end.x || start.y == end.y

  def coveredPoints: Iterator[Pos] = {
    val xStep = (end.x - start.x).sign
    val yStep = (end.y - start.y).sign

    Iterator
      .iterate(start)(p => Pos(p.x + xStep, p.y + yStep))
      .takeWhile(_ != end)
      .concat(Iterator(end))
  }
}

def parseLines: Iterator[Line] = inputLines(21)(5)
  .map(_.split(" -> ").map(_.split(',').map(_.toInt)))
  .map { case Array(Array(x1, y1), Array(x2, y2)) =>
    Line(Pos(x1, y1), Pos(x2, y2))
  }

// Use sparse map to count number of lines crossing each point
def countPoints(lines: Iterator[Line]): Int = lines
  .foldLeft(Map.empty[Pos, Int]) { case (map, line) =>
    line.coveredPoints.foldLeft(map) { case (map, point) =>
      map.updatedWith(point) {
        case None       => Some(1)
        case Some(prev) => Some(prev + 1)
      }
    }
  }
  .count(_._2 >= 2)

object Part1 {
  def main(args: Array[String]): Unit = {
    println(countPoints(parseLines.filter(_.isAxisAligned)))
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    println(countPoints(parseLines))
  }
}
