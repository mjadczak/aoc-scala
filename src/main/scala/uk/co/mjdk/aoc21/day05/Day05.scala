package uk.co.mjdk.aoc21.day05

import uk.co.mjdk.aoc21.inputLines

case class Pos(x: Int, y: Int)
case class Line(start: Pos, end: Pos) {

  val isAxisAligned: Boolean = start.x == end.x || start.y == end.y

  def coveredPoints: Iterator[Pos] = {
    // Crying out for abstraction. Lenses? General case?
    if (start.x == end.x) {
      val (startPt, endPt) =
        (Seq(start, end).minBy(_.y), Seq(start, end).maxBy(_.y))
      Iterator
        .iterate(startPt)(p => p.copy(y = p.y + 1))
        .takeWhile(_.y <= endPt.y)
    } else if (start.y == end.y) {
      val (startPt, endPt) =
        (Seq(start, end).minBy(_.x), Seq(start, end).maxBy(_.x))
      Iterator
        .iterate(startPt)(p => p.copy(x = p.x + 1))
        .takeWhile(_.x <= endPt.x)
    } else {
      throw new IllegalStateException("Non-axis-aligned line")
    }
  }
}

object Part1 {
  def main(args: Array[String]): Unit = {
    // Use sparse map to count number of lines crossing each point
    val pointMap = inputLines(5)
      .map(_.split(" -> ").map(_.split(',').map(_.toInt)))
      .map { case Array(Array(x1, y1), Array(x2, y2)) =>
        Line(Pos(x1, y1), Pos(x2, y2))
      }
      .filter(_.isAxisAligned)
      .foldLeft(Map.empty[Pos, Int]) { case (map, line) =>
        line.coveredPoints.foldLeft(map) { case (map, point) =>
          map.updatedWith(point) {
            case None       => Some(1)
            case Some(prev) => Some(prev + 1)
          }
        }
      }

    println(pointMap.count(_._2 >= 2))
  }
}
