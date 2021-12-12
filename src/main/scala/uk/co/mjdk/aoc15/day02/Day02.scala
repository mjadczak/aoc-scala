package uk.co.mjdk.aoc15.day02

import uk.co.mjdk.aoc.inputLines
import scala.util.chaining._

case class Present(length: Int, width: Int, height: Int) {
  val sideAreas: Vector[Int] = Vector(
    length * width,
    width * height,
    height * length
  )

  def paperRequired: Int =
    sideAreas.sum * 2 + sideAreas.min

  def ribbonRequired: Int =
    (Vector(length, width, height).sorted
      .take(2)
      .sum * 2) + length * width * height
}

def parsePresents: Iterator[Present] = inputLines(15)(2).map { line =>
  val Array(l, w, h) = line.split('x')
  Present(l.toInt, w.toInt, h.toInt)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    parsePresents
      .map(_.paperRequired)
      .sum
      .pipe(println)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    parsePresents
      .map(_.ribbonRequired)
      .sum
      .pipe(println)
  }
}
