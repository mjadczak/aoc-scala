package uk.co.mjdk.aoc15

import uk.co.mjdk.aoc.inputLines
import scala.util.chaining._

case class Pos(ns: Int, ew: Int) {
  def moved(char: Char): Pos = char match {
    case '<' => copy(ew = ew - 1)
    case '>' => copy(ew = ew + 1)
    case '^' => copy(ns = ns + 1)
    case 'v' => copy(ns = ns - 1)
  }
}

object Part1 {
  def main(args: Array[String]): Unit = {
    inputLines(15)(3)
      .next()
      .iterator
      .scanLeft(Pos(0, 0)) { (pos, char) =>
        pos.moved(char)
      }
      .toSet
      .size
      .pipe(println)
  }
}
