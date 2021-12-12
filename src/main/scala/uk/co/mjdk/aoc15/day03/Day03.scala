package uk.co.mjdk.aoc15.day03

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

object Pos {
  val zero: Pos = Pos(0, 0)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    inputLines(15)(3)
      .next()
      .iterator
      .scanLeft(Pos.zero) { (pos, char) =>
        pos.moved(char)
      }
      .toSet
      .size
      .pipe(println)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    inputLines(15)(3)
      .next()
      .iterator
      .zipWithIndex
      .scanLeft((Pos.zero, Pos.zero)) { case ((pos1, pos2), (char, idx)) =>
        if (idx % 2 == 0) {
          (pos1.moved(char), pos2)
        } else {
          (pos1, pos2.moved(char))
        }
      }
      .flatMap { case (pos1, pos2) => Iterator(pos1, pos2) }
      .toSet
      .size
      .pipe(println)
  }
}
