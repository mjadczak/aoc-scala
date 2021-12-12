package uk.co.mjdk.aoc15.day01

import uk.co.mjdk.aoc._
import scala.util.chaining._

object Part1 {
  def main(args: Array[String]): Unit = {
    inputLines(15)(1)
      .next()
      .iterator
      .map {
        case '(' => 1
        case ')' => -1
      }
      .sum
      .pipe(println)

  }
}
