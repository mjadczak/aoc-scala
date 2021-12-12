package uk.co.mjdk.aoc15.day01

import uk.co.mjdk.aoc._
import scala.util.chaining._

def inputAsNumbers: Iterator[Int] = inputLines(15)(1)
  .next()
  .iterator
  .map {
    case '(' => 1
    case ')' => -1
  }

object Part1 {
  def main(args: Array[String]): Unit = {
    inputAsNumbers.sum
      .pipe(println)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    inputAsNumbers
      .scanLeft(0)(_ + _)
      .zipWithIndex
      .find(_._1 == -1)
      .get
      ._2
      .pipe(println)
  }
}
