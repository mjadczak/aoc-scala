package uk.co.mjdk.aoc21.day01

import uk.co.mjdk.aoc.inputLines

import scala.io.Source
import scala.util.Using

object Part1 {
  def main(args: Array[String]): Unit = {
    println(
      inputLines(21)(1)
        .map(_.toInt)
        .sliding(2)
        .map {
          case Seq(a, b) if b > a => 1
          case _                  => 0
        }
        .sum
    )

  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    println(
      inputLines(21)(1)
        .map(_.toInt)
        .sliding(3)
        .map(_.sum)
        .sliding(2)
        .map {
          case Seq(a, b) if b > a => 1
          case _                  => 0
        }
        .sum
    )
  }
}
