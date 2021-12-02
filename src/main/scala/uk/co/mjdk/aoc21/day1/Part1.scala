package uk.co.mjdk.aoc21.day1

import scala.io.Source
import scala.util.Using

object Part1 {
  def main(args: Array[String]): Unit = {
    val sum = Using.resource(
      Source.fromInputStream(getClass.getResourceAsStream("part1.txt"))
    ) { src =>
      src
        .getLines()
        .map(_.toInt)
        .sliding(2)
        .map {
          case Seq(a, b) if b > a => 1
          case _                  => 0
        }
        .sum
    }

    println(sum)
  }
}
