package uk.co.mjdk.aoc15.day10

import scala.annotation.tailrec

val Input = "1321131112"

object Day10 {
  def main(args: Array[String]): Unit = {
    // no built-in chunk method on iterator - would be useful!
    @tailrec
    def process(
        input: Vector[Char],
        output: Vector[Char] = Vector.empty,
        currentChar: Option[Char] = None,
        currentCount: Int = 0
    ): Vector[Char] = {
      def dumpedOutput = output ++ currentChar.iterator.flatMap(c =>
        (currentCount.toString + c).toCharArray.toVector
      )

      input match {
        case Vector() => dumpedOutput
        case c +: rest if currentChar.contains(c) =>
          process(rest, output, currentChar, currentCount + 1)
        case c +: rest =>
          process(rest, dumpedOutput, Some(c), 1)
      }
    }

    val result =
      Iterator.iterate(Input.toCharArray.toVector)(process(_)).drop(40).next()
    println(result.length)
  }
}
