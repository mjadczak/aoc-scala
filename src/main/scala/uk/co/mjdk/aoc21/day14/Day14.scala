package uk.co.mjdk.aoc21.day14

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec

def parseInput: (String, Map[(Char, Char), Char]) = {
  val iter = inputLines(21)(14)
  val in = iter.next()
  val replacements = iter
    .dropWhile(_.isEmpty)
    .map { line =>
      val Array(src, target) = line.split(" -> ")
      val Array(a, b) = src.toCharArray
      val Array(c) = target.toCharArray
      (a, b) -> c
    }
    .toMap

  (in, replacements)
}

def processReplacements(
    inputS: Vector[Char],
    replacements: Map[(Char, Char), Char]
): Vector[Char] = {
  @tailrec
  def process(
      input: Vector[Char],
      output: Vector[Char] = Vector.empty
  ): Vector[Char] = input match {
    case Vector() => output
    case a +: (rest @ b +: _) if replacements.contains((a, b)) =>
      process(rest, output :+ a :+ replacements((a, b)))
    case a +: rest =>
      process(rest, output :+ a)
  }

  process(inputS)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val (input, replacements) = parseInput
    val result = Iterator
      .iterate(input.toCharArray.toVector)(processReplacements(_, replacements))
      .drop(10)
      .next()

    val grouped = result.groupMapReduce(identity)(_ => 1)(_ + _)
    val min = grouped.valuesIterator.min
    val max = grouped.valuesIterator.max
    println(max - min)
  }
}
