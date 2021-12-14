package uk.co.mjdk.aoc21.day14

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec

type CharPair = (Char, Char)

def parseInput: (String, Map[CharPair, Char]) = {
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
    replacements: Map[CharPair, Char]
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
  print(".")
  process(inputS)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val (input, replacements) = parseInput
    val result = Iterator
      .iterate(input.toCharArray.toVector)(processReplacements(_, replacements))
      .drop(10)
      .next()

    val grouped = result.groupMapReduce(identity)(_ => 1L)(_ + _)
    val min = grouped.valuesIterator.min
    val max = grouped.valuesIterator.max
    println(max - min)
  }
}

// The mechanical way is too slow for 40 iterations!
// Instead, we simulate the process by keeping track of the count of pairs and count of letters

object Part2 {
  def main(args: Array[String]): Unit = {
    val (input, replacements) = parseInput
    val fullReplacements: Map[CharPair, (CharPair, CharPair)] =
      replacements.map { case (a, b) -> c =>
        (a, b) -> ((a, c), (c, b))
      }

    val initialPairs: Map[CharPair, Long] =
      input.iterator
        .sliding(2)
        .withPartial(false)
        .map { case Seq(a, b) =>
          (a, b)
        }
        .toVector
        .groupMapReduce(identity)(_ => 1L)(_ + _)

    val counts = Iterator
      .iterate(initialPairs) { pairs =>
        pairs.iterator
          .flatMap { case (a, b) -> count =>
            fullReplacements.get((a, b)) match {
              case None           => Iterator((a, b) -> count)
              case Some((ra, rb)) => Iterator(ra -> count, rb -> count)
            }
          }
          .foldLeft(Map.empty[CharPair, Long]) { case (map, (pair, count)) =>
            map.updatedWith(pair) {
              case None    => Some(count)
              case Some(c) => Some(c + count)
            }
          }
      }
      .drop(40)
      .next()

    // count all the letters
    val characterCounts = counts.iterator
      .flatMap { case (a, b) -> count =>
        Iterator(a -> count, b -> count)
      }
      // The first and last letters were undercounted
      .concat(Iterator(input.head -> 1L, input.last -> 1L))
      .toVector
      .groupMapReduce(_._1)(_._2)(_ + _)

    // Now everything is counted twice

    val min = characterCounts.valuesIterator.min
    val max = characterCounts.valuesIterator.max
    println((max - min) / 2)

  }
}
