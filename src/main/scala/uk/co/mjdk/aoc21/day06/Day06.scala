package uk.co.mjdk.aoc21.day06

import uk.co.mjdk.aoc21.inputLines

def fishAfterNDays(n: Int): Long = {
  // Represent the number of lanternfish with each timer value by using a map
  val initialState =
    inputLines(6)
      .next()
      .split(',')
      .toVector
      .map(_.toInt)
      .groupMapReduce(identity)(_ => 1L)(_ + _)

  Iterator
    .iterate(initialState) { fish =>
      fish.foldLeft(Map.empty[Int, Long]) { case (newFish, timer -> numFish) =>
        val update: Option[Long] => Option[Long] =
          v => Some(v.getOrElse(0L) + numFish)
        timer match {
          case 0 =>
            newFish.updatedWith(6)(update).updatedWith(8)(update)
          case n =>
            newFish.updatedWith(n - 1)(update)
        }
      }
    }
    .map(_.valuesIterator.sum) // counts of fish
    // 0th value after 0 days, 1st value after 1 day, etc. Drop first 80 values to get state after 80 days.
    .drop(n)
    .next()
}

object Part1 {
  def main(args: Array[String]): Unit = {
    println(fishAfterNDays(80))
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    println(fishAfterNDays(256))
  }
}
