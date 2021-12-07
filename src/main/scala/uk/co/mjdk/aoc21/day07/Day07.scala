package uk.co.mjdk.aoc21.day07

import uk.co.mjdk.aoc21.inputLines

object Part1 {
  def main(args: Array[String]): Unit = {
    // The solution is simply the median of all the numbers - once we have an equal amount of lanternfish either side,
    // moving past one of them and to the other side decreases N-1 distances but increases N distances, so the cost
    // must be greater

    // There exist better ways of finding the median, but we can be lazy here

    val nums =
      inputLines(7).next().split(',').iterator.map(_.toInt).toVector.sorted
    // we don't need the _actual_ median, just any point between the two middle ones. In particular, for an even number
    // we can take the left middle value
    val middle = nums((nums.length - 1) / 2)
    val cost = nums.iterator.map(n => (n - middle).abs).sum
    println(cost)
  }
}
