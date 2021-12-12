package uk.co.mjdk.aoc21.day07

import uk.co.mjdk.aoc.inputLines

def crabPositions: Vector[Int] =
  inputLines(21)(7).next().split(',').iterator.map(_.toInt).toVector

object Part1 {
  def main(args: Array[String]): Unit = {
    // The solution is simply the median of all the numbers - once we have an equal amount of lanternfish either side,
    // moving past one of them and to the other side decreases N-1 distances but increases N distances, so the cost
    // must be greater

    // There exist better ways of finding the median, but we can be lazy here

    val crabs = crabPositions.sorted

    // we don't need the _actual_ median, just any point between the two middle ones. In particular, for an even number
    // we can take the left middle value
    val middle = crabs((crabs.length - 1) / 2)
    val cost = crabs.iterator.map(n => (n - middle).abs).sum
    println(cost)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    // Note the sum of the first n numbers is n(n+1)/2, and write f(t) = the total cost of point t
    // Then set f'(t) = 0 and solve. We get the mean + a correction factor which we can calculate iteratively:
    // -(1/2)*mean(sign(x-t))

    val crabs = crabPositions
    val numCrabs = crabs.length
    val mean = crabs.sum.toDouble / numCrabs
    val numSmaller = crabs.count(_ < mean)
    val correction = .5d * (numSmaller.toDouble / numCrabs.toDouble)
    val middle = (mean - correction).round
    val cost = crabs.iterator.map { n =>
      val dist = (n - middle).abs
      dist * (dist + 1) / 2
    }.sum
    println(cost)
  }
}
