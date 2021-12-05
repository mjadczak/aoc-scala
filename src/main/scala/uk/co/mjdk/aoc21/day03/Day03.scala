package uk.co.mjdk.aoc21.day03

import uk.co.mjdk.aoc21.inputLines

// This question is slightly annoying, in that usually for binary number questions a big red flag is to treat them
// as strings rather than operating on the bits directly. However, our input is coming in as strings already!

object Part1 {
  def main(args: Array[String]): Unit = {
    // state: a count of 1 bits in each position starting from LSB, and a count of lines
    val (hist, count) = inputLines(3).foldLeft((Vector.empty[Int], 0)) {
      case ((hist, count), numStr) =>
        val newHist = numStr.reverseIterator.zipWithIndex.foldLeft(hist) {
          case (hist, (c, i)) =>
            val addend = c match {
              case '0' => 0
              case '1' => 1
              case c =>
                throw new IllegalStateException(s"Unrecognised character $c")
            }
            if (i >= hist.length) {
              hist.appended(addend)
            } else {
              hist.updated(i, hist(i) + addend)
            }
        }

        (newHist, count + 1)
    }

    val numBits = hist.length
    require(numBits <= 32, s"Need $numBits bits")

    // Build our gamma
    val gamma = hist.zipWithIndex.foldLeft(0) { case (gamma, (ones, idx)) =>
      if (ones * 2 == count) {
        throw new IllegalStateException(
          s"Found equal numbers of 1 and 0 bits in bit $idx"
        )
      } else if (ones * 2 > count) {
        // set the bit at idx in gamma
        gamma | (1 << idx)
      } else {
        gamma
      }
    }

    // To get epsilon, we just flip gamma, but need to take care to only take the first N flipped bits
    val epsilon = (~gamma) & ((1 << numBits) - 1)

    println(s"Gamma = $gamma, epsilon = $epsilon, result = ${epsilon * gamma}")
  }
}
