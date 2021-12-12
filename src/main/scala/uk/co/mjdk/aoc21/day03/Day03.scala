package uk.co.mjdk.aoc21.day03

import uk.co.mjdk.aoc.inputLines

import scala.util.chaining._

// This question is slightly annoying, in that usually for binary number questions a big red flag is to treat them
// as strings rather than operating on the bits directly. However, our input is coming in as strings already!

object Part1 {
  def main(args: Array[String]): Unit = {
    // state: a count of 1 bits in each position starting from LSB, and a count of lines
    val (hist, count) = inputLines(21)(3).foldLeft((Vector.empty[Int], 0)) {
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

object Part2 {
  enum SelectionPreference {
    case MoreCommonPrefer1 // Pick the more common bit, resolve ties in favour of 1s
    case LessCommonPrefer0 // Pick the less common bit, resolve ties in favour of 0s
  }

  // bitIndex from MSB=0
  def filterNumbers(
      preference: SelectionPreference
  )(bitIndex: Int, numbers: Vector[String]): Vector[String] = {
    val (ones, zeros) = numbers.partition(_.charAt(bitIndex) == '1')
    preference match {
      case SelectionPreference.MoreCommonPrefer1 =>
        if (ones.length >= zeros.length) {
          ones
        } else {
          zeros
        }
      case SelectionPreference.LessCommonPrefer0 =>
        if (zeros.length <= ones.length) {
          zeros
        } else {
          ones
        }
    }
  }

  def findPreferredNumber(
      preference: SelectionPreference,
      numbers: Vector[String]
  ): Int = {
    val filter = filterNumbers(preference)
    Iterator
      .from(0)
      .scanLeft(numbers)((nums, idx) => filter(idx, nums))
      .find(_.length == 1)
      .get
      .head
      .pipe(Integer.parseInt(_, 2))
  }

  def main(args: Array[String]): Unit = {
    // chuck our pretty streaming out the window
    val numbers = inputLines(21)(3).toVector

    // Arguably, at this point it might be more efficient to parse all of the numbers and access the bits using bitwise
    // operations. I couldn't be bothered though.

    val o2GeneratorRating =
      findPreferredNumber(SelectionPreference.MoreCommonPrefer1, numbers)
    val co2ScrubberRating =
      findPreferredNumber(SelectionPreference.LessCommonPrefer0, numbers)

    println(
      s"O2 rating: $o2GeneratorRating, CO2 rating: $co2ScrubberRating, result = ${o2GeneratorRating * co2ScrubberRating}"
    )
  }
}
