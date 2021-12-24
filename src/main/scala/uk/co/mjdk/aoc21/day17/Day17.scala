package uk.co.mjdk.aoc21.day17

import uk.co.mjdk.aoc.inputLines

// Note that in the y direction, at some point we will be at y=0 again with the same initial speed yvel, but in the
// opposite direction. One step later, we will end up at -yvel-1. We have reached the maximum y velocity we can consider
// once yvel=-ymin-1, as anything larger than that will guarantee that we overshoot on the descent

// For part 1 we don't need anything else, as we know we can pick some x velocity that will get us on target, but the
// behaviour in the x axis is completely orthogonal to the y axis.

case class TargetArea(xMin: Int, xMax: Int, yMin: Int, yMax: Int)

def parseInput: TargetArea = {
  val InputPat = """target area: x=([-\d]+)..([-\d]+), y=([-\d]+)..([-\d]+)""".r
  inputLines(21)(17).next() match {
    case InputPat(xMin, xMax, yMin, yMax) =>
      TargetArea(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)
  }
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val target = parseInput
    // The apex height is just the sum of 0..maxYSpeed
    val ySpeed = -target.yMin - 1
    println(ySpeed * (ySpeed + 1) / 2)
  }
}
