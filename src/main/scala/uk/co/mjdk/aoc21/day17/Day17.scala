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

// X upper bound: xMax +1 lower bound: 0 (especially for my special case; the problem specifies drag to also work with
// negative x velocities, which would only make sense if x could also be negative)
// Y upper bound - as before, -ymin. Lower bound - ymin - 1

case class Vector(x: Int, y: Int) {
  def +(other: Vector): Vector = copy(x = x + other.x, y = y + other.y)
}

case class State(position: Vector, velocity: Vector) {
  require(velocity.x >= 0)
  def isInTarget(area: TargetArea): Boolean =
    position.x >= area.xMin && position.x <= area.xMax && position.y >= area.yMin && position.y <= area.yMax

  def isLost(area: TargetArea): Boolean =
    // past target, or 0 velocity and not on target
    !isInTarget(area) && (
      velocity.x == 0 && position.x < area.xMin ||
        position.x > area.xMax ||
        position.y < area.yMin
    )

  def advance: State = {
    val newVelocity = Vector(0.max(velocity.x - 1), velocity.y - 1)
    State(position + velocity, newVelocity)
  }
}

object Part2 {
  def doesHitTarget(initialVelocity: Vector, targetArea: TargetArea): Boolean =
    Iterator
      .iterate(State(Vector(0, 0), initialVelocity))(_.advance)
      .takeWhile(!_.isLost(targetArea))
      .exists(_.isInTarget(targetArea))

  def main(args: Array[String]): Unit = {
    val target = parseInput
    val goodVels = 0
      .to(target.xMax)
      .iterator
      .flatMap(xVel =>
        (target.yMin - 1)
          .to(-target.yMin)
          .iterator
          .map(yVel => Vector(xVel, yVel))
      )
      .filter(initialVel => doesHitTarget(initialVel, target))
      .toSet

    println(goodVels.size)
  }
}
