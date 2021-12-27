package uk.co.mjdk.aoc21.day22

import uk.co.mjdk.aoc.inputLines

case class Cuboid(
    x: Range,
    y: Range,
    z: Range
) {
  def isInInitializationRegion: Boolean = {
    def isRangeOk(range: Range): Boolean =
      range.start >= -50 && range.last <= 50
    isRangeOk(x) && isRangeOk(y) && isRangeOk(z)
  }

  def volume: Long = x.size.toLong * y.size * z.size

  def isEmpty: Boolean = x.isEmpty || y.isEmpty || z.isEmpty
  def nonEmpty: Boolean = !isEmpty
  def intersection(other: Cuboid): Cuboid = Cuboid(
    x.intersection(other.x),
    y.intersection(other.y),
    z.intersection(other.z)
  )
}

extension (r: Range) {
  def intersection(other: Range): Range =
    r.start.max(other.start).to(r.end.min(other.end))
}

case class Instruction(
    on: Boolean,
    cuboid: Cuboid
)

def parseInput: Vector[Instruction] = {
  val Pat =
    """(on|off) x=([-\d]+)\.\.([-\d]+),y=([-\d]+)\.\.([-\d]+),z=([-\d]+)\.\.([-\d]+)""".r
  inputLines(21)(22).map {
    case Pat(onoff, xMin, xMax, yMin, yMax, zMin, zMax) =>
      val isOn = onoff match {
        case "on"  => true
        case "off" => false
      }
      Instruction(
        isOn,
        Cuboid(
          xMin.toInt.to(xMax.toInt),
          yMin.toInt.to(yMax.toInt),
          zMin.toInt.to(zMax.toInt)
        )
      )
  }.toVector
}

object Part1 {
  def main(args: Array[String]): Unit = {
    // quick and dirty solution for now is fine - we'll need to be smarter for part 2
    val ary = Array.fill(101, 101, 101)(false)
    val instructions = parseInput.filter(_.cuboid.isInInitializationRegion)
    instructions.foreach { inst =>
      for {
        x <- inst.cuboid.x
        y <- inst.cuboid.y
        z <- inst.cuboid.z
      } ary(x + 50)(y + 50)(z + 50) = inst.on
    }

    val cubesOn = ary.iterator.flatMap(_.flatMap(_.iterator)).count(identity)
    println(cubesOn)
  }
}

// inspired by https://www.reddit.com/r/adventofcode/comments/rlxhmg/comment/hpud3sq/
// The idea is to sum up the contribution of just the "on" cuboids, by taking their volume after intersections with all
// subsequent cuboids have been subtracted - the "off" ones because they will turn those areas off, and the "on" ones
// because we will count their contribution later
// We need to build up the expression in symbolic terms first as we need to rewrite the expression in terms of
// intersections we can actually find

enum Expr {
  case Unit(cuboid: Cuboid)
  case Diff(left: Expr, right: Cuboid)
  case Intersect(left: Expr, right: Cuboid)

  def ^-(right: Cuboid): Expr = Expr.Diff(this, right)
  def ^&(right: Cuboid): Expr = Expr.Intersect(this, right)
}

object Expr {
  def apply(c: Cuboid): Expr = Expr.Unit(c)
}

extension (e: Expr) {
  def evaluate: Long = e match {
    case Expr.Unit(c)    => c.volume
    case Expr.Diff(l, r) => l.evaluate - (l ^& r).evaluate
    case Expr.Intersect(l, r) =>
      l match {
        case Expr.Unit(c)           => c.intersection(r).volume
        case Expr.Intersect(ll, lr) => (ll ^& lr.intersection(r)).evaluate
        // |A-B & C| = |A&B| - |A&B&C|
        case Expr.Diff(ll, lr) => (ll ^& r).evaluate - (ll ^& lr ^& r).evaluate
      }
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val instructions = parseInput
    val result = instructions.tails
      .takeWhile(_.nonEmpty)
      .filter(_.head.on)
      .map(_.map(_.cuboid))
      .map { v =>
        v.tail
          .filter(t => t.intersection(v.head).nonEmpty)
          .foldLeft(Expr(v.head))(_ ^- _)
      }
      .map(_.evaluate)
      .sum

    println(result)
  }
}
