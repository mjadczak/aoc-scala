package uk.co.mjdk.aoc21.day19

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.math.Ordering.Implicits.*

case class Pos(x: Int, y: Int, z: Int) {
  def rotX: Pos = Pos(x, -z, y)
  def rotY: Pos = Pos(z, y, -x)
  def rotZ: Pos = Pos(-y, x, z)

  def +(other: Pos): Pos = Pos(x + other.x, y + other.y, z + other.z)
  def -(other: Pos): Pos = Pos(x - other.x, y - other.y, z - other.z)
  def magnitude2: Int = x * x + y * y + z * z
  def magnitudeManhattan: Int = x.abs + y.abs + z.abs
}

object Pos {
  val AllRotations: Vector[Pos => Pos] =
    for {
      a <- Vector[Pos => Pos](
        identity,
        _.rotY,
        _.rotY.rotY,
        _.rotY.rotY.rotY,
        _.rotZ,
        _.rotZ.rotZ.rotZ
      )
      b <- Vector[Pos => Pos](identity, _.rotX, _.rotX.rotX, _.rotX.rotX.rotX)
    } yield a.compose(b)

  assert(AllRotations.length == 24)

  val Origin: Pos = Pos(0, 0, 0)
}

given Ordering[Pos] = Ordering[(Int, Int, Int)].on(p => (p.x, p.y, p.z))

case class Scanner(
    id: Int,
    beacons: Vector[Pos],
    origin: Pos // in local coordinates
) {
  lazy val distancesSq: Vector[Int] = for {
    a <- beacons
    b <- beacons
    if a < b
  } yield (b - a).magnitude2

  def canMatch(other: Scanner): Boolean = distancesSq
    .intersect(other.distancesSq)
    .length >= Scanner.MinDistancesMatching

  // Perhaps we could be smart and figure out _which_ pairs of beacons match in distances etc
  // Instead, after pre-verifying with above step, we just start trying to identify all pairs of beacons and stop
  // once we've found one that gives us at least 12 matching positions (modulo all possible rotations)
  def findTransformed(other: Scanner): Option[Scanner] =
    if (canMatch(other)) {
      val allMatching = for {
        rot <- Pos.AllRotations.iterator
        b1 <- beacons.iterator
        b2 <- other.beacons.iterator
        trans = b1 - rot(b2)
        xform = (p: Pos) => rot(p) + trans
        transformedBeacons = other.beacons.map(xform)
        if transformedBeacons.intersect(beacons).length >= 12
      } yield Scanner(other.id, transformedBeacons, xform(other.origin))

      allMatching.nextOption()
    } else {
      None
    }
}

object Scanner {
  // if we have at least this many distances matching, it's possible that 12 beacons match (but we must verify)
  val MinDistancesMatching: Int = 12 * 11 / 2
}

def parseScanners: Vector[Scanner] = {
  Iterator
    .unfold(inputLines(21)(19)) { iter =>
      if (!iter.hasNext) {
        None
      } else {
        val chunk = iter.dropWhile(_.isEmpty).takeWhile(_.nonEmpty).toVector
        Some((chunk, iter.dropWhile(_.isEmpty)))
      }
    }
    .zipWithIndex
    .map { case (chunk, id) =>
      require(chunk.head.contains(s"scanner $id"))
      val coords = chunk.tail.map { line =>
        val Array(x, y, z) = line.split(',')
        Pos(x.toInt, y.toInt, z.toInt)
      }
      Scanner(id, coords, Pos.Origin)
    }
    .toVector
}

def transformAll(all: Vector[Scanner]): Vector[Scanner] = {
  require(all.head.id == 0)
  transformAll(Vector(), Vector(all.head), all.tail)
}

@tailrec
def transformAll(
    // Done and transformed into 0-space, with all others matched against it
    matched: Vector[Scanner],
    // Transformed into 0-space, not yet checked for other matches
    working: Vector[Scanner],
    unmatched: Vector[Scanner] // Pending
): Vector[Scanner] = working.headOption match {
  case Some(next) =>
    val (remaining, found) = unmatched.partitionMap { candidate =>
      next.findTransformed(candidate).toRight(candidate)
    }

    transformAll(matched :+ next, working.tail ++ found, remaining)
  case None =>
    if (unmatched.nonEmpty) {
      throw new IllegalStateException(
        s"Ran out of working scanners, but still have ${unmatched.size} unmatched scanners"
      )
    } else {
      matched
    }
}

object Part1 {

  def main(args: Array[String]): Unit = {
    // take each scanner in turn, try to match it against scanner 0, then find all other matching scanners
    // we could try and always match against the "combined" scanners matched so far, but then it's unclear whether the
    // 12-beacon requirement still holds

    val scanners = parseScanners
    val matched = transformAll(scanners)

    val numBeacons = matched.iterator.flatMap(_.beacons).toSet.size
    println(numBeacons)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val scanners = parseScanners
    val scannerPositions = transformAll(scanners).map(_.origin)

    val allDistances = for {
      s1 <- scannerPositions
      s2 <- scannerPositions
    } yield (s1 - s2).magnitudeManhattan

    println(allDistances.max)
  }
}
