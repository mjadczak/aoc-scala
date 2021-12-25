package uk.co.mjdk.aoc21.day18

import uk.co.mjdk.aoc.inputLines
import uk.co.mjdk.aoc21.day18.SnailfishNum.reduce

import scala.annotation.tailrec

// Let's try to do this the "obvious" way - may need to switch to a more efficient representation later

enum SnailfishNum {
  import SnailfishNum._

  case Lit(value: Int)
  case Pair(left: SnailfishNum, right: SnailfishNum)

  def magnitude: Int = this match {
    case Lit(v)     => v
    case Pair(l, r) => 3 * l.magnitude + 2 * r.magnitude
  }

  override def toString: String = this match {
    case Lit(v)     => Integer.toString(v)
    case Pair(l, r) => s"[$l,$r]"
  }

  def +(other: SnailfishNum): SnailfishNum = reduce(Pair(this, other))
}

object SnailfishNum {
  // crying out for parsing libraries ported to Scala 3
  def parse(input: String): SnailfishNum = {
    val (num, rest) = parse(input.toCharArray.toVector)
    require(rest.isEmpty, s"$input is not a valid snailfish number")
    num
  }

  // Don't bother with tail recursion
  private def parse(input: Vector[Char]): (SnailfishNum, Vector[Char]) =
    input match {
      case c +: rest if c.isDigit => (SnailfishNum.Lit(c.asDigit), rest)
      case '[' +: rest =>
        val (l, r1) = parse(rest)
        val ',' +: r2 = r1
        val (r, r3) = parse(r2)
        val ']' +: r4 = r3
        (SnailfishNum.Pair(l, r), r4)
    }

  // There's likely a better way to do this with zippers

  private enum ExplosionInstruction {
    case NoneFound
    case Explode(left: Option[Int], right: Option[Int])
  }

  private def addToRightmost(
      addend: Int,
      tree: SnailfishNum
  ): Option[SnailfishNum] = tree match {
    case Lit(v) => Some(Lit(v + addend))
    case Pair(l, r) =>
      addToRightmost(addend, r) match {
        case Some(newR) => Some(Pair(l, newR))
        case None =>
          addToRightmost(addend, l) match {
            case Some(newL) => Some(Pair(newL, r))
            case None       => None
          }
      }
  }

  private def addToLeftmost(
      addend: Int,
      tree: SnailfishNum
  ): Option[SnailfishNum] = tree match {
    case Lit(v) => Some(Lit(v + addend))
    case Pair(l, r) =>
      addToLeftmost(addend, l) match {
        case Some(newL) => Some(Pair(newL, r))
        case None =>
          addToLeftmost(addend, r) match {
            case Some(newR) => Some(Pair(l, newR))
            case None       => None
          }
      }
  }

  private def traverseForExplosion(
      tree: SnailfishNum,
      depth: Int = 0
  ): (SnailfishNum, ExplosionInstruction) =
    tree match {
      case Lit(_) => (tree, ExplosionInstruction.NoneFound)
      case Pair(l, r) =>
        if (depth == 4) {
          // found a deep pair - should always be two literals
          val (Lit(lv), Lit(rv)) = (l, r)
          (Lit(0), ExplosionInstruction.Explode(Some(lv), Some(rv)))
        } else {
          val (newL, instL) =
            traverseForExplosion(l, depth + 1)
          // we might have found an exploding pair
          instL match {
            case ExplosionInstruction.NoneFound =>
              // all good, try right
              val (newR, instR) = traverseForExplosion(
                r,
                depth + 1
              )
              instR match {
                case ExplosionInstruction.NoneFound =>
                  // nothing to see here
                  (tree, ExplosionInstruction.NoneFound)
                case inst @ ExplosionInstruction.Explode(Some(lv), re) =>
                  // exploded in right subtree - we try to modify our left subtree, and kick the ball up the tree
                  addToRightmost(lv, l) match {
                    case None =>
                      // can't do it!
                      (Pair(l, newR), inst)
                    case Some(newL) =>
                      (Pair(newL, newR), ExplosionInstruction.Explode(None, re))
                  }
                case inst =>
                  // exploded in right subtree somewhere, but the number to the left has already been found - nothing
                  // for us to do
                  (Pair(l, newR), inst)
              }
            case inst @ ExplosionInstruction.Explode(le, Some(rv)) =>
              // exploded in left subtree - we try to modify our right subtree, and kick the ball up the tree
              addToLeftmost(rv, r) match {
                case None =>
                  // nope
                  (Pair(newL, r), inst)
                case Some(newR) =>
                  (Pair(newL, newR), ExplosionInstruction.Explode(le, None))
              }
            case inst =>
              // exploded in left subtree somewhere, but the number to the right has already been found - nothing
              // for us to do
              (Pair(newL, r), inst)
          }
        }
    }

  private def explodeIfNeeded(num: SnailfishNum): Option[SnailfishNum] = {
    traverseForExplosion(num) match {
      case (_, ExplosionInstruction.NoneFound) => None
      case (newNum, _)                         => Some(newNum)
    }
  }

  private def splitIfNeeded(num: SnailfishNum): Option[SnailfishNum] =
    num match {
      case Lit(v) if v >= 10 =>
        Some(Pair(Lit(v / 2), Lit((v + 1) / 2)))
      case Lit(_) => None
      case Pair(l, r) =>
        splitIfNeeded(l) match {
          case Some(newL) => Some(Pair(newL, r))
          case None =>
            splitIfNeeded(r) match {
              case Some(newR) => Some(Pair(l, newR))
              case None       => None
            }
        }
    }

  @tailrec
  def reduce(num: SnailfishNum): SnailfishNum = {
    explodeIfNeeded(num) match {
      case Some(newNum) => reduce(newNum)
      case None =>
        splitIfNeeded(num) match {
          case Some(newNum) => reduce(newNum)
          case None         => num
        }
    }
  }
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val result = inputLines(21)(18)
      .map(SnailfishNum.parse)
      .reduceLeft(_ + _)

    println(result.magnitude)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val numbers = inputLines(21)(18).map(SnailfishNum.parse).toVector
    val maxMagnitude = numbers.iterator
      .flatMap(n1 => numbers.iterator.map(n2 => (n1, n2)))
      .filterNot((l, r) => l == r)
      .map(_ + _)
      .map(_.magnitude)
      .max
    println(maxMagnitude)
  }
}
