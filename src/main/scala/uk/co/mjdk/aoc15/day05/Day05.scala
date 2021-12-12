package uk.co.mjdk.aoc15.day05

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.util.chaining._

// Let's have fun and do it in a single pass
val Vowels: Set[Char] = Set('a', 'e', 'i', 'o', 'u')
val NaughtyCombinations: Set[Vector[Char]] = Set(
  Vector('a', 'b'),
  Vector('c', 'd'),
  Vector('p', 'q'),
  Vector('x', 'y')
)

// We could peek at the next letter, but remembering the last is neater as we can do all 3 checks on the "current" letter
@tailrec
def isNice(
    letters: List[Char],
    maybeLastLetter: Option[Char] = None,
    numVowels: Int = 0,
    hasDoubleLetter: Boolean = false
): Boolean =
  letters match {
    case Nil => numVowels >= 3 && hasDoubleLetter
    case letter :: rest =>
      val newNumVowels =
        if (Vowels.contains(letter)) numVowels + 1 else numVowels
      maybeLastLetter match {
        case None => isNice(rest, Some(letter), newNumVowels, hasDoubleLetter)
        case Some(lastLetter) =>
          if (NaughtyCombinations.contains(Vector(lastLetter, letter))) {
            false
          } else {
            val foundDoubleLetter = lastLetter == letter
            isNice(
              rest,
              Some(letter),
              newNumVowels,
              hasDoubleLetter || foundDoubleLetter
            )
          }
      }
  }

object Part1 {
  def main(args: Array[String]): Unit = {
    inputLines(15)(5)
      .map(_.toCharArray.toList)
      .count(isNice(_))
      .pipe(println)
  }
}

// We need a window of three letters - previous, current, and next. We handle the overlapping repeat case by being
// careful about when we admit the seen combination to the search set
// Note we can also exit early here!
@tailrec
def isNice2(
    letters: List[Char],
    maybeLastLetter: Option[Char] = None,
    seenPairs: Set[Vector[Char]] = Set.empty,
    hasRepeatedPair: Boolean = false,
    hasWindowedLetter: Boolean = false
): Boolean = {
  if (hasRepeatedPair && hasWindowedLetter) {
    true
  } else {
    letters match {
      case Nil => false // would have returned above if satisfied
      case letter :: rest =>
        val maybeNextLetter = rest.headOption
        // check for repeated pair
        val foundRepeatedPair = maybeNextLetter match {
          case None => false
          case Some(nextLetter) =>
            seenPairs.contains(Vector(letter, nextLetter))
        }
        // check for windowed letter
        val foundWindowedLetter = (maybeLastLetter, maybeNextLetter) match {
          case (Some(lastLetter), Some(nextLetter)) => lastLetter == nextLetter
          case _                                    => false
        }
        // save the just-seen pair for later
        val newPair =
          maybeLastLetter.map(lastLetter => Vector(lastLetter, letter))

        isNice2(
          rest,
          Some(letter),
          seenPairs ++ newPair,
          hasRepeatedPair || foundRepeatedPair,
          hasWindowedLetter || foundWindowedLetter
        )
    }
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    inputLines(15)(5)
      .map(_.toCharArray.toList)
      .count(isNice2(_))
      .pipe(println)
  }
}
