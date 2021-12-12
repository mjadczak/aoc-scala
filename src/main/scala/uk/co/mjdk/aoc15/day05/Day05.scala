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
