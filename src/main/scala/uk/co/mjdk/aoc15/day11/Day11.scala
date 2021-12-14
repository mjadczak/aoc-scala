package uk.co.mjdk.aoc15.day11

import scala.annotation.tailrec

val Input = "vzbxkghb"

@tailrec
def increment(password: Vector[Char], pos: Int = 0): Vector[Char] = {
  val index = password.length - 1 - pos
  password(index) match {
    case 'z' =>
      increment(password.updated(index, 'a'), pos + 1)
    case c =>
      password.updated(index, (c + 1).toChar)
  }
}

// Very similar to Day 5
val BannedLetters = Set('i', 'o', 'l')
@tailrec
def validate(
    password: Vector[Char],
    maybeLastLetter: Option[Char] = None,
    seenPairs: Set[Char] = Set.empty,
    hasIncreasingWindow: Boolean = false
): Boolean = {
  password match {
    case Vector() =>
      hasIncreasingWindow && seenPairs.size >= 2
    case letter +: rest if BannedLetters.contains(letter) => false
    case letter +: rest =>
      val maybeNextLetter = rest.headOption
      // check for repeated pair - I am interpreting "two different pairs" as them not being allowed to be the same
      // letter in each case?
      val foundRepeatedPair =
        !seenPairs.contains(letter) && maybeLastLetter.contains(letter)
      // check for windowed letter
      val foundIncreasingWindow = (maybeLastLetter, maybeNextLetter) match {
        case (Some(lastLetter), Some(nextLetter)) =>
          (letter - lastLetter) == 1 && (nextLetter - letter) == 1
        case _ => false
      }
      // save the just-seen pair for later
      val newPair =
        if (foundRepeatedPair) {
          Some(letter)
        } else {
          None
        }

      validate(
        rest,
        Some(letter),
        seenPairs ++ newPair,
        hasIncreasingWindow || foundIncreasingWindow
      )
  }

}

object Part1 {
  def main(args: Array[String]): Unit = {
    val res = Iterator
      .iterate(Input.toCharArray.toVector)(increment(_))
      .find(validate(_))
      .get
    println(res.mkString)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val res = Iterator
      .iterate(Input.toCharArray.toVector)(increment(_))
      .filter(validate(_))
      .drop(1)
      .next()
    println(res.mkString)
  }
}
