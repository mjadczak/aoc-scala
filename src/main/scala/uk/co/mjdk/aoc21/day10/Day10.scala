package uk.co.mjdk.aoc21.day10

import uk.co.mjdk.aoc.*

import scala.annotation.tailrec

enum BracketType(val charSet: Set[Char]) {
  case Rounded extends BracketType(Set('(', ')'))
  case Square extends BracketType(Set('[', ']'))
  case Brace extends BracketType(Set('{', '}'))
  case Angle extends BracketType(Set('<', '>'))
}

enum BracketDirection(val charSet: Set[Char]) {
  case Opening extends BracketDirection(Set('(', '[', '{', '<'))
  case Closing extends BracketDirection(Set(')', ']', '}', '>'))
}

case class Bracket(typ: BracketType, direction: BracketDirection)

object Bracket {
  def parse(c: Char): Bracket = {
    val typ = BracketType.values.filter(_.charSet.contains(c)).only
    val dir = BracketDirection.values.filter(_.charSet.contains(c)).only
    Bracket(typ, dir)
  }

}

enum ParseResult {
  case Ok
  case Illegal(illegalType: BracketType)
  case Incomplete(stack: List[BracketType])
}

@tailrec
def parseLine(
    input: List[Char],
    stack: List[BracketType] = List.empty
): ParseResult = input match {
  case List() if stack.isEmpty => ParseResult.Ok
  case List()                  => ParseResult.Incomplete(stack)
  case c :: rest =>
    val b = Bracket.parse(c)
    b.direction match {
      case BracketDirection.Opening => parseLine(rest, b.typ :: stack)
      case BracketDirection.Closing =>
        stack match {
          case b.typ :: stackRest =>
            parseLine(rest, stackRest)
          case _ =>
            ParseResult.Illegal(b.typ)
        }
    }
}

def illegalScore(typ: BracketType): Int = typ match {
  case BracketType.Rounded => 3
  case BracketType.Square  => 57
  case BracketType.Brace   => 1197
  case BracketType.Angle   => 25137
}

def completionScore(typ: BracketType): Long = typ match {
  case BracketType.Rounded => 1
  case BracketType.Square  => 2
  case BracketType.Brace   => 3
  case BracketType.Angle   => 4
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val score = inputLines(21)(10)
      .map(_.toList)
      .map(parseLine(_))
      .collect { case ParseResult.Illegal(typ) =>
        illegalScore(typ)
      }
      .sum

    println(score)
  }
}

object Part2 {
  // Note we don't need to actually come up with the completion - just the score for it
  def main(args: Array[String]): Unit = {
    val scores = inputLines(21)(10)
      .map(_.toList)
      .map(parseLine(_))
      .collect { case ParseResult.Incomplete(stack) =>
        stack.foldLeft(0L) { case (score, typ) =>
          score * 5 + completionScore(typ)
        }
      }
      .toVector
      .sorted

    println(scores(scores.length / 2))
  }
}
