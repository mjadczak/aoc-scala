package uk.co.mjdk.aoc21.day13

import uk.co.mjdk.aoc.inputLines

case class Pos(x: Int, y: Int)

enum Axis {
  case X
  case Y
}

object Axis {
  def parse(input: String): Axis = input match {
    case "x" => X
    case "y" => Y
  }
}

case class Fold(axis: Axis, value: Int) {
  def apply(pos: Pos): Pos = {
    axis match {
      // Crying out for lenses
      case Axis.X =>
        pos.copy(x = value - (pos.x - value).abs)
      case Axis.Y =>
        pos.copy(y = value - (pos.y - value).abs)
    }
  }
}

def parseInput: (Set[Pos], Vector[Fold]) = {
  val iter = inputLines(21)(13)
  val positions = iter
    .takeWhile(s => s.nonEmpty && s(0).isDigit)
    .map { line =>
      val Array(x, y) = line.split(',')
      Pos(x.toInt, y.toInt)
    }
    .toSet
  val folds = iter
    .dropWhile(!_.startsWith("fold"))
    .map { line =>
      val Array(axis, value) = line.stripPrefix("fold along ").split('=')
      Fold(Axis.parse(axis), value.toInt)
    }
    .toVector

  (positions, folds)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val (positions, folds) = parseInput
    val folded = positions.map(folds.head.apply)
    print(folded.size)
  }
}

def prettyGrid(grid: Set[Pos], fold: Option[Fold] = None): String = {
  // not the most efficient way of going about things, but expedient
  val minX = grid.minBy(_.x).x
  val minY = grid.minBy(_.y).y
  val maxX = grid.maxBy(_.x).x
  val maxY = grid.maxBy(_.y).y

  val regularOutput: Pos => String = pos => if (grid(pos)) "#" else " "
  val output: Pos => String = fold match {
    case None => regularOutput
    case Some(Fold(Axis.X, foldX)) =>
      pos => if (pos.x == foldX) "|" else regularOutput(pos)
    case Some(Fold(Axis.Y, foldY)) =>
      pos => if (pos.y == foldY) "-" else regularOutput(pos)
  }

  s"${grid.size} ON, x=$minX..$maxX, y=$minY..$maxY\n" +
    (minY to maxY).iterator
      .map { y =>
        (minX to maxX).iterator.map { x =>
          output(Pos(x, y))
        }.mkString
      }
      .mkString("\n")
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val (positions, folds) = parseInput
    val folded = folds.foldLeft(positions) { (poss, fold) =>
//      println(prettyGrid(poss, Some(fold)))
//      println()
//      println()

      poss.map(fold.apply)
    }
    println(prettyGrid(folded))
  }
}
