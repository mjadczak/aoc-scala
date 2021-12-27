package uk.co.mjdk.aoc21.day20

import uk.co.mjdk.aoc.inputLines

case class Pos(row: Int, col: Int) {
  def neighbourhood: Vector[Pos] = for {
    r <- -1.to(1).toVector
    c <- -1.to(1)
  } yield Pos(row + r, col + c)
}

extension (ab: Vector[Boolean]) {
  def toInt: Int = {
    val len = ab.length

    require(len <= 31)
    // big endian
    ab.iterator.zipWithIndex
      .filter(_._1)
      .map((_, idx) => 1 << (len - (idx + 1)))
      .foldLeft(0)(_ | _)
  }
}

case class Image(
    minRow: Int,
    maxRow: Int,
    minCol: Int,
    maxCol: Int,
    pixels: Set[Pos],
    canvas: Boolean
) {
  def getValue(pixel: Pos): Boolean = if (
    pixel.row >= minRow &&
    pixel.row <= maxRow &&
    pixel.col >= minCol &&
    pixel.col <= maxCol
  ) {
    pixels(pixel)
  } else {
    canvas
  }

  def transform(algo: Vector[Boolean]): Image = {
    // we always need to start 1px away from the current corners
    val newPixelsIter = for {
      row <- (minRow - 1).to(maxRow + 1).iterator
      col <- (minCol - 1).to(maxCol + 1).iterator
      pos = Pos(row, col)
      idx = Pos(row, col).neighbourhood.map(getValue).toInt
      if algo(idx)
    } yield pos

    val canvasIdx = if (canvas) Integer.parseInt("111111111", 2) else 0
    Image(
      minRow - 1,
      maxRow + 1,
      minCol - 1,
      maxCol + 1,
      newPixelsIter.toSet,
      algo(canvasIdx)
    )
  }
}

def parseInput: (Vector[Boolean], Image) = {
  val iter = inputLines(21)(20)
  val algo = iter
    .next()
    .iterator
    .map {
      case '#' => true
      case '.' => false
    }
    .toVector

  val imgLines = iter.dropWhile(_.isEmpty).toVector

  val setPositions = imgLines.iterator.zipWithIndex.flatMap {
    case (line, row) =>
      line.zipWithIndex.flatMap { case (char, col) =>
        char match {
          case '#' => Some(Pos(row, col))
          case '.' => None
        }
      }
  }.toSet

  (
    algo,
    Image(
      0,
      imgLines.length - 1,
      0,
      imgLines(0).length - 1,
      setPositions,
      false
    )
  )
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val (algo, image) = parseInput
    println(image.transform(algo).transform(algo).pixels.size)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val (algo, image) = parseInput
    val transformed = Iterator.iterate(image)(_.transform(algo)).drop(50).next()
    println(transformed.pixels.size)
  }
}
