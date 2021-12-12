package uk.co.mjdk.aoc21.day04

import uk.co.mjdk.aoc.inputLines

// We store a lookup from number -> row and column, and count of numbers called in each row/col
// 0-indexed rows and cols
// Assume numbers cannot repeat in one board
case class Pos(row: Int, col: Int)
case class BingoBoard private (
    unmarkedNumbersLookup: Map[Int, Pos],
    rowCounts: Vector[Int] = Iterator.fill(5)(0).toVector,
    colCounts: Vector[Int] = Iterator.fill(5)(0).toVector
) {
  val Size = 5
  require(rowCounts.length == Size)
  require(colCounts.length == Size)

  def unmarkedNumbers: Iterator[Int] = unmarkedNumbersLookup.keysIterator
  // Returns true in first position if after applying the number, the board has won
  //   because we're lazy, it's not idempotent (but it could be) -> it will not return true the second time you ask
  def markNumber(number: Int): (Boolean, BingoBoard) =
    unmarkedNumbersLookup.get(number).fold((false, this)) {
      case Pos(row, col) =>
        val newLookup = unmarkedNumbersLookup - number
        val (wonRows, newRowCounts) = {
          val newCnt = rowCounts(row) + 1
          (newCnt == Size, rowCounts.updated(row, newCnt))
        }
        val (wonCols, newColCounts) = {
          val newCnt = colCounts(col) + 1
          (newCnt == Size, colCounts.updated(col, newCnt))
        }
        (wonCols || wonRows, BingoBoard(newLookup, newRowCounts, newColCounts))
    }
}

object BingoBoard {
  def fromNumbers(numbers: Seq[Seq[Int]]): BingoBoard = {
    require(numbers.length == 5)
    require(numbers.forall(_.length == 5))

    val lookup = numbers.zipWithIndex.foldLeft(Map.empty[Int, Pos]) {
      case (map, (line, row)) =>
        line.zipWithIndex.foldLeft(map) { case (map, (num, col)) =>
          map + (num -> Pos(row, col))
        }
    }

    BingoBoard(lookup)
  }
}

def parseNumsAndBoards: (Vector[Int], List[BingoBoard]) = {
  val iter = inputLines(21)(4)
  val drawnNumbers = iter.next().split(',').iterator.map(_.toInt).toVector
  val boards = iter
    .grouped(6)
    .map { lines =>
      lines.drop(1).map(_.trim.split("\\s+").toSeq.map(_.trim.toInt))
    }
    .map(BingoBoard.fromNumbers)
    .toList

  (drawnNumbers, boards)
}

def iterateNumbers(
    drawnNumbers: Vector[Int],
    initialBoards: List[BingoBoard]
): Iterator[(List[Int], List[BingoBoard])] = {
  drawnNumbers.iterator
    .scanLeft((List.empty[Int], initialBoards)) { case ((_, boards), nextNum) =>
      boards.partitionMap { board =>
        board.markNumber(nextNum) match {
          case (false, newBoard) => Right(newBoard)
          case (true, newBoard)  =>
            // we won!
            val score = newBoard.unmarkedNumbers.sum * nextNum
            Left(score)
        }
      }
    }
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val (drawnNumbers, boards) = parseNumsAndBoards

    // state: boards, optional final score from winning board
    val winningScore = iterateNumbers(drawnNumbers, boards)
      .find(_._1.nonEmpty) // find first occurrence of some scores
      .map(_._1.head)
      .get

    println(winningScore)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val (drawnNumbers, boards) = parseNumsAndBoards

    val lastScoreToWin = iterateNumbers(drawnNumbers, boards)
      .map(_._1)
      .filter(_.nonEmpty)
      // No convenient .last on an iterator, even when we know it's finite
      // Could have written one, but this is fine
      .toVector
      .last
      .head

    println(lastScoreToWin)
  }
}
