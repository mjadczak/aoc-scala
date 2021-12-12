package uk.co.mjdk.aoc21.day11

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.collection.immutable.Queue

case class Pos(row: Int, col: Int) {
  def adjacentPositions: Set[Pos] = {
    val alternatives = for {
      r <- -1 to 1
      c <- -1 to 1
    } yield copy(row = row + r, col = col + c)
    alternatives.toSet.excl(this)
  }
}

def parseGrid: Map[Pos, Int] =
  inputLines(21)(11).zipWithIndex
    .foldLeft(Map.empty[Pos, Int]) { case (grid, (line, row)) =>
      line.zipWithIndex.foldLeft(grid) { case (grid, (char, col)) =>
        grid + (Pos(row, col) -> char.asDigit)
      }
    }

def prettyGrid(grid: Map[Pos, Int]): String = {
  val maxRow = grid.keysIterator.map(_.row).max
  val maxCol = grid.keysIterator.map(_.col).max
  (0 to maxRow)
    .map { row =>
      (0 to maxCol).map { col =>
        grid(Pos(row, col)) match {
          case i if i <= 9 => i.toString
          case _           => "!"
        }
      }.mkString
    }
    .mkString("\n")
}

def increaseEnergy(
    grid: Map[Pos, Int],
    pos: Pos
): (Map[Pos, Int], Option[Pos]) = {
  val newEnergy = grid(pos) + 1
  val newGrid = grid + (pos -> newEnergy)
  if (newEnergy > 9) {
    (newGrid, Some(pos))
  } else {
    (newGrid, None)
  }
}

@tailrec
def processFlashes(
    grid: Map[Pos, Int],
    flashedQueue: Queue[Pos],
    flashedSet: Set[Pos] = Set.empty
): (Map[Pos, Int], Set[Pos]) = flashedQueue.dequeueOption match {
  case None => (grid, flashedSet)
  case Some((flashingPos, queueRest)) if flashedSet.contains(flashingPos) =>
    processFlashes(grid, queueRest, flashedSet)
  case Some((flashingPos, queueRest)) =>
    val (newGrid, newQueue) = flashingPos.adjacentPositions
      .filter(grid.contains)
      .foldLeft((grid, queueRest)) { case ((grid, queue), adjacentPos) =>
        val (newGrid, flashed) = increaseEnergy(grid, adjacentPos)
        (newGrid, queue.appendedAll(flashed))
      }

    processFlashes(newGrid, newQueue, flashedSet + flashingPos)
}

def simulateStep(grid: Map[Pos, Int]): (Int, Map[Pos, Int]) = {
  // Increase all levels by 1, and keep track of any position which goes above 9
  val (newGrid, queue) =
    grid.keysIterator.foldLeft((grid, Queue.empty[Pos])) {
      case ((grid, queue), pos) =>
        val (newGrid, flashed) = increaseEnergy(grid, pos)
        (newGrid, queue.appendedAll(flashed))
    }

  // Work through the queue of flashed positions, increasing their neighbours and adding to queue
  // Also keep track of the full set of flashed positions for this step
  val (processedGrid, flashedSet) = processFlashes(newGrid, queue)

  // Set all flashed positions to 0
  val zeroedGrid = flashedSet.iterator.foldLeft(processedGrid) { (grid, pos) =>
    grid + (pos -> 0)
  }

  (flashedSet.size, zeroedGrid)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val totalFlashes =
      Iterator.unfold(parseGrid)(grid => Some(simulateStep(grid))).take(100).sum

    println(totalFlashes)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val grid = parseGrid
    val (_, stepNum) = Iterator
      .unfold(grid)(grid => Some(simulateStep(grid)))
      .zipWithIndex
      .find(_._1 == grid.size)
      .get

    // our steps are 0-based
    println(stepNum + 1)
  }
}
