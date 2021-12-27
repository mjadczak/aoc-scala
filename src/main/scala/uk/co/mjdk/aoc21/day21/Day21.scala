package uk.co.mjdk.aoc21.day21

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.collection.immutable.Queue

def aggRollValues: Iterator[Int] =
  Iterator
    .continually(1.to(100).iterator)
    .flatten
    .grouped(3)
    .map(_.sum)

case class Space(position: Int) {
  require(position >= 1 && position <= 10)
  def forward(num: Int): Space = {
    // convert to 0-based, modular addition, convert to 1-based
    Space(((position - 1 + num) % 10) + 1)
  }
}

case class Player(
    id: Int,
    space: Space,
    score: Int = 0
) {
  def movedBy(num: Int): Player = {
    val newSpace = space.forward(num)
    Player(id, newSpace, score + newSpace.position)
  }
}

case class State(
    player1: Player,
    player2: Player,
    player1Turn: Boolean = true
) {
  def winner(winningScore: Int): Option[Player] = if (
    player1.score >= winningScore
  ) {
    Some(player1)
  } else if (player2.score >= winningScore) {
    Some(player2)
  } else {
    None
  }

  def loser(winningScore: Int): Option[Player] = winner(winningScore) match {
    case Some(`player1`) => Some(player2)
    case Some(`player2`) => Some(player1)
    case None            => None
  }

  def next(aggDiceValue: Int): State = if (player1Turn) {
    copy(
      player1 = player1.movedBy(aggDiceValue),
      player1Turn = false
    )
  } else {
    copy(
      player2 = player2.movedBy(aggDiceValue),
      player1Turn = true
    )
  }
}

def parseInput: (Int, Int) = {
  val Pat = """Player (\d) starting position: (\d+)""".r
  val iter = inputLines(21)(21)
  val Pat("1", pos1) = iter.next()
  val Pat("2", pos2) = iter.next()
  (pos1.toInt, pos2.toInt)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val (pos1, pos2) = parseInput
    val initialState = (
      State(
        Player(
          1,
          Space(pos1)
        ),
        Player(
          2,
          Space(pos2)
        )
      ),
      0
    )

    val (finalState, numDice) = aggRollValues
      .scanLeft(initialState) { case ((state, numDice), diceRoll) =>
        (state.next(diceRoll), numDice + 1)
      }
      .find(_._1.winner(1000).nonEmpty)
      .get

    println(finalState.loser(1000).get.score * numDice * 3)
  }
}

def playDirac(initialState: State): (Long, Long) =
  playDirac(Map(initialState -> 1L), 0L, 0L)

def allNextStates(state: State): Iterator[State] = for {
  r1 <- 1.to(3).iterator
  r2 <- 1.to(3).iterator
  r3 <- 1.to(3).iterator
} yield state.next(r1 + r2 + r3)

// total roll -> number of states
val allNextRolls: Map[Int, Int] = {
  val allRolls = for {
    r1 <- 1.to(3)
    r2 <- 1.to(3)
    r3 <- 1.to(3)
  } yield r1 + r2 + r3

  allRolls.groupMapReduce(identity)(_ => 1)(_ + _)
}

@tailrec
def playDirac(
    inProgress: Map[State, Long],
    wonPlayer1: Long,
    wonPlayer2: Long
): (Long, Long) =
  if (inProgress.isEmpty) {
    (wonPlayer1, wonPlayer2)
  } else {
    val (finished, unfinished) = inProgress.partition(_._1.winner(21).nonEmpty)
    val (finished1, finished2) = finished.foldLeft((0L, 0L)) {
      case ((w1, w2), (state, num)) =>
        state.winner(21) match {
          case Some(state.player1) => (w1 + num, w2)
          case Some(state.player2) => (w1, w2 + num)
        }
    }

    val newInProgress = unfinished.iterator
      .flatMap { case (state, num) =>
        allNextRolls.map { case (nextRoll, numNext) =>
          state.next(nextRoll) -> num * numNext
        }
      }
      .toVector
      .groupMapReduce(_._1)(_._2)(_ + _)

    playDirac(newInProgress, wonPlayer1 + finished1, wonPlayer2 + finished2)
  }

object Part2 {
  def main(args: Array[String]): Unit = {
    val (pos1, pos2) = parseInput
    val initialState = State(
      Player(
        1,
        Space(pos1)
      ),
      Player(
        2,
        Space(pos2)
      )
    )

    val (won1, won2) = playDirac(initialState)

    println((won1, won2))
    println(won1.max(won2))
  }
}
