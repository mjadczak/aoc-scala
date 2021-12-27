package uk.co.mjdk.aoc21.day21

import uk.co.mjdk.aoc.inputLines

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
    numAggregateDiceValuesRead: Int = 0,
    player1Turn: Boolean = true
) {
  def winner: Option[Player] = if (player1.score >= 1000) {
    Some(player1)
  } else if (player2.score >= 1000) {
    Some(player2)
  } else {
    None
  }

  def loser: Option[Player] = winner match {
    case Some(`player1`) => Some(player2)
    case Some(`player2`) => Some(player1)
    case None            => None
  }

  def next(aggDiceValue: Int): State = if (player1Turn) {
    copy(
      player1 = player1.movedBy(aggDiceValue),
      numAggregateDiceValuesRead = numAggregateDiceValuesRead + 1,
      player1Turn = false
    )
  } else {
    copy(
      player2 = player2.movedBy(aggDiceValue),
      numAggregateDiceValuesRead = numAggregateDiceValuesRead + 1,
      player1Turn = true
    )
  }

  def numDiceRolls: Int = numAggregateDiceValuesRead * 3
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

    val finalState = aggRollValues
      .scanLeft(initialState) { case (state, diceRoll) =>
        state.next(diceRoll)
      }
      .find(_.winner.nonEmpty)
      .get

    println(finalState.loser.get.score * finalState.numDiceRolls)
  }
}
