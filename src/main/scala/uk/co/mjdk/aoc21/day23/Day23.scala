package uk.co.mjdk.aoc21.day23

import uk.co.mjdk.aoc.inputLines

import java.util
import java.util.{Comparator, PriorityQueue}
import scala.annotation.tailrec
import scala.jdk.OptionConverters._
import scala.jdk.CollectionConverters._

enum Amphipod {
  case Amber
  case Bronze
  case Copper
  case Desert
}

extension (a: Amphipod) {
  def cost: Int = a match {
    case Amphipod.Amber  => 1
    case Amphipod.Bronze => 10
    case Amphipod.Copper => 100
    case Amphipod.Desert => 1000
  }

  def roomIdx: Int = a.ordinal

  def letter: Char = a match {
    case Amphipod.Amber  => 'A'
    case Amphipod.Bronze => 'B'
    case Amphipod.Copper => 'C'
    case Amphipod.Desert => 'D'
  }
}

case class State private (
    // head == space nearest to hallway
    sideRooms: Vector[Vector[Option[Amphipod]]],
    // for simplicity we represent all squares, even those never occupied
    hallway: Vector[Option[Amphipod]]
) {
  def withExtraPods: State = {
    if (hallway.flatten.nonEmpty) {
      throw new IllegalStateException("Hallway not empty")
    }
    if (!sideRooms.map(_.flatten).forall(_.length == 2)) {
      throw new IllegalArgumentException("Side rooms not length 2")
    }
    val rows = sideRooms.map(_.flatten).transpose
    val additionalRows = {
      import Amphipod._
      Vector(
        Vector(
          Desert,
          Copper,
          Bronze,
          Amber
        ),
        Vector(
          Desert,
          Bronze,
          Amber,
          Copper
        )
      )
    }
    val newRows = rows(0) +: additionalRows :+ rows(1)
    copy(sideRooms = newRows.transpose.map(_.map(Some(_))))
  }

  val sideRoomDepth: Int = sideRooms.head.length
  def isComplete: Boolean =
    sideRooms
      .zip(Amphipod.values)
      .forall { case (room, typ) => room.forall(_.contains(typ)) }

  override def toString: String = {
    val sideRoomsT = sideRooms.transpose
    "#" * 13 + "\n" + "#" + hallway
      .map(_.map(_.letter).getOrElse('.'))
      .mkString + "#\n" + "#" * 3 + sideRoomsT.head
      .map(_.map(_.letter).getOrElse("."))
      .mkString("#") + "#" * 3 + "\n" +
      sideRoomsT.tail
        .map(sr =>
          "  #" + sr.map(_.map(_.letter).getOrElse(".")).mkString("#") + "#  "
        )
        .mkString("\n") +
      "\n  #########  "
  }

  private def hallwaySquareForRoom(idx: Int): Int = (idx + 1) * 2

  def nextStates: Iterator[(State, Int)] = {
    // enumerate all valid next states, along with the cost to get to them

    // try moving candidates in the side rooms
    val sideRoomMoves = sideRooms.iterator.zipWithIndex.flatMap {
      case (room, roomIdx) =>
        // the first non-empty space can consider moving, but only if it is in the wrong hallway or else it is blocking
        // pods further in which are in the wrong hallway
        val (empty, occupiedM) = room.span(_.isEmpty)
        assert(occupiedM.forall(_.isDefined))
        val occupied = occupiedM.map(_.get)
        if (occupied.exists(_.roomIdx != roomIdx)) {
          // there is someone in the room with the wrong room -> try to move the topmost pod
          // enumerate moves into hallway
          val pod = occupied.head
          val newSideRoom = room.updated(empty.length, None)
          val numStepsToHallway = empty.length + 1
          val hallwaySquare = hallwaySquareForRoom(roomIdx)
          def movedTo(targetSquare: Int): (State, Int) = {
            val numStepsInHallway = (hallwaySquare - targetSquare).abs
            val newHallway = hallway.updated(targetSquare, Some(pod))
            (
              State(sideRooms.updated(roomIdx, newSideRoom), newHallway),
              (numStepsToHallway + numStepsInHallway) * pod.cost
            )
          }

          val leftSteps = hallwaySquare
            .to(0, -1)
            .iterator
            .takeWhile(hallway(_).isEmpty)
            .filterNot(State.ForbiddenHallwaySquares)
            .map(movedTo)
          val rightSteps = hallwaySquare
            .until(hallway.length)
            .iterator
            .takeWhile(hallway(_).isEmpty)
            .filterNot(State.ForbiddenHallwaySquares)
            .map(movedTo)

          // Check if we can go straight to our room
          val targetRoomIdx = pod.roomIdx
          val targetRoom = sideRooms(targetRoomIdx)
          val toRoomStep =
            if (
              // we're already there, but we need to move out to let someone else out
              targetRoomIdx == roomIdx ||
              // there's no free space in our room
              !targetRoom.exists(_.isEmpty) ||
              // there are amphipods there which don't belong
              targetRoom.flatten.exists(_.roomIdx != targetRoomIdx)
            ) {
              None
            } else {
              val targetSquare = hallwaySquareForRoom(targetRoomIdx)
              if (
                hallwaySquare
                  .to(targetSquare, (targetSquare - hallwaySquare).sign)
                  .iterator
                  .map(hallway)
                  .forall(_.isEmpty)
              ) {
                // we have a path
                val numStepsInHallway = (hallwaySquare - targetSquare).abs
                val numEmpties = targetRoom.iterator.takeWhile(_.isEmpty).length
                val numStepsFromHallway = numEmpties
                val idxInTargetRoom = numEmpties - 1
                Some(
                  (
                    copy(
                      sideRooms = sideRooms
                        .updated(roomIdx, newSideRoom)
                        .updated(
                          pod.roomIdx,
                          targetRoom.updated(idxInTargetRoom, Some(pod))
                        )
                    ),
                    (numStepsToHallway + numStepsInHallway + numStepsFromHallway) * pod.cost
                  )
                )
              } else {
                // don't have a path to our room
                None
              }
            }

          leftSteps ++ rightSteps ++ toRoomStep.iterator
        } else {
          // This room is complete, nothing to do
          Iterator.empty
        }
    }

    // Now try moving pods already in the hallway
    val hallwayMoves = hallway.iterator.zipWithIndex
      .collect { case (Some(pod), idx) =>
        (pod, idx)
      }
      .flatMap { case (pod, hallwaySquare) =>
        val targetRoomIdx = pod.roomIdx
        val targetSquare = hallwaySquareForRoom(targetRoomIdx)
        val targetRoom = sideRooms(targetRoomIdx)
        if (
          // do we have a path?
          hallwaySquare
            .to(targetSquare, (targetSquare - hallwaySquare).sign)
            .iterator
            .drop(1) // we don't need to check if our square is empty - it's not but that's ok!
            .map(hallway)
            .forall(_.isEmpty) &&
          // is there room in the target room?
          targetRoom.exists(_.isEmpty) &&
          // are there no amphipods there that don't belong?
          !targetRoom.flatten.exists(_.roomIdx != targetRoomIdx)
        ) {
          val numStepsInHallway = (hallwaySquare - targetSquare).abs
          val numEmpties = targetRoom.iterator.takeWhile(_.isEmpty).length
          val numStepsFromHallway = numEmpties
          val idxInTargetRoom = numEmpties - 1
          Some(
            State(
              sideRooms.updated(
                targetRoomIdx,
                targetRoom.updated(idxInTargetRoom, Some(pod))
              ),
              hallway.updated(hallwaySquare, None)
            ),
            (numStepsInHallway + numStepsFromHallway) * pod.cost
          )
        } else {
          None
        }
      }

    sideRoomMoves ++ hallwayMoves
  }
}

object State {
  // This works only for 4 side rooms and 2 hallways squares either side
  private val ForbiddenHallwaySquares: Set[Int] = Set(2, 4, 6, 8)

  def fromInitialSideRooms(sideRooms: Vector[Vector[Amphipod]]): State = {
    require(sideRooms.length == 4)
    val sideRoomDepth = sideRooms.head.length
    require(sideRooms.forall(_.length == sideRoomDepth))
    val counts = sideRooms.flatten.groupMapReduce(identity)(_ => 1)(_ + _)
    require(Amphipod.values.forall(typ => counts(typ) == sideRoomDepth))
    State(
      sideRooms.map(_.map(a => Some(a))),
      Vector.fill(11)(None)
    )
  }

  def fromInput: State = {
    val rooms = inputLines(21)(23)
      .map(_.iterator.filter(_.isUpper).toVector)
      .filter(_.length == 4)
      .map(_.map {
        case 'A' => Amphipod.Amber
        case 'B' => Amphipod.Bronze
        case 'C' => Amphipod.Copper
        case 'D' => Amphipod.Desert
      })
      .toVector
      .transpose

    fromInitialSideRooms(rooms)
  }
}

// Dijkstra time again. Like Day 15, we use mutable collections because we don't have a good immutable PQ handy

def findLowestCost(initialState: State): Int = {
  val costs = new util.HashMap[State, Int]()
  val pq =
    new PriorityQueue[(State, Int)](
      Comparator.comparingInt[(State, Int)](_._2)
    )
  pq.add((initialState, 0))
  costs.put(initialState, 0)
  // val prev = new util.HashMap[State, State]()
  val visitedStates = new util.HashSet[State]()
  var finalState: Option[State] = None
  while (finalState.isEmpty) {
    if (pq.isEmpty) {
      throw new IllegalStateException("Ran out of states to try!")
    }
    val (curState, curMinCost) = pq.poll()
    if (curState.isComplete) {
      finalState = Some(curState)
    } else {
      curState.nextStates
        .filterNot { case (s, _) =>
          visitedStates.contains(s)
        }
        .foreach { case (nextState, stepCost) =>
          val tentativeCost = costs.getOrDefault(nextState, Int.MaxValue)
          val candidateCost = curMinCost + stepCost
          if (candidateCost < tentativeCost) {
            costs.put(nextState, candidateCost)
            // no need to remove the old version - see Day 15
            pq.add((nextState, candidateCost))
            // prev.put(nextState, curState)
          }
        }
      visitedStates.add(curState)
    }
  }
//  // debug: show final path
//  val finalPath = (Iterator
//    .iterate(finalState.get)(prev.get)
//    .takeWhile(_ != initialState) ++ Iterator(initialState)).toVector.reverse
//
//  finalPath.foreach { s =>
//    println(s)
//    println()
//  }

  costs.get(finalState.get)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    println(findLowestCost(State.fromInput))
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    println(findLowestCost(State.fromInput.withExtraPods))
  }
}
