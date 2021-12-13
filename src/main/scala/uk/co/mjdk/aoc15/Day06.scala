package uk.co.mjdk.aoc15

import uk.co.mjdk.aoc.inputLines
import scala.util.chaining._

case class Pos(x: Int, y: Int) {
  def to(other: Pos): Iterator[Pos] = {
    require(x <= other.x && y <= other.y)
    (x to other.x).iterator.flatMap { newX =>
      (y to other.y).iterator.map(newY => Pos(newX, newY))
    }
  }
}

enum InstructionType {
  case TurnOn
  case TurnOff
  case Toggle
}

object InstructionType {
  def parse(input: String): InstructionType = input match {
    case "turn on"  => TurnOn
    case "turn off" => TurnOff
    case "toggle"   => Toggle
  }
}

case class Instruction(typ: InstructionType, start: Pos, end: Pos)

object Instruction {
  private val InstructionPattern =
    """(turn off|turn on|toggle) (\d{1,3}),(\d{1,3}) through (\d{1,3}),(\d{1,3})""".r

  def parse(input: String): Instruction =
    input match {
      case InstructionPattern(instructionType, startX, startY, endX, endY) =>
        Instruction(
          InstructionType.parse(instructionType),
          Pos(startX.toInt, startY.toInt),
          Pos(endX.toInt, endY.toInt)
        )
    }
}

// Normally I would use immutable data structures for these, but in this case it's ungodly slow - let's give in
// and mutate some arrays

def updateWithInstruction[T](grid: Array[Array[T]])(
    parseInstruction: InstructionType => T => T
)(instruction: Instruction): Unit = {
  val update = parseInstruction(instruction.typ)
  instruction.start
    .to(instruction.end)
    .foreach(pos => grid(pos.x)(pos.y) = update(grid(pos.x)(pos.y)))
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val grid: Array[Array[Boolean]] = Array.fill(1000)(Array.fill(1000)(false))

    val update = updateWithInstruction(grid) {
      case InstructionType.TurnOn  => _ => true
      case InstructionType.TurnOff => _ => false
      case InstructionType.Toggle  => t => !t
    }

    inputLines(15)(6)
      .map(Instruction.parse)
      .foreach { update }

    val lightsOn = grid.iterator.flatMap(_.iterator).count(identity)

    println(lightsOn)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val grid: Array[Array[Int]] = Array.fill(1000)(Array.fill(1000)(0))

    val update = updateWithInstruction(grid) {
      case InstructionType.TurnOn  => x => x + 1
      case InstructionType.TurnOff => x => 0.max(x - 1)
      case InstructionType.Toggle  => x => x + 2
    }

    inputLines(15)(6)
      .map(Instruction.parse)
      .foreach { update }

    val totalBrightness = grid.iterator.flatMap(_.iterator).sum

    println(totalBrightness)
  }
}
