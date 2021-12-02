package uk.co.mjdk.aoc21.day2

import uk.co.mjdk.aoc21.day1.Part1.getClass

import scala.io.Source
import scala.util.Using

object Part2 {
  enum Command {
    case Forward(x: Int)
    case Down(depth: Int)
    case Up(depth: Int)
  }

  case class Position(horizontalPosition: Int, depth: Int, aim: Int) {
    def movedBy(command: Command): Position = command match {
      case Command.Forward(x) =>
        copy(
          horizontalPosition = horizontalPosition + x,
          depth = depth + aim * x
        )
      case Command.Down(d) => copy(aim = aim + d)
      case Command.Up(d)   => copy(aim = aim - d)
    }
  }

  object Position {
    val zero: Position = Position(0, 0, 0)
  }

  def main(args: Array[String]): Unit = {
    val finalPos = Using.resource(
      Source.fromInputStream(getClass.getResourceAsStream("input.txt"))
    ) { src =>
      src
        .getLines()
        .map { str =>
          val parts = str.split(' ')
          assert(parts.length == 2)
          val num = parts(1).toInt
          parts(0) match {
            case "forward" => Command.Forward(num)
            case "down"    => Command.Down(num)
            case "up"      => Command.Up(num)
          }
        }
        .foldLeft(Position.zero)((pos, cmd) => pos.movedBy(cmd))
    }

    println(finalPos.depth * finalPos.horizontalPosition)
  }
}
