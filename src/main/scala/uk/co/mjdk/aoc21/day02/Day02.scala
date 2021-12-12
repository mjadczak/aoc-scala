package uk.co.mjdk.aoc21.day02

import uk.co.mjdk.aoc.inputLines

enum Command {
  case Forward(x: Int)
  case Down(depth: Int)
  case Up(depth: Int)
}

def commands: Iterator[Command] = inputLines(21)(2).map { str =>
  val parts = str.split(' ')
  assert(parts.length == 2)
  val num = parts(1).toInt
  parts(0) match {
    case "forward" => Command.Forward(num)
    case "down"    => Command.Down(num)
    case "up"      => Command.Up(num)
  }
}

case class Position1(horizontalPosition: Int, depth: Int) {
  def movedBy(command: Command): Position1 = command match {
    case Command.Forward(x) =>
      copy(horizontalPosition = horizontalPosition + x)
    case Command.Down(d) => copy(depth = depth + d)
    case Command.Up(d)   => copy(depth = depth - d)
  }
}

object Position1 {
  val zero: Position1 = Position1(0, 0)
}

case class Position2(horizontalPosition: Int, depth: Int, aim: Int) {
  def movedBy(command: Command): Position2 = command match {
    case Command.Forward(x) =>
      copy(
        horizontalPosition = horizontalPosition + x,
        depth = depth + aim * x
      )
    case Command.Down(d) => copy(aim = aim + d)
    case Command.Up(d)   => copy(aim = aim - d)
  }
}

object Position2 {
  val zero: Position2 = Position2(0, 0, 0)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val finalPos =
      commands.foldLeft(Position1.zero)((pos, cmd) => pos.movedBy(cmd))
    println(finalPos.depth * finalPos.horizontalPosition)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val finalPos =
      commands.foldLeft(Position2.zero)((pos, cmd) => pos.movedBy(cmd))
    println(finalPos.depth * finalPos.horizontalPosition)
  }
}
