package uk.co.mjdk.aoc21.day22

import uk.co.mjdk.aoc.inputLines

case class Cube(
    x: Range,
    y: Range,
    z: Range
) {
  def isInInitializationRegion: Boolean = {
    def isRangeOk(range: Range): Boolean =
      range.start >= -50 && range.last <= 50
    isRangeOk(x) && isRangeOk(y) && isRangeOk(z)
  }
}

case class Instruction(
    on: Boolean,
    cube: Cube
)

def parseInput: Vector[Instruction] = {
  val Pat =
    """(on|off) x=([-\d]+)\.\.([-\d]+),y=([-\d]+)\.\.([-\d]+),z=([-\d]+)\.\.([-\d]+)""".r
  inputLines(21)(22).map {
    case Pat(onoff, xMin, xMax, yMin, yMax, zMin, zMax) =>
      val isOn = onoff match {
        case "on"  => true
        case "off" => false
      }
      Instruction(
        isOn,
        Cube(
          xMin.toInt.to(xMax.toInt),
          yMin.toInt.to(yMax.toInt),
          zMin.toInt.to(zMax.toInt)
        )
      )
  }.toVector
}

object Part1 {
  def main(args: Array[String]): Unit = {
    // quick and dirty solution for now is fine - we'll need to be smarter for part 2
    val ary = Array.fill(101, 101, 101)(false)
    val instructions = parseInput.filter(_.cube.isInInitializationRegion)
    instructions.foreach { inst =>
      for {
        x <- inst.cube.x
        y <- inst.cube.y
        z <- inst.cube.z
      } ary(x + 50)(y + 50)(z + 50) = inst.on
    }

    val cubesOn = ary.iterator.flatMap(_.flatMap(_.iterator)).count(identity)
    println(cubesOn)
  }
}
