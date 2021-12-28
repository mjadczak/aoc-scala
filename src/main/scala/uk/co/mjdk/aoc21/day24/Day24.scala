package uk.co.mjdk.aoc21.day24

import uk.co.mjdk.aoc.inputLines

enum Register {
  case W
  case X
  case Y
  case Z
}

enum Operand {
  case Reg(register: Register)
  case Lit(value: Int)
}

enum Instruction {
  case Inp(reg: Register)
  case Add(left: Register, right: Operand)
  case Mul(left: Register, right: Operand)
  case Div(left: Register, right: Operand)
  case Mod(left: Register, right: Operand)
  case Eql(left: Register, right: Operand)
}

object Instruction {
  private val InpPat = """inp ([wxyz])""".r
  private val OpPat = """([a-z]{3}) ([wxyz]) ([wxyz]|[-\d]+)""".r
  def parse(input: String): Instruction = {
    input match {
      case InpPat(regC) =>
        Inp(Register.valueOf(regC.toUpperCase()))
      case OpPat(inst, l, r) =>
        val lReg = Register.valueOf(l.toUpperCase())
        val rOp = r.toIntOption
          .map(Operand.Lit.apply)
          .getOrElse(Operand.Reg(Register.valueOf(r.toUpperCase())))
        inst match {
          case "add" => Add(lReg, rOp)
          case "mul" => Mul(lReg, rOp)
          case "div" => Div(lReg, rOp)
          case "mod" => Mod(lReg, rOp)
          case "eql" => Eql(lReg, rOp)
        }
    }
  }
}

case class Alu private (
    registers: Map[Register, Int],
    inputs: List[Int]
) {
  private def executeBinary(
      operation: (Int, Int) => Int,
      reg: Register,
      op: Operand
  ): Alu = {
    val l = registers(reg)
    val r = op match {
      case Operand.Lit(v) => v
      case Operand.Reg(r) => registers(r)
    }
    val result = operation(l, r)
    copy(
      registers = registers + (reg -> result)
    )
  }

  def execute(instruction: Instruction): Alu = instruction match {
    case Instruction.Inp(reg) =>
      val input :: rest = inputs
      copy(
        registers = registers + (reg -> input),
        inputs = rest
      )
    case Instruction.Add(l, r) => executeBinary(_ + _, l, r)
    case Instruction.Mul(l, r) => executeBinary(_ * _, l, r)
    case Instruction.Div(l, r) => executeBinary(_ / _, l, r)
    case Instruction.Mod(l, r) => executeBinary(_ % _, l, r)
    case Instruction.Eql(l, r) =>
      executeBinary((a, b) => if (a == b) 1 else 0, l, r)
  }
}

object Alu {
  def initial(inputs: List[Int]): Alu = Alu(
    Register.values.iterator.map(_ -> 0).toMap,
    inputs
  )
}

def checkNumer(program: Iterable[Instruction], input: List[Int]): Boolean = {
  val finalState =
    program.foldLeft(Alu.initial(input))((alu, inst) => alu.execute(inst))
  finalState.registers(Register.Z) == 0
}

// Yes, it's bad to manipulate integers with string operations, but I'm being lazy
def allPossibleNumbers: Iterator[List[Int]] =
  Iterator
    .iterate(java.lang.Long.parseLong("9" * 14))(_ - 1)
    .tapEach(l => if (l % 1000000 == 0) println(l))
    .map(_.toString)
    .filterNot(_.contains('0'))
    .map(_.iterator.map(_.asDigit).toList)

// Brute-force, committing for posterity but this is too slow
object Part1 {
  def main(args: Array[String]): Unit = {
    val program = inputLines(21)(24).map(Instruction.parse).toVector
    val firstValid = allPossibleNumbers
      .find(checkNumer(program, _))
      .get
    val validValue = java.lang.Long.parseLong(firstValid.mkString)
    println(validValue)
  }
}
