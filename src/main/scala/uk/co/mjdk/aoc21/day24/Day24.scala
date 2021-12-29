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
  private val InpPat = """inp ([wxyz])(?:\s*;.*)?""".r
  private val OpPat = """([a-z]{3}) ([wxyz]) ([wxyz]|[-\d]+)(?:\s*;.*)?""".r
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
    registers: Map[Register, Long],
    inputs: List[Int]
) {
  private def executeBinary(
      operation: (Long, Long) => Long,
      reg: Register,
      op: Operand
  ): Alu = {
    val l = registers(reg)
    val r = op match {
      case Operand.Lit(v) => v.toLong
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
    Register.values.iterator.map(_ -> 0L).toMap,
    inputs
  )
}

def checkNumber(program: Iterable[Instruction], input: List[Int]): Long = {
  val finalState =
    program.foldLeft(Alu.initial(input))((alu, inst) => alu.execute(inst))
//  val finalReg = finalState.registers(Register.Z)
//  val decompiled = checkNumberDecompiled(input)
//  if (finalReg != decompiled) {
//    println(
//      s"Mismatch on input ${input.mkString}: asm $finalReg, decompiled $decompiled"
//    )
//  }
  finalState.registers(Register.Z)
}

// Below was hand-decompiled from the program. Notice that the program is just an unrolled loop of the form below
// for each digit, with only three parameters which change (call them D, A, B). x, y, and w are reset each iteration.

// inp w
// mul x 0
// add x z
// mod x 26
// div z 1  ; <- D=false if 1, D=true if 26
// add x 12 ; <- A
// eql x w
// eql x 0
// mul y 0
// add y 25
// mul y x
// add y 1
// mul z y
// mul y 0
// add y w
// add y 4  ; <- B
// mul y x
// add z y

case class DigitParams(d: Boolean, a: Int, b: Int)

// In the spirit of "don't hardcode things" the below could be extracted with a big pattern match on chunks of the
// input, but I'm not sure whether all possible inputs are of the same form with only those 3 parameters changing, so
// I won't over-generalise
object DigitParams {
  val Decompiled: Vector[DigitParams] = Vector(
    DigitParams(false, 12, 4),
    DigitParams(false, 11, 11),
    DigitParams(false, 13, 5),
    DigitParams(false, 11, 11),
    DigitParams(false, 14, 14),
    DigitParams(true, -10, 7),
    DigitParams(false, 11, 11),
    DigitParams(true, -9, 4),
    DigitParams(true, -3, 6),
    DigitParams(false, 13, 5),
    DigitParams(true, -5, 9),
    DigitParams(true, -10, 12),
    DigitParams(true, -4, 14),
    DigitParams(true, -5, 14)
  )
}

// z is a stack of base-26 numbers. We have 7 D=false (df) and 7 D=true (dt) steps. On a df step, we must add
// a new number to the stack, as the A params are all > 8. On a dt step we remove a number from the stack, but have
// a chance to add a new one. Because of the number of each type of step, to get z=0 (empty stack) at the end, we need
// to ensure that we never add a number to the stack on a dt step.

def checkNumberDecompiled(input: List[Int]): Long =
  input.zip(DigitParams.Decompiled).iterator.foldLeft(0L) {
    case (z, (digit, params)) =>
      val z1 = if (params.d) z / 26 else z
      if (digit != (z % 26) + params.a) (z1 * 26) + digit + params.b else z1
  }

enum DigitSort {
  case Ascending
  case Descending
}

// Because we consider most significant numbers first, we can use a greedy strategy and always pick the best number
// we can for each step. For df steps, we just pick 9 (or 1 for smallest), and for the dt steps we pick whatever
// digit we need to
def findValidNumber(
    direction: DigitSort,
    checkerProgram: Vector[Instruction]
): Long = {
  val candidateDigits = direction match {
    case DigitSort.Ascending  => 1.to(9)
    case DigitSort.Descending => 9.to(1, -1)
  }

  case class State(
      digitsChosen: Vector[Int] = Vector(),
      zContents: List[Int] = List()
  ) {
    def curZ: Int = zContents.headOption.getOrElse(0)
    def withDfStep(chosenDigit: Int, newZDigit: Int): State = {
      copy(
        digitsChosen = digitsChosen.appended(chosenDigit),
        zContents = newZDigit :: zContents
      )
    }
    def withDtStep(chosenDigit: Int): State = {
      copy(
        digitsChosen = digitsChosen.appended(chosenDigit),
        zContents = zContents.tail
      )
    }
  }

  val finalState =
    DigitParams.Decompiled.tails.takeWhile(_.nonEmpty).foldLeft(State()) {
      case (state, params +: rest) =>
        if (params.d) {
          // dt step - we pick the digit so as to not add a new entry to the stack - so we don't care about B
          val chosenDigit = state.curZ + params.a
          if (chosenDigit > 9 || chosenDigit < 1) {
            // This should not happen, we should have already taken care of this in the df step
            throw new IllegalStateException(
              s"Could not choose a valid digit - curZ is ${state.curZ} and A is ${params.a}, so need invalid digit $chosenDigit"
            )
          }
          state.withDtStep(chosenDigit)
        } else {
          // df step - we pick the first digit we can, but we need to look ahead and see who will be popping us off
          // and make sure they can choose a valid digit
          val correspondingIdx = rest.iterator
            .map(p => if (p.d) -1 else 1)
            .scanLeft(1)(_ + _)
            .zipWithIndex
            .find(_._1 == 0)
            .get
            ._2 - 1
          val correspondingParams = rest(correspondingIdx)
          assert(correspondingParams.d)
          def isValidDigit(digit: Int): Boolean = {
            val newZDigit = digit + params.b
            // they will need to choose a digit such that digit + A = our new Z
            val theirDigit = newZDigit + correspondingParams.a
            theirDigit <= 9 && theirDigit >= 1
          }
          val chosenDigit = candidateDigits.find(isValidDigit).get
          // there is no way we can choose a digit to not add an entry to the stack, so we don't even care about A
          val newZDigit = chosenDigit + params.b
          if (newZDigit >= 26) {
            throw new IllegalStateException(
              s"Expected z digits to be base-26 but calculated $newZDigit"
            )
          }
          state.withDfStep(chosenDigit, newZDigit)
        }
    }

  if (finalState.zContents.nonEmpty) {
    throw new IllegalStateException(
      s"Expected Z-stack to be empty at the end, but contained ${finalState.zContents}"
    )
  }

  // Now double-check with the original program
  val zRes = checkNumber(checkerProgram, finalState.digitsChosen.toList)
  if (zRes != 0) {
    throw new IllegalStateException(
      s"MONAD disagrees with our number - returned $zRes"
    )
  }

  java.lang.Long.parseLong(finalState.digitsChosen.mkString)
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
    val chosen = findValidNumber(DigitSort.Descending, program)
    println(chosen)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val program = inputLines(21)(24).map(Instruction.parse).toVector
    val chosen = findValidNumber(DigitSort.Ascending, program)
    println(chosen)
  }
}
