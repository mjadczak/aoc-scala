package uk.co.mjdk.aoc15.day07

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.collection.immutable.Queue

// JVM's handling of various integer sizes is not great - it's easiest to just use ints

enum Operand {
  case Const(value: Int)
  case Gate(id: String)
}

object Operand {
  def from(input: String): Operand =
    input.toIntOption.map(Operand.Const.apply).getOrElse(Operand.Gate(input))
}

sealed trait Gate extends Product {
  def calculate(lookup: String => Int): Int
  def dependencies: Set[String]

  protected def getValue(lookup: String => Int, operand: Operand): Int =
    operand match {
      case Operand.Const(value) => value
      case Operand.Gate(id)     => lookup(id)
    }

  protected def idOrNone(operand: Operand): Option[String] = operand match {
    case Operand.Const(_) => None
    case Operand.Gate(id) => Some(id)
  }
}

object Gate {
  sealed trait UnaryGate(operator: Int => Int) extends Gate {
    val operand: Operand
    override val dependencies: Set[String] = Set(idOrNone(operand)).flatten
    override def calculate(lookup: String => Int): Int =
      operator(getValue(lookup, operand))
  }
  case class Not(operand: Operand) extends UnaryGate(~_)
  case class Id(operand: Operand) extends UnaryGate(identity)

  sealed trait BinaryGate(operator: (Int, Int) => Int) extends Gate {
    val left: Operand
    val right: Operand
    override val dependencies: Set[String] =
      Set(idOrNone(left), idOrNone(right)).flatten
    override def calculate(lookup: String => Int): Int =
      operator(getValue(lookup, left), getValue(lookup, right))
  }
  case class And(left: Operand, right: Operand) extends BinaryGate(_ & _)
  case class Or(left: Operand, right: Operand) extends BinaryGate(_ | _)
  case class LShift(left: Operand, right: Operand) extends BinaryGate(_ << _)
  case class RShift(left: Operand, right: Operand) extends BinaryGate(_ >>> _)
}

// Returns lookup of ID to gate, and also a map of ID -> downstream deps)
def parseGates: Map[String, Gate] = {
  // Would use a parsing lib at this point, but all the good ones have not been ported to Scala 3 yet :(
  val IdPat = """(\d+|[a-z]+) -> ([a-z]+)""".r
  val NotPat = """NOT (\d+|[a-z]+) -> ([a-z]+)""".r
  val BinaryPat = """(\d+|[a-z]+) ([A-Z]+) (\d+|[a-z]+) -> ([a-z]+)""".r

  inputLines(15)(7).map {
    case NotPat(src, target) =>
      target -> Gate.Not(Operand.from(src))
    case IdPat(src, target) =>
      target -> Gate.Id(Operand.from(src))
    case BinaryPat(left, "AND", right, target) =>
      target -> Gate.And(Operand.from(left), Operand.from(right))
    case BinaryPat(left, "OR", right, target) =>
      target -> Gate.Or(Operand.from(left), Operand.from(right))
    case BinaryPat(left, "LSHIFT", right, target) =>
      target -> Gate.LShift(Operand.from(left), Operand.from(right))
    case BinaryPat(left, "RSHIFT", right, target) =>
      target -> Gate.RShift(Operand.from(left), Operand.from(right))

  }.toMap

}

def calculateDeps(gates: Map[String, Gate]): Map[String, Set[String]] = {
  gates.iterator.foldLeft(Map.empty[String, Set[String]]) {
    case (map, id -> gate) =>
      gate.dependencies.foldLeft(map) { case (map, depId) =>
        map.updatedWith(depId) {
          case None      => Some(Set(id))
          case Some(set) => Some(set + id)
        }
      }
  }
}

def emulate(
    gates: Map[String, Gate],
    dependentLookup: Map[String, Set[String]]
): Map[String, Int] = {
  @tailrec
  def doEmulate(
      queue: Queue[String],
      result: Map[String, Int]
  ): Map[String, Int] = {
    queue.dequeueOption match {
      case None => result
      case Some((gateId, queueRest)) =>
        val gate = gates(gateId)
        // println(s"$gateId <- $gate")
        val signal = gate.calculate(result)
        val newResult = result + (gateId -> signal)
        // We should have just done a topo sort or DFS + memoization, but I started this way
        doEmulate(
          queueRest.appendedAll(
            dependentLookup.getOrElse(gateId, Set.empty).filter { depGate =>
              gates(depGate).dependencies.subsetOf(newResult.keySet)
            }
          ),
          newResult
        )
    }
  }

  val initialQueue =
    gates.iterator.filter(_._2.dependencies.isEmpty).map(_._1).to(Queue)
  doEmulate(initialQueue, Map.empty)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val gates = parseGates
    val deps = calculateDeps(gates)
    val result = emulate(gates, deps)
    println(result("a"))
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val gates1 = parseGates
    val deps1 = calculateDeps(gates1)
    val result1 = emulate(gates1, deps1)
    val a = result1("a")
    val gates2 = gates1 + ("b" -> Gate.Id(Operand.Const(a)))
    val deps2 = calculateDeps(gates2)
    val result2 = emulate(gates2, deps2)
    println(result2("a"))
  }
}
