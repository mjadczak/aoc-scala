package uk.co.mjdk.aoc21.day16

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.util.chaining._

// Yes, it would be better to use byte arrays instead of Vector[Boolean]s, but the data size here is tiny so this
// is easier

sealed trait PacketType {
  def id: Int
}

object PacketType {
  case object Literal extends PacketType {
    override val id: Int = 4
  }
  sealed trait Operator(override val id: Int) extends PacketType {
    def applyLogic(inputs: Vector[Long]): Long
  }
  object Operator {
    case object Sum extends Operator(0) {
      override def applyLogic(inputs: Vector[Long]): Long = inputs.sum
    }
    case object Product extends Operator(1) {
      override def applyLogic(inputs: Vector[Long]): Long = inputs.product
    }
    case object Minimum extends Operator(2) {
      override def applyLogic(inputs: Vector[Long]): Long = inputs.min
    }
    case object Maximum extends Operator(3) {
      override def applyLogic(inputs: Vector[Long]): Long = inputs.max
    }
    case object GreaterThan extends Operator(5) {
      override def applyLogic(inputs: Vector[Long]): Long = {
        require(inputs.length == 2)
        if (inputs(0) > inputs(1)) 1 else 0
      }
    }
    case object LessThan extends Operator(6) {
      override def applyLogic(inputs: Vector[Long]): Long = {
        require(inputs.length == 2)
        if (inputs(0) < inputs(1)) 1 else 0
      }
    }
    case object Equal extends Operator(7) {
      override def applyLogic(inputs: Vector[Long]): Long = {
        require(inputs.length == 2)
        if (inputs(0) == inputs(1)) 1 else 0
      }
    }
  }

  def forId(id: Int): PacketType = id match {
    case 0 => Operator.Sum
    case 1 => Operator.Product
    case 2 => Operator.Minimum
    case 3 => Operator.Maximum
    case 4 => Literal
    case 5 => Operator.GreaterThan
    case 6 => Operator.LessThan
    case 7 => Operator.Equal
    case n => throw new IllegalArgumentException(s"Invalid type ID $id")
  }
}

sealed trait Packet {
  def version: Int
  def typ: PacketType
  def value: Long
}

object Packet {
  case class Literal(version: Int, value: Long) extends Packet {
    val typ: PacketType = PacketType.Literal
  }
  case class Operator(
      version: Int,
      typ: PacketType.Operator,
      children: Vector[Packet]
  ) extends Packet {
    override def value: Long = typ.applyLogic(children.map(_.value))
  }

  private class PacketParser(reader: BitstreamReader) {
    def parse(): Packet = {
      val version = reader
        .readBits(3)
        .toInt
      val typId = reader
        .readBits(3)
        .toInt
      val packetType = PacketType.forId(typId)
      packetType match {
        case PacketType.Literal     => parseLiteral(version)
        case t: PacketType.Operator => parseOperator(version, t)
      }
    }

    def parseLiteral(version: Int): Packet.Literal = {
      @tailrec
      def parseValueBits(
          result: Vector[Boolean] = Vector()
      ): Vector[Boolean] = {
        val isContinue = reader.readBit()
        val newResult = result ++ reader.readBits(4)
        if (isContinue) {
          parseValueBits(newResult)
        } else {
          newResult
        }
      }

      val value = parseValueBits().toLong
      Packet.Literal(version, value)
    }

    def parseOperator(
        version: Int,
        packetType: PacketType.Operator
    ): Packet.Operator = {
      val lengthTyp = reader.readBit()
      lengthTyp match {
        case false =>
          // read packets until we've read N bits
          val bitLength = reader
            .readBits(15)
            .toInt
          val startOffset = reader.offset
          val endOffset = startOffset + bitLength
          @tailrec
          def readPackets(result: Vector[Packet] = Vector()): Vector[Packet] = {
            val newResult = result :+ parse()
            if (reader.offset < endOffset) {
              readPackets(newResult)
            } else {
              if (reader.offset > endOffset) {
                throw new IllegalStateException(
                  s"Expected to read $bitLength bits of packet, but read ${endOffset - startOffset}"
                )
              }
              newResult
            }
          }
          Packet.Operator(version, packetType, readPackets())
        case true =>
          // read N packets
          val numPackets = reader
            .readBits(11)
            .toInt
          Packet.Operator(
            version,
            packetType,
            0.until(numPackets).iterator.map(_ => parse()).toVector
          )
      }
    }
  }

  def parse(input: Vector[Byte]): Packet = {
    val reader = new BitstreamReader(input)
    val pak = new PacketParser(reader).parse()
    pak
  }

}

extension (ab: Vector[Boolean]) {

  def toLong: Long = {
    val len = ab.length
    // Let's hope no negative numbers
    require(len <= 63)
    // big endian
    ab.iterator.zipWithIndex
      .filter(_._1)
      .map((_, idx) => 1L << (len - (idx + 1)))
      .foldLeft(0L)(_ | _)
  }

  def toInt: Int = {
    val len = ab.length
    // Let's hope no negative numbers
    require(len <= 31)
    // big endian
    ab.iterator.zipWithIndex
      .filter(_._1)
      .map((_, idx) => 1 << (len - (idx + 1)))
      .foldLeft(0)(_ | _)
  }

  def toBinaryStr: String = {
    ab.map(b => if (b) '1' else '0').mkString
  }
}

class BitstreamReader(bytes: Vector[Byte]) {
  private var offset_ = 0
  def offset: Int = offset_
  val length: Int = bytes.length * 8

  def reset(): Unit = offset_ = 0
  def setOffset(newOffset: Int): Unit = {
    require(newOffset < length)
    offset_ = newOffset
  }
  def readBit(): Boolean = {
    if (offset_ >= length) {
      throw new IndexOutOfBoundsException(
        s"$offset_ out of bounds, length is $length"
      )
    }
    val byte = offset_ / 8
    val bit = offset_ % 8
    offset_ += 1
    (bytes(byte) & (1 << (7 - bit))) != 0
  }
  def readBits(numBits: Int): Vector[Boolean] = {
    // There's almost certainly a more efficient way to get multiple bits at a time
    // We don't care in this case
    0.until(numBits).iterator.map(idx => readBit()).toVector
  }
}

def parseInput: Vector[Byte] =
  inputLines(21)(16)
    .next()
    .grouped(2)
    .map(s => Integer.parseInt(s, 16).toByte)
    .toVector

object Part1 {
  def main(args: Array[String]): Unit = {
    val input = parseInput
    val packet = Packet.parse(input)

    def addVersions(packet: Packet): Int = {
      packet match {
        case Packet.Literal(version, _) => version
        case Packet.Operator(version, _, children) =>
          version + children.iterator.map(addVersions).sum
      }
    }

    println(addVersions(packet))
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val input = parseInput
    val packet = Packet.parse(input)
    println(packet)
    println(packet.value)
  }
}
