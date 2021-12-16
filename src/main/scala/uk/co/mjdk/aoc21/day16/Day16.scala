package uk.co.mjdk.aoc21.day16

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec
import scala.util.chaining._

// Yes, it would be better to use byte arrays instead of Vector[Boolean]s, but the data size here is tiny so this
// is easier

enum PacketType(val id: Int) {
  case Literal extends PacketType(4)
  case Other extends PacketType(-1)
}

sealed trait Packet(typ: PacketType) {
  def version: Int
}

object Packet {
  case class Literal(version: Int, value: Long)
      extends Packet(PacketType.Literal)
  case class Operator(version: Int, children: Vector[Packet])
      extends Packet(PacketType.Other)

  private class PacketParser(reader: BitstreamReader) {
    def parse(): Packet = {
      val version = reader
        .readBits(3)
        .tap(s => print(s"VERSION: ${s.toBinaryStr}"))
        .toInt
        .tap(s => println(s" = $s"))
      val typId = reader
        .readBits(3)
        .tap(s => print(s"TYP: ${s.toBinaryStr}"))
        .toInt
        .tap(s => println(s" = $s"))
      typId match {
        case PacketType.Literal.id => parseLiteral(version)
        case _                     => parseOperator(version)
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

      val value = parseValueBits()
        .tap(s => print(s"VALUE: ${s.toBinaryStr}"))
        .toLong
        .tap(s => println(s" = $s"))
      Packet.Literal(version, value)
    }

    def parseOperator(version: Int): Packet.Operator = {
      val lengthTyp = reader.readBit()
      lengthTyp match {
        case false =>
          // read packets until we've read N bits
          val bitLength = reader
            .readBits(15)
            .tap(s => print(s"BITLEN: ${s.toBinaryStr}"))
            .toInt
            .tap(s => println(s" = $s"))
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
          Packet.Operator(version, readPackets())
        case true =>
          // read N packets
          val numPackets = reader
            .readBits(11)
            .tap(s => print(s"PAKLEN: ${s.toBinaryStr}"))
            .toInt
            .tap(s => println(s" = $s"))
          Packet.Operator(
            version,
            0.until(numPackets).iterator.map(_ => parse()).toVector
          )
      }
    }
  }

  def parse(input: Vector[Byte]): Packet = {
    val reader = new BitstreamReader(input)
    val pak = new PacketParser(reader).parse()
    println(s"READER OFFSET: ${reader.offset}, LEN: ${reader.length}")
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

object Blah {
  def main(args: Array[String]): Unit = {
    val input = parseInput
    val packet = Packet.parse(input)

    def addVersions(packet: Packet): Int = {
      packet match {
        case Packet.Literal(version, _) => version
        case Packet.Operator(version, children) =>
          version + children.iterator.map(addVersions).sum
      }
    }

    println(addVersions(packet))
  }
}
