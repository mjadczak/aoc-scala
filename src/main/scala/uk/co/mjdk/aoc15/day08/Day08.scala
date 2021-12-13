package uk.co.mjdk.aoc15.day08

import uk.co.mjdk.aoc.inputLines

import scala.annotation.tailrec

def parseString(input: String): String = parseString(input.toCharArray.toList)

@tailrec
def parseString(
    input: List[Char],
    output: String = ""
): String = input match {
  case Nil =>
    output
  case '"' :: rest if output.isEmpty || rest.isEmpty =>
    parseString(rest, output)
  case '\\' :: '\\' :: rest =>
    parseString(rest, output + '\\')
  case '\\' :: '"' :: rest =>
    parseString(rest, output + '"')
  case '\\' :: 'x' :: h :: l :: rest =>
    val char = (Integer
      .parseInt(h.toString, 16) << 2 + Integer.parseInt(l.toString, 16)).toChar
    parseString(rest, output + char)
  case c :: rest =>
    parseString(rest, output + c)
}

def encodeString(input: String): String = encodeString(input.toCharArray.toList)

@tailrec
def encodeString(
    input: List[Char],
    output: String = ""
): String = input match {
  case Nil =>
    output + '"'
  case _ if output.isEmpty =>
    encodeString(input, output + '"')
  case '"' :: rest =>
    encodeString(rest, output + """\"""")
  case '\\' :: rest =>
    encodeString(rest, output + """\\""")
  case c :: rest =>
    encodeString(rest, output + c)
}

object Part1 {
  def main(args: Array[String]): Unit = {
    val total = inputLines(15)(8).map { line =>
      val parsed = parseString(line)
      line.length - parsed.length
    }.sum
    println(total)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val total = inputLines(15)(8).map { line =>
      val encoded = encodeString(line)
      encoded.length - line.length
    }.sum
    println(total)
  }
}
