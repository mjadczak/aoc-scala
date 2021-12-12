package uk.co.mjdk.aoc15.day04

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.util.chaining._

val Input: String = "yzbqklnj"

val Md5Algo = MessageDigest.getInstance("MD5")

def md5Digest(str: String): Array[Byte] =
  Md5Algo.digest(str.getBytes(StandardCharsets.UTF_8))

def hasZeroes(numZeroes: Int, ba: Array[Byte]): Boolean = {
  if (ba.length < (numZeroes + 1) / 2) {
    false
  } else {
    val wholeZeroes = (0 until (numZeroes / 2)).forall(i => ba(i) == 0)
    if (numZeroes % 2 == 0) {
      wholeZeroes
    } else {
      val lastIndex = numZeroes / 2
      wholeZeroes && (ba(lastIndex) & 0xf0) == 0
    }
  }
}

def findNZeroes(numZeroes: Int): Int =
  Iterator
    .from(1)
    .map(i => (i, md5Digest(s"$Input$i")))
    .find(x => hasZeroes(numZeroes, x._2))
    .get
    ._1

object Part1 {
  def main(args: Array[String]): Unit = {
    findNZeroes(5)
      .pipe(println)
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    findNZeroes(6)
      .pipe(println)
  }
}
