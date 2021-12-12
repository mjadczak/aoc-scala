package uk.co.mjdk.aoc15.day04

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.util.chaining._

val Input: String = "yzbqklnj"

val Md5Algo = MessageDigest.getInstance("MD5")

def md5Digest(str: String): Array[Byte] =
  Md5Algo.digest(str.getBytes(StandardCharsets.UTF_8))

def has5InitialZeroes(ba: Array[Byte]): Boolean = {
  // 5 zeroes => two 0 bytes and a byte with upper half of 0
  if (ba.length < 3) {
    false
  } else {
    ba(0) == 0 && ba(1) == 0 && (ba(2) & 0xf0) == 0
  }
}

object Part1 {
  def main(args: Array[String]): Unit = {
    Iterator
      .from(1)
      .map(i => (i, md5Digest(s"$Input$i")))
      .find(x => has5InitialZeroes(x._2))
      .get
      ._1
      .pipe(println)
  }
}
