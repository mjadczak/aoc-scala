package uk.co.mjdk.aoc21.day08

import uk.co.mjdk.aoc21.inputLines
import scala.math.pow

// 0: abc efg =6
// 1:   c  f  =2
// 2: a cde g =5
// 3: a cd fg =5
// 4:  bcd f  =4
// 5: ab d fg =5
// 6: ab defg =6
// 7: a c  f  =3
// 8: abcdefg =7
// 9: abcd fg =6

// 2 -> 1
// 3 -> 7
// 4 -> 4
// 5 -> 2,3,5
// 6 -> 0,6,9
// 7 -> 8

object Part1 {
  def main(args: Array[String]): Unit = {
    val known = Set(2, 3, 4, 7)

    val count = inputLines(8)
      .flatMap(_.split('|')(1).trim.split(' ').iterator)
      .map(_.length)
      .count(known)

    println(count)
  }
}

extension [T](s: Iterable[T]) {
  def only: T = {
    if (s.size > 1) {
      throw new IllegalStateException(s"Expected single element in set $s")
    }
    s.head
  }
}

extension (n: Int) {
  // Not the efficient way, doesn't need to be
  // Could just use built-in power, but that's not an integer one
  def pow(x: Int): Int = {
    Iterator.iterate(1)(_ * n).drop(x).next()
  }
}

object Part2 {
  def main(args: Array[String]): Unit = {
    val sum =
      inputLines(8).map { line =>
        val Array(ins, outs) =
          line.split('|').map(_.trim.split(' ').map(_.toCharArray.toSet))
        val inSet = ins.toSet.groupBy(_.size)
        val outDigits = outs.toVector

        val d1 = inSet(2).only
        val d7 = inSet(3).only
        val d4 = inSet(4).only
        val d8 = inSet(7).only

        def assertHas(d: Int, s: Set[Char]): Unit = {
          if (!inSet(s.size).contains(s)) {
            throw new IllegalStateException(
              s"Expected digit $d to be represented by $s but it was not in $inSet"
            )
          }
        }

        // 3 is the 5-size set which contains both segments in 1
        val d3 = inSet(5).filter(d1.subsetOf).only

        // 9 is the union of 3 and 4
        val d9 = d3.union(d4)
        assertHas(9, d9)

        // 0 is the 6-size set which is not 9 and which shares both segments with 1
        val d0 = inSet(6).excl(d9).filter(d1.subsetOf).only

        // 6 is the remaining 6-size set
        val d6 = inSet(6).excl(d9).excl(d0).only

        // 8 - (3 union 4) gives us the e segment - 2 contains it and 5 does not
        val eSeg = d8.diff(d3.union(d4)).only
        val d2 = inSet(5).excl(d3).filter(_.contains(eSeg)).only
        val d5 = inSet(5).excl(d3).excl(d2).only

        val digitMap = Map(
          d0 -> 0,
          d1 -> 1,
          d2 -> 2,
          d3 -> 3,
          d4 -> 4,
          d5 -> 5,
          d6 -> 6,
          d7 -> 7,
          d8 -> 8,
          d9 -> 9
        )

        val number =
          outDigits.reverseIterator
            .map(digitMap)
            .zipWithIndex
            .map { (digit, pos) =>
              digit * 10.pow(pos)
            }
            .sum

        number
      }.sum

    println(sum)
  }
}
