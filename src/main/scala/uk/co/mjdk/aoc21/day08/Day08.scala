package uk.co.mjdk.aoc21.day08

import uk.co.mjdk.aoc21.inputLines

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

// Don't model the problem in a very detailed way before we know part 2
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
