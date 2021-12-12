package uk.co.mjdk

import scala.io.Source
import scala.util.Using

package object aoc {
  // Used to stream these in, and just leak the open source, but that bothered me. The inputs are small enough that
  // we should just read them in wholesale, but it's nice to keep a streaming interface.
  def inputLines(year: Int)(dayNum: Int): Iterator[String] =
    Using
      .resource(
        Source
          .fromResource(
            s"uk/co/mjdk/aoc${year.formatted("%02d")}/input${dayNum.formatted("%02d")}.txt"
          )
      ) {
        _.getLines().toVector
      }
      .iterator

  extension [T](s: Iterable[T]) {
    def only: T = {
      if (s.size > 1) {
        throw new IllegalStateException(s"Expected single element in set $s")
      }
      s.head
    }
  }
}
