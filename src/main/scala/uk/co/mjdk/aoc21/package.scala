package uk.co.mjdk

import scala.io.Source

package object aoc21 {
  // Normally the stream opened by this function should be closed. In this very particular case we know that the
  // program is pretty much finished after the stream is consumed, so we don't care in the name of clearer logic.
  // Tradeoffs like this is why having something like ZIO around is nice, as you can have your cake and eat it too.
  // Maybe a ZIO version is in the cards - for now, we leak the stream.
  def inputLines(dayNum: Int): Iterator[String] =
    Source
      .fromResource(s"uk/co/mjdk/aoc21/input${dayNum.formatted("%02d")}.txt")
      .getLines()
}
