package com.jh.board

import org.specs2.mutable._


class RookSpec extends Specification {

  "Rook" should {
    "should return all posible moves" in {
      Rook('b).basicMoves(Square('d', 4)) must_===
      List(
        Vector(Square('c', 4), Square('b', 4), Square('a', 4)),
        Vector(Square('e', 4), Square('f', 4), Square('g', 4), Square('h', 4)),
        Vector(Square('d', 5), Square('d', 6), Square('d', 7), Square('d', 8)),
        Vector(Square('d', 3), Square('d', 2), Square('d', 1))
      )
    }
  }
}
