package com.jh.board

import org.specs2.mutable._


class KingMovmentSpec extends Specification {

  "King" should {
    "should return all posible moves" in {
      King('b).basicMoves(Square('d', 4)) must_===
      List(
        List(
          Square('c', 5), Square('d', 5), Square('e', 5),
          Square('c', 4),                 Square('e', 4),
          Square('c', 3), Square('d', 3), Square('e', 3)
        )
      )
    }

    "should return only valid squares" in {
      King('b).basicMoves(Square('a', 1)) must_===
      List(
        List(
          
          Square('a', 2), Square('b', 2),
                          Square('b', 1)
        )
      )
    }
  }
}
