package com.jh.board

import org.specs2.mutable._


class BoardSpec extends Specification {

  "Board" should {
    "should return correct squares to the left of given position" in {
      Board.squaresLeftOf(Square('e', 4)) must_=== Vector(Square('a', 4), Square('b', 4), Square('c', 4), Square('d', 4))
      Board.squaresLeftOf(Square('a', 4)) must_=== Vector()
    }

    "should return correct squares to the right of given position" in {
      Board.squaresRightOf(Square('e', 4)) must_=== Vector(Square('f', 4), Square('g', 4), Square('h', 4))
      Board.squaresRightOf(Square('h', 4)) must_=== Vector()
    }
  }
}
