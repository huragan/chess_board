package com.jh.board

import org.specs2.mutable._


class BoardSpec extends Specification {

  "Board" should {
    "should return correct squares to the left of given position" in {
      Square.untilValid(Square('e', 4), (_.leftSquare)) must_=== List(Square('d', 4), Square('c', 4), Square('b', 4), Square('a', 4))
      Board.squaresLeftOf(Square('a', 4)) must_=== Vector()
    }

    "should return correct squares to the right of given position" in {
      Board.squaresRightOf(Square('e', 4)) must_=== Vector(Square('f', 4), Square('g', 4), Square('h', 4))
      Board.squaresRightOf(Square('h', 4)) must_=== Vector()
    }

    "should return correct squares to the top of given position" in {
      Board.squaresTopOf(Square('e', 4)) must_=== Vector(Square('e', 5), Square('e', 6), Square('e', 7), Square('e', 8))
      Board.squaresTopOf(Square('e', 8)) must_=== Vector()
    }

    "should return correct squares to the bottom of given position" in {
      Board.squaresBottomOf(Square('e', 4)) must_=== Vector(Square('e', 1), Square('e', 2), Square('e', 3))
      Board.squaresBottomOf(Square('e', 1)) must_=== Vector()
    }
  }
}
