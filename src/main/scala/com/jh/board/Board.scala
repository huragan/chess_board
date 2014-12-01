package com.jh.board

sealed trait Piece {
  def canMove(pos: Square): Boolean = true
  def basicMoves(curPos: Square): Seq[Seq[Square]]
}

case class King(color : Symbol) extends Piece {

  def basicMoves(curPos: Square): Seq[Seq[Square]] = List(
    List(
      curPos.topLeftSquare,    curPos.topSquare,    curPos.topRightSquare,
      curPos.leftSquare,                            curPos.rightSquare,
      curPos.bottomLeftSquare, curPos.bottomSquare, curPos.bottomRightSquare
    ).filter(_.isValid)
  )
  
  override def toString: String = if (color == 'w) "K" else "k"
}

case class Rook(color : Symbol) extends Piece {
  def basicMoves(curPos: Square): Seq[Seq[Square]] = List(
    Board.squaresLeftOf(curPos),
    Board.squaresRightOf(curPos),
    Board.squaresTopOf(curPos),
    Board.squaresBottomOf(curPos)
  )

  override def toString: String = if (color == 'w) "R" else "r"

}

case class Square(col : Char, row : Int) {
  def prevRow: Int  = row - 1
  def nextRow: Int  = row + 1
  def prevCol: Char = (col.toInt - 1) toChar
  def nextCol: Char = (col.toInt + 1) toChar

  
  def topSquare: Square         = Square(nextCol, row)
  def topLeftSquare: Square     = Square(nextCol, prevRow)
  def topRightSquare: Square    = Square(nextCol, nextRow)
  def leftSquare: Square        = Square(col, prevRow)
  def rightSquare: Square       = Square(col, nextRow)
  def bottomSquare: Square      = Square(prevCol, row)
  def bottomLeftSquare: Square  = Square(prevCol, prevRow)
  def bottomRightSquare: Square = Square(prevCol, nextRow)

  def isValid: Boolean = (Board.cols contains col) && (Board.rows contains row)

  override def toString: String = col.toString + row.toString
}

case class Board(val board: Map[Square, Piece]) {
  def move(from: Square, to: Square): Board = board.get(from) match {
    case Some(piece) => {
      if (piece.canMove(from)) {
        Board((board - from) + (to -> piece))
      }
      else this
    }
    case None        => this
  }

  override def toString = {
    Board.board.map(sqr =>
      board.get(sqr) match {
        case Some(p) => p.toString
        case None    => "_"
      }
    ) grouped(Board.rows.size) map (_ mkString " ") mkString "\n"
  }
}

object Board {
  val cols =   ('a' to 'h')
  val rows   = ( 1  to  8 )

  val board: List[Square] = (for {
    col <- cols
    row <- rows
  } yield Square(col, row)).toList

  def squaresLeftOf(pos: Square): Seq[Square]   = (cols takeWhile (c => c < pos.col))  map (c => Square(c, pos.row))
  def squaresRightOf(pos: Square): Seq[Square]  = (cols dropWhile (c => c <= pos.col)) map (c => Square(c, pos.row))
  def squaresBottomOf(pos: Square): Seq[Square] = (rows takeWhile (r => r < pos.row))  map (r => Square(pos.col, r))
  def squaresTopOf(pos:Square): Seq[Square]     = (rows dropWhile (r => r <= pos.row)) map (r => Square(pos.col, r))

  def apply(): Board = new Board(
    Map(
      Square('a', 1) -> King('w),
      Square('e', 4) -> King('b)
    )
  )

  def main(args: Array[String]) = {
    val board = Board()
    println(board.move(Square('a', 1), Square('a', 2)))
  }
}
