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
    Square.untilValid(curPos:Square, (_.leftSquare)),
    Square.untilValid(curPos:Square, (_.rightSquare)),
    Square.untilValid(curPos:Square, (_.topSquare)),
    Square.untilValid(curPos:Square, (_.bottomSquare))
  )

  override def toString: String = if (color == 'w) "R" else "r"

}

case class Square(col : Char, row : Int) {
  def prevRow: Int  = row - 1
  def nextRow: Int  = row + 1
  def prevCol: Char = (col.toInt - 1) toChar
  def nextCol: Char = (col.toInt + 1) toChar
  
  def topSquare: Square         = Square(col, nextRow)
  def topLeftSquare: Square     = Square(prevCol, nextRow)
  def topRightSquare: Square    = Square(nextCol, nextRow)
  def leftSquare: Square        = Square(prevCol, row)
  def rightSquare: Square       = Square(nextCol, row)
  def bottomSquare: Square      = Square(col, prevRow)
  def bottomLeftSquare: Square  = Square(prevCol, prevRow)
  def bottomRightSquare: Square = Square(nextCol, prevRow)

  def isValid: Boolean = (Board.cols contains col) && (Board.rows contains row)
 
  override def toString: String = col.toString + row.toString
}
object Square {
  def untilValid(cur:Square, f: Square => Square): List[Square] = {
    val skipFirst = f(cur)
    until(skipFirst, f)
  }

  def until(cur:Square, f: Square => Square): List[Square] =
  if (cur.isValid) cur :: until(f.apply(cur), f) else Nil
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
