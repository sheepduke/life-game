package lifegame

import scala.util.Random

type Board = Array[Array[Boolean]]

object Board {
  def empty(boardSize: Int) = {
    Array.ofDim[Boolean](boardSize, boardSize)
  }
}

extension (board: Board) {
  def placeRandomSeeds(seedCount: Int) = {
    val boardLength = board.length
    val indices = Range.Exclusive(0, boardLength * boardLength, 1)
    val seeds = Random.shuffle(indices).take(seedCount)
    for (index <- seeds) {
      board(index / boardLength)(index % boardLength) = true
    }
  }

  def print(): Unit = {
    for (row <- board) {
      for (cell <- row) {
        scala.Predef.print(if cell then "X " else "- ")
      }
      println()
    }
  }

  def evolve(): Board = {
    val newBoard = Board.empty(board.length)

    for (row <- Range.Exclusive(0, board.length, 1)) {
      for (col <- Range.Exclusive(0, board.length, 1)) {
        newBoard(row)(col) = board.willBeAlive(row, col)
      }
    }

    newBoard
  }

  private def willBeAlive(row: Int, col: Int) = {
    var neighbourCount = 0

    for (i <- (row - 1 to row + 1) if i >= 0 && i < board.length) {
      for (j <- (col - 1 to col + 1) if j >= 0 && j < board.length) {
        neighbourCount += (if board(i)(j) && !(i == row && j == col) then 1
                           else 0)
      }
    }

    neighbourCount == 3 || (board(row)(col) && neighbourCount == 2)
  }
}

@main
def run(boardSize: Int, seedCount: Int, isInteractive: Boolean = true) = {
  var board = Board.empty(boardSize)
  board.placeRandomSeeds(seedCount)

  val maxRound = 100000

  if (isInteractive) {
    for (round <- 1 to maxRound) {
      println(s"Round $round:")
      board.print()
      println()

      board = board.evolve()
      Thread.sleep(100)
    }
  } else {
    time {
      for (_ <- 1 to maxRound) {
        board = board.evolve()
      }
    }
  }
}

def time[R](block: => R): R = {
  val t0 = System.nanoTime()
  val result = block // call-by-name
  val t1 = System.nanoTime()
  println("Elapsed time: " + (t1 - t0) + "ns")
  result
}
