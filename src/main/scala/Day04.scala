import scala.io.Source

object Day04 {
  case class Cell(number: Int, marked: Boolean = false) {
    def mark(): Cell = Cell(number, marked = true)
  }

  case class Board(numbers: List[List[Cell]]) {
    def mark(n: Int): Board = Board(
      numbers.map(line => line.map(cell =>
        if (cell.number == n) cell.mark() else cell)
      )
    )
    def won(): Boolean = {
      numbers.exists(line => line.forall(_.marked)) ||
        numbers.head.indices.exists(i => col(i).forall(_.marked))
    }
    def col(i: Int): List[Cell] = numbers.map(line => line(i))

    def unmarkedScore(): Int = numbers.flatMap(line => line.map {
      case Cell(number, false) => number
      case Cell(_, true) => 0
    }).sum
  }

  case class Game(numbers: Seq[Int], boards: List[Board]) {
    def playForWin(): Int = {
      var playBoards = boards
      for (n <- numbers) {
        playBoards = playBoards.map(_.mark(n))
        val wonOpt = playBoards.find(board => board.won())
        if (wonOpt.isDefined) {
          val winningBoard = wonOpt.get
          val score = n * winningBoard.unmarkedScore()
          return score
        }
      }
      0
    }

    def playForLose(): Int = {
      var playBoards = boards
      var lastBoard: Option[Board] = None
      for (n <- numbers) {
        playBoards = playBoards.map(_.mark(n))
        val leftNotWon = playBoards.count(board => !board.won())
        if (leftNotWon == 1) {
          lastBoard = playBoards.find(!_.won())
        }
        if (leftNotWon == 0) {
          val score = n * lastBoard.get.mark(n).unmarkedScore()
          return score
        }
      }
      0
    }
  }

  def read(source: Source): Game = {
    val lines = source.getLines().toList
    val numberSeq = lines.head.split(',') map { _.toInt }
    val boardsLines = lines.splitAt(2)._2
    val boardCount = (boardsLines.length + 1) / 6
    Game(numberSeq, (0 until boardCount).map(i => readBoard(boardsLines, i)).toList)
  }

  def readBoard(lines: List[String], boardIndex: Int): Board = {
    val boardLines = lines.slice(6 * boardIndex, 6 * boardIndex + 5)
    Board(boardLines.map(line => line.trim.split("\\s+").map(n => Cell(n.toInt)).toList))
  }
}
