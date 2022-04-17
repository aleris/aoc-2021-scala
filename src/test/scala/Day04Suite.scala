import Day04.Cell
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day04Suite extends AnyFunSpec {
  describe("Day 04 test") {
    it("reads game for test data") {
      val game = Day04.read(Source.fromResource("day_04_test.txt"))
      game.numbers.head should be (7)
      game.numbers.last should be (1)
      game.boards.length should be (3)
      val lastBoard = game.boards(2)
      lastBoard.numbers(0)(0) should be (Cell(14))
      lastBoard.numbers(0)(4) should be (Cell(4))
      lastBoard.numbers(4)(0) should be (Cell(2))
      lastBoard.numbers(4)(4) should be (Cell(7))
    }

    it("gets correct score for the winning board") {
      val game = Day04.read(Source.fromResource("day_04_test.txt"))
      val score = game.playForWin()
      score should be (4512)
    }

    it("gets correct score for the losing board") {
      val game = Day04.read(Source.fromResource("day_04_test.txt"))
      val score = game.playForLose()
      score should be (1924)
    }

    it("reads game") {
      val game = Day04.read(Source.fromResource("day_04.txt"))
      game.numbers.head should be (91)
      game.numbers.last should be (16)
      game.boards.length should be (100)
      val lastBoard = game.boards(99)
      lastBoard.numbers(0)(0) should be (Cell(22))
      lastBoard.numbers(0)(4) should be (Cell(63))
      lastBoard.numbers(4)(0) should be (Cell(68))
      lastBoard.numbers(4)(4) should be (Cell(26))
    }

  }

  describe("Day 04") {
    it("a. plays for win") {
      val game = Day04.read(Source.fromResource("day_04.txt"))
      val score = game.playForWin()
      println(score)
    }

    it("b. plays for lose") {
      val game = Day04.read(Source.fromResource("day_04.txt"))
      val score = game.playForLose()
      println(score)
    }
  }
}
