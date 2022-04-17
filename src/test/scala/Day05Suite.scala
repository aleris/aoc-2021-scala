import Day05.Grid
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day05Suite extends AnyFunSpec {
  describe("Day 05 test") {
    it("reads lines") {
      val lines = Day05.readAsLines(Source.fromResource("day_05_test.txt"))
      lines.length should be (10)
      lines(9).to.y should be (2)
    }

    it("creates array") {
      val lines = Day05.readAsLines(Source.fromResource("day_05_test.txt"))
      val grid = Grid(lines)
      grid.rows should be (10)
      grid.columns should be (10)
    }

    it("overlaps line correctly") {
      val lines = Day05.readAsLines(Source.fromResource("day_05_test.txt"))
      val grid = Grid(lines)
      grid.overlap(lines(0))
      grid.at(0, 9) should be (1)
      grid.at(5, 9) should be (1)
      grid.at(6, 9) should be (0)
      grid.at(0, 8) should be (0)
    }

    it("overlaps  a line vertical") {
      val lines = Day05.readAsLines(Source.fromResource("day_05_test.txt"))
      val grid = Grid(lines)
      grid.overlap(lines(3))
      grid.at(2, 1) should be (1)
    }

    it("overlaps a line over another") {
      val lines = Day05.readAsLines(Source.fromResource("day_05_test.txt"))
      val grid = Grid(lines)
      grid.overlap(lines(0))
      grid.overlap(lines(6))
      grid.at(0, 9) should be (2)
      grid.at(1,9) should be (2)
      grid.at(2,9) should be (2)
      grid.at(3,9) should be (1)
      grid.at(5,9) should be (1)
      grid.at(6,9) should be (0)
      grid.at(0,8) should be (0)
    }

    it("counts the number of points where at least two lines overlap") {
      val lines = Day05.readAsLines(Source.fromResource("day_05_test.txt"))
      val grid = Grid(lines)
      grid.overlapAllHorizontalOrVertical(lines)
      grid.countHigherThan(2) should be (5)
    }

    it("counts the number of points where at least two lines overlap with diagonals") {
      val lines = Day05.readAsLines(Source.fromResource("day_05_test.txt"))
      val grid = Grid(lines)
      grid.overlapAll(lines)
      grid.countHigherThan(2) should be (12)
    }
  }

  describe("Day 05") {
    it("a. counts the number of points where at least two lines overlap") {
      val lines = Day05.readAsLines(Source.fromResource("day_05.txt"))
      val grid = Grid(lines)
      grid.overlapAllHorizontalOrVertical(lines)
      println(grid.countHigherThan(2))
    }

    it("b. counts the number of points where at least two lines overlap with diagonals") {
      val lines = Day05.readAsLines(Source.fromResource("day_05.txt"))
      val grid = Grid(lines)
      grid.overlapAll(lines)
      println(grid.countHigherThan(2))
    }
  }
}
