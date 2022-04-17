import Day15.{Coordinate, Grid, PathFinder}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day15Suite extends AnyFunSpec {
  describe("Day 15 test") {
    it("reads grid") {
      val grid = Grid(Source.fromResource("day_15_test.txt"))
      grid.width should be (10)
      grid.height should be (10)
      val pathFinder = PathFinder(grid)
      val path = pathFinder.search(Coordinate(0, 0), Coordinate(grid.width - 1, grid.height - 1))
      grid.pathRisk(path) should be (40)
    }

    it("explodes tiled") {
      val grid = Grid(Source.fromResource("day_15_test.txt"))
      val big = grid.explode(5)
      big.width should be (50)
      big.height should be (50)
      big(49, 0) should be (6)
      big(0, 49) should be (6)
      big(49, 49) should be (9)
    }
  }

  describe("Day 15") {
    it("a. lowest path risk") {
      val grid = Grid(Source.fromResource("day_15.txt"))
      val pathFinder = PathFinder(grid)
      val path = pathFinder.search(Coordinate(0, 0), Coordinate(grid.width - 1, grid.height - 1))
      println(grid.pathRisk(path))
    }

    it("b. lowest path risk for big grid") {
      val grid = Grid(Source.fromResource("day_15.txt"))
      val big = grid.explode(5)
      val pathFinder = PathFinder(big)
      val path = pathFinder.search(Coordinate(0, 0), Coordinate(big.width - 1, big.height - 1))
      println(big.pathRisk(path))
    }

  }
}
