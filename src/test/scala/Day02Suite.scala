import org.scalatest.funspec.AnyFunSpec

import scala.io.Source

class Day02Suite extends AnyFunSpec {
  describe("Day 02") {
    val moves = Day02.readMoves(Source.fromResource("day_02.txt"))
    it("a. should follow moves") {
      println(Day02.followMoves(moves))
    }

    it("b. should follow moves with aim") {
      println(Day02.followMovesWithAim(moves))
    }
  }
}
