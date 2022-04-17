import org.scalatest.funspec.AnyFunSpec

import scala.io.Source

class Day01Suite extends AnyFunSpec {
  describe("Day 01") {
    val depths = Day01.readDepths(Source.fromResource("day_01.txt"))

    it("a. should count increases") {
      println(Day01.countIncreases(depths))
    }

    it("b. should count increases with window 3") {
      println(Day01.countIncreasesWindow(depths, 3))
    }
  }
}
