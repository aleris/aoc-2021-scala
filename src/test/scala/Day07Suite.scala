import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day07Suite extends AnyFunSpec {
  describe("Day 07 test") {
    it("reads positions") {
      val positions = Day07.readPositions(Source.fromResource("day_07_test.txt"))
      positions.size should be (10)
    }

    it("finds optimal fuel cost position") {
      val positions = Day07.readPositions(Source.fromResource("day_07_test.txt"))
      val fuelCostPosition = Day07.findOptimalCost(positions, Day07.linearFuelConsumption)
      fuelCostPosition.cost should be (37)
      fuelCostPosition.position should be (2)
    }

    it("finds optimal fuel cost position with incremental cost") {
      val positions = Day07.readPositions(Source.fromResource("day_07_test.txt"))
      val fuelCostPosition = Day07.findOptimalCost(positions, Day07.incrementalFuelConsumption)
      fuelCostPosition.cost should be (168)
      fuelCostPosition.position should be (5)
    }
  }

  describe("Day 07") {
    it("a. finds optimal fuel cost position") {
      val positions = Day07.readPositions(Source.fromResource("day_07.txt"))
      val fuelCostPosition = Day07.findOptimalCost(positions, Day07.linearFuelConsumption)
      println(fuelCostPosition.cost)
    }

    it("b. finds optimal fuel cost position with incremental cost") {
      val positions = Day07.readPositions(Source.fromResource("day_07.txt"))
      val fuelCostPosition = Day07.findOptimalCost(positions, Day07.incrementalFuelConsumption)
      println(fuelCostPosition.cost)
    }
  }
}
