import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day03Suite extends AnyFunSpec {
  describe("Day 03 test") {
    val lines = Day03.read(Source.fromResource("day_03_test.txt"))
    it("read power consumption") {
      Day03.readPowerConsumption(lines) should be (198)
    }

    it("read life support rating") {
      Day03.readLifeSupportRating(lines) should be (230)
    }
  }

  describe("Day 03") {
    val lines = Day03.read(Source.fromResource("day_03.txt"))
    it("a. read power consumption") {
      println(Day03.readPowerConsumption(lines))
    }

    it("b. read life support rating") {
      println(Day03.readLifeSupportRating(lines))
    }
  }
}
