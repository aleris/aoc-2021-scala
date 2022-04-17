import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day06Suite extends AnyFunSpec {
  describe("Day 06 test") {
    it("read") {
      val school = Day06.readInitial(Source.fromResource("day_06_test.txt"))
      school.map.size should be (4)
      school.map(3) should be (2)
      school.map(2) should be (1)
    }

    it("simulates one day") {
      val school = Day06.readInitial(Source.fromResource("day_06_test.txt"))
      school.simulate(1)
      school.map.size should be (4)
      school.map(2) should be (2)
      school.map(1) should be (1)
    }

    it("simulates two days") {
      val school = Day06.readInitial(Source.fromResource("day_06_test.txt"))
      school.simulate(2)
      school.map.size should be (5)
      school.map(1) should be (2)
      school.map(0) should be (1)
      school.map(8) should be (1)
    }

    it("simulates three days") {
      val school = Day06.readInitial(Source.fromResource("day_06_test.txt"))
      school.simulate(3)
      school.map.size should be (6)
      school.map(0) should be (2)
      school.map(7) should be (1)
      school.map(8) should be (1)
    }

    it("simulates 18 days") {
      val school = Day06.readInitial(Source.fromResource("day_06_test.txt"))
      school.simulate(18)
      school.count should be (26)
    }

    it("simulates 80 days") {
      val school = Day06.readInitial(Source.fromResource("day_06_test.txt"))
      school.simulate(80)
      school.count should be (5934)
    }

    it("simulates 256 days") {
      val school = Day06.readInitial(Source.fromResource("day_06_test.txt"))
      school.simulate(256)
      school.count should be (26984457539L)
    }
  }

  describe("Day 06") {
    it("a. simulates 80 days") {
      val school = Day06.readInitial(Source.fromResource("day_06.txt"))
      school.simulate(80)
      println(school.count)
    }

    it("b. simulates 256 days") {
      val school = Day06.readInitial(Source.fromResource("day_06.txt"))
      school.simulate(256)
      println(school.count)
    }
  }
}
