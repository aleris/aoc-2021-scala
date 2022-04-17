import Day14.{PolymerPair, PolymerizationEquipment}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day14Suite extends AnyFunSpec {
  describe("Day 14 test") {
    it("reads the instructions") {
      val instructions = Day14.read(Source.fromResource("day_14_test.txt"))
      instructions.template should be("NNCB")
      instructions.insertions(PolymerPair("CH")) should be('B')
    }

    it("counts elements after 2 steps") {
      val instructions = Day14.read(Source.fromResource("day_14_test.txt"))
      val polymerizationEquipment = new PolymerizationEquipment()
      val statistics = polymerizationEquipment.run(2, instructions)
      statistics.counters(PolymerPair("BB")) should be (2)
    }

    it("counts elements after 10 steps") {
      val instructions = Day14.read(Source.fromResource("day_14_test.txt"))
      val polymerizationEquipment = new PolymerizationEquipment()
      val statistics = polymerizationEquipment.run(10, instructions)
      statistics.counts('B') should be(1749)
      statistics.counts('C') should be(298)
      statistics.counts('H') should be(161)
      Day14.subtractedMostAndLeastCounts(statistics.counts) should be (1588)
    }

    it("counts elements after 40 steps") {
      val instructions = Day14.read(Source.fromResource("day_14_test.txt"))
      val polymerizationEquipment = new PolymerizationEquipment()
      val statistics = polymerizationEquipment.run(40, instructions)
      statistics.counts('B') should be(2192039569602L)
      statistics.counts('H') should be(3849876073L)
      Day14.subtractedMostAndLeastCounts(statistics.counts) should be (2188189693529L)
    }
  }

  describe("Day 14") {
    it("a. determines the difference between most and least element counts") {
      val instructions = Day14.read(Source.fromResource("day_14.txt"))
      val polymerizationEquipment = new PolymerizationEquipment()
      val statistics = polymerizationEquipment.run(10, instructions)
      println(Day14.subtractedMostAndLeastCounts(statistics.counts)) // 2345
    }

    it("b. determines the difference between most and least element counts after 40 steps") {
      val instructions = Day14.read(Source.fromResource("day_14.txt"))
      val polymerizationEquipment = new PolymerizationEquipment()
      val statistics = polymerizationEquipment.run(40, instructions)
      println(Day14.subtractedMostAndLeastCounts(statistics.counts))
    }
  }
}
