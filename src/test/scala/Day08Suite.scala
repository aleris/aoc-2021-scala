import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day08Suite extends AnyFunSpec {
  describe("Day 08 test") {
    it("reads all entries") {
      val entries = Day08.readAllEntries(Source.fromResource("day_08_test.txt"))
      entries.size should be (10)
      entries.head.uniqueSignalPatterns.length should be(10)
      entries.head.uniqueSignalPatterns.last should be (Set('e', 'd', 'b'))
      entries.head.outputValue.length should be(4)
      entries.head.outputValue.last should be (Set('g', 'c', 'e', 'b'))
    }

    it("counts with unique number of segments") {
      val entries = Day08.readAllEntries(Source.fromResource("day_08_test.txt"))
      Day08.countWithUniqueNumberOfSegments(entries) should be (26)
    }

    it("maps an entry") {
      val entries = Day08.readAllEntries(
        Source.fromString("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
      )
      val mapping = Day08.mapEntry(entries(0))
      mapping.size should be (10)
      mapping(Day08.segmentsOfDigitString("acedgfb")) should be(Day08.digitToSegments(8))
      mapping(Day08.segmentsOfDigitString("cdfbe")) should be(Day08.digitToSegments(5))
      mapping(Day08.segmentsOfDigitString("gcdfa")) should be(Day08.digitToSegments(2))
      mapping(Day08.segmentsOfDigitString("fbcad")) should be(Day08.digitToSegments(3))
      mapping(Day08.segmentsOfDigitString("dab")) should be(Day08.digitToSegments(7))
      mapping(Day08.segmentsOfDigitString("cefabd")) should be(Day08.digitToSegments(9))
      mapping(Day08.segmentsOfDigitString("cdfgeb")) should be(Day08.digitToSegments(6))
      mapping(Day08.segmentsOfDigitString("eafb")) should be(Day08.digitToSegments(4))
      mapping(Day08.segmentsOfDigitString("cagedb")) should be(Day08.digitToSegments(0))
      mapping(Day08.segmentsOfDigitString("ab")) should be(Day08.digitToSegments(1))
    }

    it("decodes an entry output") {
      val entries = Day08.readAllEntries(
        Source.fromString("acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf")
      )
      Day08.decodeOutput(entries(0)) should be ("5353")
    }

    it("sums all decoded entries") {
      val entries = Day08.readAllEntries(Source.fromResource("day_08_test.txt"))
      Day08.sumOfDecoded(entries) should be (61229)
    }
  }

  describe("Day 08") {
    it("a. counts with unique number of segments") {
      val entries = Day08.readAllEntries(Source.fromResource("day_08.txt"))
      println(Day08.countWithUniqueNumberOfSegments(entries))
    }

    it("b. sums all decoded entries") {
      val entries = Day08.readAllEntries(Source.fromResource("day_08.txt"))
      println(Day08.sumOfDecoded(entries))
    }
  }
}
