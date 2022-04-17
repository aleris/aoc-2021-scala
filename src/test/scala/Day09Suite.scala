import Day09.{HeightMap, Point}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day09Suite extends AnyFunSpec {
  describe("Day 09 test") {
    it("reads height map") {
      val heightMap = HeightMap(Source.fromResource("day_09_test.txt"))
      heightMap.at(Point(0, 0)) should be (2)
      heightMap.at(Point(0, 9)) should be (0)
      heightMap.at(Point(4, 0)) should be (9)
      heightMap.at(Point(4, 9)) should be (8)
    }

    it("determines if is low level") {
      val heightMap = HeightMap(Source.fromResource("day_09_test.txt"))
      heightMap.isLowLevel(Point(0, 0)) should be (false)
      heightMap.isLowLevel(Point(0, 1)) should be (true)
      heightMap.isLowLevel(Point(0, 2)) should be (false)
      heightMap.isLowLevel(Point(0, 3)) should be (false)
      heightMap.isLowLevel(Point(0, 9)) should be (true)
      heightMap.isLowLevel(Point(1, 0)) should be (false)
      heightMap.isLowLevel(Point(1, 9)) should be (false)
      heightMap.isLowLevel(Point(2, 2)) should be (true)
      heightMap.isLowLevel(Point(4, 0)) should be (false)
      heightMap.isLowLevel(Point(4, 2)) should be (false)
      heightMap.isLowLevel(Point(4, 6)) should be (true)
      heightMap.isLowLevel(Point(4, 9)) should be (false)
    }

    it("determines all low levels") {
      val heightMap = HeightMap(Source.fromResource("day_09_test.txt"))
      heightMap.allLowLevels.size should be (4)
    }

    it("sums the risk levels at all low levels") {
      val heightMap = HeightMap(Source.fromResource("day_09_test.txt"))
      heightMap.sumOfRiskLevelsAtLowLevels should be (15)
    }

    it("gets the correct basin size") {
      val heightMap = HeightMap(Source.fromResource("day_09_test.txt"))
      heightMap.fillBasinSize(Point(0, 1)) should be (3)
      heightMap.fillBasinSize(Point(0, 9)) should be (9)
      heightMap.fillBasinSize(Point(2, 2)) should be (14)
      heightMap.fillBasinSize(Point(4, 6)) should be (9)
    }

    it("gets the top 3 basin sizes for all map") {
      val heightMap = HeightMap(Source.fromResource("day_09_test.txt"))
      heightMap.top3BasinSizes should be (Seq(14, 9, 9))
    }
  }

  describe("Day 09") {
    it("a. sums the risk levels at all low levels") {
      val heightMap = HeightMap(Source.fromResource("day_09.txt"))
      println(heightMap.sumOfRiskLevelsAtLowLevels)
    }

    it("b. multiplies top 3 basin sizes") {
      val heightMap = HeightMap(Source.fromResource("day_09.txt"))
      val top3BasinSizes = heightMap.top3BasinSizes
      println(top3BasinSizes(0) * top3BasinSizes(1) * top3BasinSizes(2))
    }
  }
}
