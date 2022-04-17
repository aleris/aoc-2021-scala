import Day13.{FoldLeft, FoldUp, Sheet}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day13Suite extends AnyFunSpec {
  describe("Day 13 test") {
    it("reads the sheet") {
      val sheet = Sheet(Source.fromResource("day_13_test.txt"))
      sheet.paper.width should be (11)
      sheet.paper.height should be (15)
      sheet.folds.size should be (2)
      sheet.paper(0, 0) should be (false)
      sheet.paper(6, 10) should be (true)
    }

    it("folds it up") {
      val sheet = Sheet(Source.fromResource("day_13_test.txt"))
      val folded = sheet.paper.fold(FoldUp(7))
      folded.visibleCount should be (17)
    }

    it("folds it up then left") {
      val sheet = Sheet(Source.fromResource("day_13_test.txt"))
      val folded = sheet.paper
        .fold(FoldUp(7))
        .fold(FoldLeft(5))
      folded.toString should be (
        """#####
          |#...#
          |#...#
          |#...#
          |#####
          |.....
          |.....
          |""".stripMargin
      )
    }

    it("reads the big sheet") {
      val sheet = Sheet(Source.fromResource("day_13.txt"))
      sheet.paper.width should be (1311)
      sheet.paper.height should be (895)
      sheet.folds.head.isInstanceOf[FoldLeft] should be (true)
    }
  }

  describe("Day 13") {
    it("a. folds the first one") {
      val sheet = Sheet(Source.fromResource("day_13.txt"))
      println(sheet.paper.fold(sheet.folds.head).visibleCount)
    }

    it("b. folds it completely") {
      val sheet = Sheet(Source.fromResource("day_13.txt"))
      val folded = sheet.folds.foldLeft(sheet.paper)((paper, fold) => paper.fold(fold))
      println(folded)
      // JZGUAPRB
    }
  }
}
