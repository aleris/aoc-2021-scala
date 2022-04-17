import Day12.{CaveGraph, Navigator, SingleSmallTwiceVisitStrategy, SmallOnceVisitStrategy}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day12Suite extends AnyFunSpec {
  describe("Day 12 test") {
    it("finds all paths in smaller example") {
      val caveGraph = CaveGraph(Source.fromResource("day_12_test.txt"))
      val navigator = new Navigator(caveGraph, new SmallOnceVisitStrategy())
      val paths = navigator.generateAllPaths()
      paths.size should be (10)
      paths(3).caves.map(_.id).mkString(",") should be ("start,A,b,A,c,A,end")
    }

    it("finds all paths in larger example") {
      val caveGraph = CaveGraph(Source.fromResource("day_12_test_larger.txt"))
      val navigator = new Navigator(caveGraph, new SmallOnceVisitStrategy())
      val paths = navigator.generateAllPaths()
      paths.size should be (19)
    }

    it("finds all paths in even larger example") {
      val caveGraph = CaveGraph(Source.fromResource("day_12_test_even_larger.txt"))
      val navigator = new Navigator(caveGraph, new SmallOnceVisitStrategy())
      val paths = navigator.generateAllPaths()
      paths.size should be (226)
    }

    it("finds all paths when visiting a single small cave is allowed") {
      val caveGraph = CaveGraph(Source.fromResource("day_12_test.txt"))
      val navigator = new Navigator(caveGraph, new SingleSmallTwiceVisitStrategy())
      val paths = navigator.generateAllPaths()
//      println(paths.mkString("\n"))
      paths.size should be (36)
    }

    it("finds all paths when visiting a single small cave is allowed in larger example") {
      val caveGraph = CaveGraph(Source.fromResource("day_12_test_larger.txt"))
      val navigator = new Navigator(caveGraph, new SingleSmallTwiceVisitStrategy())
      val paths = navigator.generateAllPaths()
      paths.size should be (103)
    }

    it("finds all paths when visiting a single small cave is allowed in even larger example") {
      val caveGraph = CaveGraph(Source.fromResource("day_12_test_even_larger.txt"))
      val navigator = new Navigator(caveGraph, new SingleSmallTwiceVisitStrategy())
      val paths = navigator.generateAllPaths()
      paths.size should be (3509)
    }

  }

  describe("Day 12") {
    it("a. finds all paths in cave") {
      val caveGraph = CaveGraph(Source.fromResource("day_12.txt"))
      val navigator = new Navigator(caveGraph, new SmallOnceVisitStrategy())
      val paths = navigator.generateAllPaths()
      println(paths.size)
    }

    it("b. finds all paths in cave when visiting a single small cave is allowed") {
      val caveGraph = CaveGraph(Source.fromResource("day_12.txt"))
      val navigator = new Navigator(caveGraph, new SingleSmallTwiceVisitStrategy())
      val paths = navigator.generateAllPaths()
      println(paths.size)
    }
  }
}
