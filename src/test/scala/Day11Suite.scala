import Day11.{Coordinate, Grid, Simulator}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

import scala.io.Source

class Day11Suite extends AnyFunSpec {
  describe("Day 11 test") {
    it("creates a grid") {
      val grid = Grid(Source.fromResource("day_11_test_larger.txt"))
      grid(Coordinate(0, 0)) should be (5)
      grid(Coordinate(1, 1)) should be (7)
      grid(Coordinate(9, 9)) should be (6)
      grid(Coordinate(9, 0)) should be (3)
      grid(Coordinate(0, 9)) should be (5)
    }

    it("gets correct neighbours") {
      val grid = Grid(Source.fromResource("day_11_test_smaller.txt"))
      grid.neighbors(Coordinate(0, 0)) should be (Seq(Coordinate(1, 0), Coordinate(1, 1), Coordinate(0, 1)))
      grid.neighbors(Coordinate(4, 2)) should be (Seq(Coordinate(4, 1), Coordinate(4, 3), Coordinate(3, 3), Coordinate(3, 2), Coordinate(3, 1)))
      grid.neighbors(Coordinate(4, 4)) should be (Seq(Coordinate(4, 3), Coordinate(3, 4), Coordinate(3, 3)))
    }

    it("flashes recursive correctly") {
      val grid = Grid(Source.fromResource("day_11_test_smaller.txt"))
      val simulator = new Simulator(grid)
      simulator.flashRecursive(Coordinate(1, 1))
      grid.toString should be (
        """23432
          |30003
          |40904
          |30003
          |23432
          |""".stripMargin
      )
    }

    it("simulates steps correctly for smaller example") {
      val grid = Grid(Source.fromResource("day_11_test_smaller.txt"))
      val simulator = new Simulator(grid)

      simulator.simulateStep()
      grid.toString should be (
        """34543
          |40004
          |50005
          |40004
          |34543
          |""".stripMargin
      )

      simulator.simulateStep()
      grid.toString should be (
        """45654
          |51115
          |61116
          |51115
          |45654
          |""".stripMargin
      )
    }

    it("simulates steps correctly for larger example") {
      val grid = Grid(Source.fromResource("day_11_test_larger.txt"))
      val simulator = new Simulator(grid)

      simulator.simulateStep()
      grid.toString should be (
        """6594254334
          |3856965822
          |6375667284
          |7252447257
          |7468496589
          |5278635756
          |3287952832
          |7993992245
          |5957959665
          |6394862637
          |""".stripMargin
      )

      simulator.simulateStep()
      grid.toString should be (
        """8807476555
          |5089087054
          |8597889608
          |8485769600
          |8700908800
          |6600088989
          |6800005943
          |0000007456
          |9000000876
          |8700006848
          |""".stripMargin
      )

      simulator.simulateStep()
      grid.toString should be (
        """0050900866
          |8500800575
          |9900000039
          |9700000041
          |9935080063
          |7712300000
          |7911250009
          |2211130000
          |0421125000
          |0021119000
          |""".stripMargin
      )

      simulator.simulateStep()
      grid.toString should be (
        """2263031977
          |0923031697
          |0032221150
          |0041111163
          |0076191174
          |0053411122
          |0042361120
          |5532241122
          |1532247211
          |1132230211
          |""".stripMargin
      )
    }

    it("simulates 20 steps correctly") {
      val grid = Grid(Source.fromResource("day_11_test_larger.txt"))
      val simulator = new Simulator(grid)
      simulator.simulate(20)

      grid.toString should be (
        """3936556452
          |5686556806
          |4496555690
          |4448655580
          |4456865570
          |5680086577
          |7000009896
          |0000000344
          |6000000364
          |4600009543
          |""".stripMargin
      )
    }

    it("simulates 100 steps correctly") {
      val grid = Grid(Source.fromResource("day_11_test_larger.txt"))
      val simulator = new Simulator(grid)
      simulator.simulate(100)

      grid.toString should be (
        """0397666866
          |0749766918
          |0053976933
          |0004297822
          |0004229892
          |0053222877
          |0532222966
          |9322228966
          |7922286866
          |6789998766
          |""".stripMargin
      )
    }

    it("tracks flashes correctly for 10 steps") {
      val grid = Grid(Source.fromResource("day_11_test_larger.txt"))
      val simulator = new Simulator(grid)
      simulator.simulate(10)
      simulator.numberOfFlashes should be (204)
    }

    it("tracks flashes correctly for 100 steps") {
      val grid = Grid(Source.fromResource("day_11_test_larger.txt"))
      val simulator = new Simulator(grid)
      simulator.simulate(100)
      simulator.numberOfFlashes should be (1656)
      simulator.currentStep should be (100)
    }

    it("determines first step where all flashed") {
      val grid = Grid(Source.fromResource("day_11_test_larger.txt"))
      val simulator = new Simulator(grid)
      simulator.simulateUntilAllFlashed()
      simulator.currentStep should be (195)
    }
  }

  describe("Day 11") {
    it("a. determines number of flashes after 100 steps") {
      val grid = Grid(Source.fromResource("day_11.txt"))
      val simulator = new Simulator(grid)
      simulator.simulate(100)
      println(simulator.numberOfFlashes)
    }

    it("b. determines first step where all flashed") {
      val grid = Grid(Source.fromResource("day_11.txt"))
      val simulator = new Simulator(grid)
      simulator.simulateUntilAllFlashed()
      println(simulator.currentStep)
    }
  }
}
