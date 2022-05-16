import Day17.{ProbeLauncher, TargetArea, Vector}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper


class Day17Suite extends AnyFunSpec {
  describe("Day 17 test") {
    val probeLauncher = new ProbeLauncher()
    val targetArea = TargetArea("target area: x=20..30, y=-10..-5")

    it("computes successful trajectory") {
      probeLauncher.testTrajectory(Vector(7, 2), targetArea).successful should be (true)
      probeLauncher.testTrajectory(Vector(6, 3), targetArea).successful should be (true)
      probeLauncher.testTrajectory(Vector(9, 0), targetArea).successful should be (true)
      probeLauncher.testTrajectory(Vector(17, -4), targetArea).successful should be (false)
    }

    it("computes highest Y") {
      probeLauncher.testTrajectory(Vector(6, 9), targetArea).highestY should be (45)
    }

    it("determines velocity for aim with style") {
      probeLauncher.aimWithStyle(targetArea).velocity should be (Vector(6, 9))
      probeLauncher.aimWithStyle(targetArea).highestY should be (45)
    }

    it("determines all initial velocities for hitting target") {
      probeLauncher.findAllAims(targetArea) should be (112)
    }
  }

  describe("Day 17") {
    val probeLauncher = new ProbeLauncher()
    val targetArea = TargetArea("target area: x=288..330, y=-96..-50")
    it("a. find velocity for aim with style") {
      println(probeLauncher.aimWithStyle(targetArea))
    }

    it("b. find all aims") {
      println(probeLauncher.findAllAims(targetArea))
    }

  }
}
