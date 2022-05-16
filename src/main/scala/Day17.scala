object Day17 {
  case class TargetArea(xMin: Int, xMax: Int, yMin: Int, yMax: Int) {
    def contains(position: Vector): Boolean =
      xMin <= position.x && position.x <= xMax &&
        yMin <= position.y && position.y <= yMax
  }

  object TargetArea {
    def apply(s: String) = {
      val pattern = "target area: x=(-?\\d+)..(-?\\d+), y=(-?\\d+)..(-?\\d+)".r
      val pattern(xMin, xMax, yMin, yMax) = s
      new TargetArea(xMin.toInt, xMax.toInt, yMin.toInt, yMax.toInt)
    }
  }

  case class Vector(x: Int, y: Int) {
    def +(v: Vector): Vector = {
      Vector(x + v.x, y + v.y)
    }
  }

  case class TrajectoryResult(successful: Boolean, highestY: Int)

  case class AimResult(velocity: Vector, highestY: Int)

  class ProbeLauncher {
    def testTrajectory(initialVelocity: Vector, targetArea: TargetArea): TrajectoryResult = {
      var position = Vector(0, 0)
      var velocity = initialVelocity
      val dirH = if (initialVelocity.x < 0)  -1 else 1
      var highestY = Int.MinValue
      do {
        if (targetArea.contains(position)) {
          return TrajectoryResult(successful = true, highestY)
        }
        position += velocity
        if (highestY < position.y) {
          highestY = position.y
        }
        val accelerationX = if (velocity.x == 0) 0 else -dirH
        val accelerationY = -1
        velocity += Vector(accelerationX, accelerationY)
      } while (!isOvershot(position, targetArea))
      TrajectoryResult(successful = false, highestY)
    }

    def aimWithStyle(targetArea: TargetArea): AimResult = {
      val x = determineMinX(targetArea)
      var highestY = Int.MinValue
      var velocity = Vector(0, 0)
      for (y <- 1 to -targetArea.yMin * -targetArea.yMin) {
        val trajectoryResult = testTrajectory(Vector(x, y), targetArea)
        if (trajectoryResult.successful && highestY < trajectoryResult.highestY) {
          highestY = trajectoryResult.highestY
          velocity = Vector(x, y)
        }
      }
      AimResult(velocity, highestY)
    }

    def findAllAims(targetArea: TargetArea): Int = {
      val minX = determineMinX(targetArea)

      var count = 0
      for (x <- minX to targetArea.xMax; y <- targetArea.yMin to -targetArea.yMin * -targetArea.yMin) {
        val trajectoryResult = testTrajectory(Vector(x, y), targetArea)
        if (trajectoryResult.successful) {
          count += 1
        }
      }
      count
    }

    private def isOvershot(position: Vector, targetArea: TargetArea): Boolean =
      targetArea.xMax < position.x || position.y < targetArea.yMin

    private def determineMinX(targetArea: TargetArea): Int = {
      var minX = 0
      do {
        minX += 1
      } while (!testTrajectory(Vector(minX, -targetArea.yMax), targetArea).successful)
      minX
    }
  }
}
