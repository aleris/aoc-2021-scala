import scala.io.Source

object Day11 {
  case class Coordinate(x: Int, y: Int) {
    def neighbours: Seq[Coordinate] =
      Seq((0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0), (-1, -1))
        .map {case (dx, dy) => Coordinate(x + dx, y + dy)}

    def isInside(grid: Grid): Boolean = if (y < 0) false
      else if (grid.height - 1 < y) false
      else if (x < 0) false
      else if (grid.width - 1 < x) false
      else true
  }

  case class Grid(private val data: Array[Array[Int]]) {
    def width: Int = data.head.length
    def height: Int = data.length

    def apply(coordinate: Coordinate): Int = data(coordinate.y)(coordinate.x)

    def update(coordinate: Coordinate, value: Int): Unit = data(coordinate.y)(coordinate.x) = value

    def incrementAll(): Unit = allCoordinates.foreach { c => this(c) += 1}

    def neighbors(coordinate: Coordinate): Seq[Coordinate] =
      coordinate.neighbours.filter(_.isInside(this))

    override def toString: String =
      data.map(_.mkString("")).mkString("\n") + "\n"

    def toStringPadded: String =
      data.map { row =>
        row.map(c => "  " + c takeRight 3).mkString("")
      }.mkString("\n") + "\n"

    def allCoordinates: Seq[Coordinate] =
      for (x <- 0 until width; y <- 0 until height)
        yield Coordinate(x, y)
  }

  object Grid {
    def apply(source: Source): Grid = Grid(
      source.getLines()
        .map { line =>
          line.toCharArray.map(_.toString.toInt)
        }
        .toArray
    )
  }

  class Simulator(grid: Grid) {
    private var stepIndex: Int = -1
    private var flashCount: Int = 0
    private var allFlashedFlag: Boolean = false

    def incrementStep(): Unit = stepIndex += 1
    def incrementFlashCount(): Unit = flashCount += 1
    def markAsAllFlashed(): Unit = allFlashedFlag = true

    def simulate(steps: Int): Unit = {
      for (_ <- 0 until steps) {
        simulateStep()
      }
    }

    def numberOfFlashes: Int = flashCount

    def simulateUntilAllFlashed(): Unit = {
      do {
        simulateStep()
      } while (!allFlashedFlag)
    }

    def currentStep: Int = stepIndex + 1

    def simulateStep(): Unit = {
      incrementStep()
      grid.incrementAll()
      flash()
      if (areAllFlashed) markAsAllFlashed()
    }

    def flash(): Unit =
      for (coordinate <- grid.allCoordinates) {
        if (isFlashable(coordinate)) {
          flashRecursive(coordinate)
        }
      }

    def flashRecursive(coordinate: Coordinate): Unit = {
      incrementFlashCount()
      grid(coordinate) = 0
      grid.neighbors(coordinate).foreach { n =>
        if (isNotFlashed(n)) grid(n) += 1
        if (isFlashable(n)) flashRecursive(n)
      }
    }

    private def isNotFlashed(coordinate: Coordinate): Boolean = grid(coordinate) != 0

    private def isFlashable(coordinate: Coordinate): Boolean = 9 < grid(coordinate)

    private def areAllFlashed: Boolean = grid.allCoordinates.forall(grid(_) == 0)
  }
}
