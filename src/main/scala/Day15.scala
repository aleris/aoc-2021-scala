import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

object Day15 {
  case class Coordinate(x: Int, y: Int) {
    def neighbours: Seq[Coordinate] =
      Seq((0, -1), (1, 0), (0, 1), (-1, 0))
        .map {case (dx, dy) => Coordinate(x + dx, y + dy)}

    def isInside(grid: Grid): Boolean = if (y < 0) false
      else if (grid.height - 1 < y) false
      else if (x < 0) false
      else if (grid.width - 1 < x) false
      else true

    def distance(to: Coordinate): Int = math.abs(to.x - x) + math.abs(to.y - y)

    override def toString: String = s"$x,$y"
  }

  case class Grid(private val data: Array[Array[Int]]) {
    def width: Int = data.head.length
    def height: Int = data.length

    def apply(coordinate: Coordinate): Int = data(coordinate.y)(coordinate.x)
    def apply(x: Int, y: Int): Int = data(y)(x)

    def update(coordinate: Coordinate, value: Int): Unit = data(coordinate.y)(coordinate.x) = value
    def update(x: Int, y: Int, value: Int): Unit = data(y)(x) = value

    def neighbours(coordinate: Coordinate): Seq[Coordinate] =
      coordinate.neighbours.filter(_.isInside(this))

    def pathRisk(path: List[Coordinate]): Int = path.tail.map(this(_)).sum

    def explode(tiles: Int): Grid = {
      val big = Grid(Array.fill(height * tiles, width * tiles) { 0 })

      def wrappedIncreaseRisk(risk: Int): Int = if (risk < 9) risk + 1 else 1

      for (x <- 0 until width) {
        for (y <- 0 until height) {
          big(x, y) = this(x, y)
        }
      }

      for (tile <- 1 until tiles) {
        for (x <- 0 until width) {
          for (y <- 0 until height) {
            big(tile * width + x, y) = wrappedIncreaseRisk(big((tile - 1) * width + x, y))
          }
        }
      }

      for (tile <- 1 until tiles) {
        for (x <- 0 until big.width) {
          for (y <- 0 until height) {
            big(x, tile * height + y) = wrappedIncreaseRisk(big(x, (tile - 1) * height + y))
          }
        }
      }

      big
    }

    override def toString: String =
      data.map(_.mkString("")).mkString("\n") + "\n"
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

  case class PathFinder(grid: Grid) {
    case class CoordinateWithPriority(coordinate: Coordinate, priority: Int)
    object CoordinateWithPriority {
      val ordering: Ordering[CoordinateWithPriority] =
        Ordering.by((coordinateWithRisk: CoordinateWithPriority) => coordinateWithRisk.priority)
    }

    private val fromMap = mutable.Map[Coordinate, Coordinate]()
    private val cost = mutable.Map[Coordinate, Int]()
    private val frontier = new mutable.PriorityQueue[CoordinateWithPriority]()(CoordinateWithPriority.ordering)

    def search(start: Coordinate, end: Coordinate): List[Coordinate] = {
      frontier.enqueue(CoordinateWithPriority(start, 0))
      fromMap += start -> start
      cost += start -> 0
      while (frontier.nonEmpty) {
        val current = frontier.dequeue().coordinate
        grid.neighbours(current).foreach { to =>
          val newCost = cost.getOrElse(current, 0) + grid(to)
          val existingCost = cost.getOrElse(to, Int.MaxValue)
          if (newCost < existingCost) {
            cost(to) = newCost
            frontier.enqueue(CoordinateWithPriority(to, to.distance(end)))
            fromMap(to) = current
          }
        }
      }
      val path = ArrayBuffer[Coordinate](end)
      var c = end
      while (c != start) {
        val from = fromMap(c)
        path.addOne(from)
        c = from
      }
      path.toList.reverse
    }
  }
}
