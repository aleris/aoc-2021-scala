import scala.io.Source

object Day05 {
  case class Point(x: Int, y: Int)
  case class Line(from: Point, to: Point) {
    def isVerticalOrHorizontal: Boolean = from.x == to.x || from.y == to.y
  }

  object Line {
    def apply(line: String): Line = {
      val pattern = "(\\d+),(\\d+) -> (\\d+),(\\d+)".r
      val pattern(fromX, fromY, toX, toY) = line
      Line(Point(fromX.toInt, fromY.toInt), Point(toX.toInt, toY.toInt))
    }
  }

  case class Grid(array: Array[Array[Int]]) {
    def rows: Int = array.length
    def columns: Int = array.head.length

    def overlap(line: Line): Unit = {
      val fromX = line.from.x
      val fromY = line.from.y
      val toX = line.to.x
      val toY = line.to.y
      val sx = math.signum(toX - fromX)
      val sy = math.signum(toY - fromY)
      if (sx == 0) {
        val ry = fromY to toY by sy
        for (y <- ry) {
          array(y)(fromX) += 1
        }
      } else if (sy == 0) {
        val rx = fromX to toX by sx
        for (x <- rx) {
          array(fromY)(x) += 1
        }
      } else {
        val rx = fromX to toX by sx
        val ry = fromY to toY by sy
        for ((x, y) <- rx.zip(ry)) {
          array(y)(x) += 1
        }
      }
    }

    def at(x: Int, y: Int): Int = array(y)(x)

    def overlapAllHorizontalOrVertical(lines: List[Line]): Unit =
      lines.filter {_.isVerticalOrHorizontal}.foreach {line => overlap(line)}

    def overlapAll(lines: List[Line]): Unit =
      lines.foreach(line => overlap(line))

    def countHigherThan(threshold: Int): Int =
      array.map {row => row.count {threshold <= _}}.sum

    override def toString: String =
      array.map(row =>
        row.map(v =>
          (if (v == 0) "." else v.toString).padTo(2, ' ')
        ).mkString("")
      ).mkString("\n") + "\n"
  }

  object Grid {
    def apply(lines: List[Line]): Grid = {
      val maxX = math.max(lines.map {_.from.x + 1}.max, lines.map {_.to.x + 1}.max)
      val maxY = math.max(lines.map {_.from.y + 1}.max, lines.map {_.to.y + 1}.max)
      Grid(Array.tabulate(maxY, maxX) { (_, _) => 0 })
    }
  }

  def readAsLines(source: Source): List[Line] = source.getLines().map(Line(_)).toList
}
