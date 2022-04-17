import scala.collection.mutable
import scala.io.Source

object Day09 {
  case class Point(row: Int, col: Int) {
    def up: Point = Point(row - 1, col)
    def down: Point = Point(row + 1, col)
    def left: Point = Point(row, col - 1)
    def right: Point = Point(row, col + 1)
  }

  class HeightMap(array: Array[Array[Int]]) {
    def at(p: Point): Int = array(p.row)(p.col)

    def isLowLevel(p: Point): Boolean = {
      import p.{row, col}
      val v = at(p)
      if (0 < row && at(p.up) <= v) return false
      if (row < array.length - 1 && at(p.down) <= v) return false
      if (0 < col && at(p.left) <= v) return false
      if (col < array.head.length - 1 && at(p.right) <= v) return false
      true
    }

    def riskLevel(p: Point): Int = at(p) + 1

    def allLowLevels: Seq[Point] =
      for (row <- array.indices; col <- array.head.indices; if isLowLevel(Point(row, col)))
        yield Point(row, col)

    def sumOfRiskLevelsAtLowLevels: Int = allLowLevels.map(riskLevel).sum

    def fillBasinSize(startPoint: Point): Int = {
      val toVisit = new mutable.Stack[Point]
      var visited = Set[Point]()
      toVisit.push(startPoint)

      def addToVisitIfVisitable(p: Point): Unit = {
        if (visited.contains(p)) return
        if (at(p) == 9) return
        toVisit.push(p)
      }

      while (toVisit.nonEmpty) {
        val p = toVisit.pop()
        import p.{row, col}
        if (0 < row) addToVisitIfVisitable(p.up)
        if (row < array.length - 1) addToVisitIfVisitable(p.down)
        if (0 < col) addToVisitIfVisitable(p.left)
        if (col < array.head.length - 1) addToVisitIfVisitable(p.right)
        visited += p
      }
      visited.size
    }

    def top3BasinSizes: Seq[Int] = allLowLevels.map(fillBasinSize).sorted.reverse.take(3)
  }

  object HeightMap {
    def apply(source: Source): HeightMap =
      new HeightMap(source.getLines().map(line => line.toCharArray.map(_.toString.toInt)).toArray)
  }
}
