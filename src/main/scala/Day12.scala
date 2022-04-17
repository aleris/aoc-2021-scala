import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

object Day12 {
  case class Connection(from: String, to: String)

  case class Cave(id: String, connections: ListBuffer[Cave] = ListBuffer()) {
    def isBig: Boolean = id.forall(_.isUpper)
    def isSmall: Boolean = id.forall(_.isLower)
    def isStart: Boolean = id == "start"
    def isEnd: Boolean = id == "end"

    def canEqual(other: Any): Boolean = other.isInstanceOf[Cave]

    override def toString = id

    override def equals(other: Any): Boolean = other match {
      case that: Cave => id == that.id
      case _ => false
    }

    override def hashCode(): Int = id.hashCode
  }

  case class CaveGraph(connections: Seq[Connection]) {
    private val cavesMap = mutable.Map[String, Cave]()
    for (c <- connections) {
      val from = cavesMap.getOrElseUpdate(c.from, Cave(c.from))
      val to = cavesMap.getOrElseUpdate(c.to, Cave(c.to))
      from.connections += to
      to.connections += from
    }
    val caves: Map[String, Cave] = cavesMap.toMap
    val start: Cave = caves("start")
    val end: Cave = caves("end")
  }

  object CaveGraph {
    def apply(source: Source): CaveGraph = CaveGraph(
      source.getLines().map(_.split('-')).map(nodes => Connection(nodes(0), nodes(1))).toSeq
    )
  }

  case class CavePath(caves: List[Cave]) {
    def startsWith(path: CavePath): Boolean = {
      if (caves.size < path.caves.size) {
        return false
      }
      path.caves.zipWithIndex.forall {
        case (c, i) => c == caves(i)
      }
    }

    def contains(cave: Cave) : Boolean = caves.contains(cave)

    def count(cave: Cave): Int = caves.count(_ == cave)

    def current: Cave = caves.last

    def size: Int = caves.size

    def +(cave: Cave): CavePath = CavePath(caves ::: List(cave))
  }

  object CavePath {
    def apply(cave: Cave): CavePath = CavePath(List(cave))
  }

  trait VisitStrategy {
    def canVisitSmall(path: CavePath, next: Cave): Boolean
  }

  class Navigator(caveGraph: CaveGraph, visitStrategy: VisitStrategy) {
    def generateAllPaths(): List[CavePath] = {
      val paths = new ListBuffer[CavePath]()
      generatePaths(CavePath(caveGraph.start), paths)
      paths.toList
    }

    private def generatePaths(path: CavePath, paths: ListBuffer[CavePath]): Unit = {
      val cave = path.current
      cave.connections
        .foreach { next =>
          if (next.isEnd) {
            paths.addOne(path + next)
          } else if (visitStrategy.canVisitSmall(path, next)) {
            generatePaths(path + next, paths)
          } else if (next.isBig && !path.startsWith(path + next)) {
            generatePaths(path + next, paths)
          }
        }
    }
  }

  class SmallOnceVisitStrategy extends VisitStrategy {
    override def canVisitSmall(path: CavePath, next: Cave): Boolean =
      next.isSmall && !path.contains(next)
  }

  class SingleSmallTwiceVisitStrategy extends VisitStrategy {
    override def canVisitSmall(path: CavePath, next: Cave): Boolean =
      next.isSmall && !next.isStart && (!path.contains(next) || areSmallOnPathAtMostOnce(path))

    private def areSmallOnPathAtMostOnce(path: CavePath): Boolean =
      path
        .caves
        .filter { cave => cave.isSmall }
        .groupBy(identity)
        .mapValues(_.size)
        .values
        .forall(_ < 2)
  }
}
