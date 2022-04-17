import scala.io.Source

object Day02 {
  object Directions extends Enumeration {
    type Direction = Value
    val Forward = Value("forward")
    val Up = Value("up")
    val Down = Value("down")
  }

  case class Move(direction: Directions.Direction, distance: Int)

  def followMoves(moves: List[Move]): Int = {
    var depth = 0
    var position = 0
    for (move <- moves) {
      move match {
        case Move(Directions.Forward, distance) => position += distance
        case Move(Directions.Up, distance) => depth -= distance
        case Move(Directions.Down, distance) => depth += distance
      }
    }
    depth * position
  }

  def followMovesWithAim(moves: List[Move]): Int = {
    var depth = 0
    var position = 0
    var aim = 0
    for (move <- moves) {
      move match {
        case Move(Directions.Forward, distance) => {
          position += distance
          depth += aim * distance
        }
        case Move(Directions.Up, distance) => aim -= distance
        case Move(Directions.Down, distance) => aim += distance
      }
    }
    depth * position
  }

  def readMoves(source: Source): List[Move] = source.getLines()
    .filter(l => l.trim().nonEmpty)
    .map { l =>
      val segments = l.split(" ")
      Move(Directions.withName(segments(0)), segments(1).toInt)
    }
    .toList
}
