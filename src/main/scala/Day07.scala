import scala.io.Source

object Day07 {
  def readPositions(source: Source): List[Int] =
    source.getLines().next().split(',').map {_.toInt}.toList

  case class FuelCostPosition(cost: Int, position: Int)

  val linearFuelConsumption: (Int, Int) => Int = (p1: Int, p2: Int) => math.abs(p1 - p2)
  val incrementalFuelConsumption: (Int, Int) => Int = (p1: Int, p2: Int) => {
    val d = math.abs(p1 - p2)
    (1 to d).sum
  }

  def findOptimalCost(positions: List[Int], calcFuelConsumption: (Int, Int) => Int): FuelCostPosition = {
    val max = positions.max
    var minCost = Int.MaxValue
    var posForMinCost = -1
    for (position <- 0 to max) {
      val cost = calculateFuelCost(positions, position, calcFuelConsumption)
      if (cost < minCost) {
        minCost = cost
        posForMinCost = position
      }
    }
    FuelCostPosition(minCost, posForMinCost)
  }

  private def calculateFuelCost(positions: List[Int], position: Int, costFun: (Int, Int) => Int): Int =
    positions.map { pos => costFun(pos, position)}.sum
}
