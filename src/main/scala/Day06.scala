import scala.io.Source

object Day06 {
  class School(list: List[Int]) {
    var map: Map[Int, Long] = Map[Int, Long]()

    list.foreach { n =>
      val count = map.getOrElse(n, 0L) + 1L
      map += n -> count
    }

    def simulate(days: Int): Unit = for (_ <- 0 until days) { simulateOneDay() }

    def count: Long = map.values.sum

    private def simulateOneDay(): Unit = {
      val spawnCount = map.getOrElse(0, 0L)
      val newPairs = map.toList.map(pair => {
        val (age, count) = pair
        val newAge = age match {
          case 0 => 6
          case _ => age - 1
        }
        (newAge, count)
      })
      map = newPairs.groupMap(_._1)(_._2).map(pair => pair._1 -> pair._2.sum)
      if (spawnCount != 0) {
        map += (8 -> spawnCount)
      }
    }

    override def toString: String = map.toString()
  }

  def readInitial(source: Source): School =
    new School(source.getLines().next().split(',').map(_.toInt).toList)
}
