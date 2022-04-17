import scala.io.Source

object Day01 {
  def countIncreases(depths: List[Int]): Int = {
    (1 until depths.length)
      .count(i => depths(i - 1) < depths(i))
  }

  def countIncreasesWindow(depths: List[Int], window: Int): Int = {
    val windowCount = depths.length / window
    val maxWindowIndex = windowCount * window
    val windowSums = (0 until maxWindowIndex)
      .map(i => (i until i + window).map(i => depths(i)).sum)
    (1 until windowSums.length).count(i => windowSums(i - 1) < windowSums(i))
  }

  def readDepths(source: Source) = source.getLines()
    .filter(l => l.trim().nonEmpty)
    .map(l => l.toInt)
    .toList
}
