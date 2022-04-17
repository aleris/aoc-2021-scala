import scala.io.Source
import scala.language.postfixOps

object Day03 {
  def readPowerConsumption(lines: List[String]) = {
    val bitCounts = aggregateBitCounts(lines)
    val length = lines.head.length
    def asStrByLength(one: Char, zero: Char) = (0 until length) map { i =>
      val (ones, zeros) = bitCounts(i)
      if (ones < zeros) zero else one
    } mkString ""
    val gamma = Integer.parseInt(asStrByLength('1', '0'), 2)
    val epsilon = Integer.parseInt(asStrByLength('0', '1'), 2)
    gamma * epsilon
  }

  def readLifeSupportRating(lines: List[String]): Int = {
    val oxygenGeneratorRatingCriteria = (ones: Int, zeros: Int) => if (ones < zeros) '0' else '1'
    val oxygenGeneratorRating = readLifeSupportParam(lines, oxygenGeneratorRatingCriteria)
    val co2ScrubberRatingCriteria = (ones: Int, zeros: Int) => if (ones < zeros) '1' else '0'
    val co2ScrubberRating = readLifeSupportParam(lines, co2ScrubberRatingCriteria)
    oxygenGeneratorRating * co2ScrubberRating
  }

  def readLifeSupportParam(lines: List[String], criteria: (Int, Int) => Char): Int = {
    val length = lines.head.length
    var remainingLines = lines
    for (i <- (0 until length)) {
      val bitCounts = aggregateBitCounts(remainingLines)
      val (ones, zeros) = bitCounts(i)
      val bf = criteria(ones, zeros)
      remainingLines = remainingLines.filter(l => l(i) == bf)
      if (remainingLines.length == 1) return Integer.parseInt(remainingLines.head, 2)
    }
    0
  }

  def aggregateBitCounts(lines: List[String]) =  (0 until lines.head.length) map {i =>
    val bits = lines map { _(i) }
    val ones = bits filter { b => b == '1' }
    val zeros = bits filter { b => b == '0' }
    (ones.length, zeros.length)
  } toList

  def read(source: Source): List[String] = source.getLines().toList
}
