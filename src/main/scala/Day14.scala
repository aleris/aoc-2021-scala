import scala.collection.mutable
import scala.io.Source

object Day14 {
  case class PolymerPair(first: Char, second: Char) {
    override def toString: String = s"$first$second"
  }

  object PolymerPair {
    def apply(pairStr: String): PolymerPair = PolymerPair(pairStr(0), pairStr(1))
  }

  case class Instructions(template: String, insertions: Map[PolymerPair, Char]) { }

  case class Statistics(instructions: Instructions, counters: Map[PolymerPair, Long]) {
    def counts: Map[Char, Long] = {
      val charCounts = counters
        .toSeq
        .map { case (polymerPair, count) => polymerPair.first -> count }
      val last = Seq(instructions.template.last -> 1L)
      (charCounts ++ last)
        .groupBy { case (char, _) => char}
        .mapValues { values =>
          values.map { case (_, count) => count}.sum
        }
        .toMap
    }
  }

  class PolymerizationEquipment {

    def run(steps: Int, instructions: Instructions): Statistics = {
      val initialTemplateCounters =
        instructions.template.toCharArray
          .sliding(2)
          .map(a => PolymerPair(a(0), a(1)) -> 1L)
          .toSeq
          .groupBy { case (polymerPair, _) => polymerPair }
          .mapValues(_.size.toLong)
          .toMap

      var counters = initialTemplateCounters
      (0 until steps).foreach { step =>
        counters = runStep(counters, instructions)
      }

      Statistics(instructions, counters)
    }

    def runStep(counters: Map[PolymerPair, Long], instructions: Instructions): Map[PolymerPair, Long] = {
      val insertions = instructions.insertions
      val newCounters = mutable.Map[PolymerPair, Long]()
      val pairs = counters.keys
      for (pair <- pairs) {
        val count = counters(pair)
        val n = insertions(pair)
        val p1 = PolymerPair(pair.first, n)
        newCounters(p1) = count + newCounters.getOrElse(p1, 0L)
        val p2 = PolymerPair(n, pair.second)
        newCounters(p2) = count + newCounters.getOrElse(p2, 0L)
      }
      newCounters.toMap
    }
  }

  def subtractedMostAndLeastCounts(counts: Map[Char, Long]): Long = {
    val sorted = counts.toList.sortBy { case (_, count) => count }
    sorted.last._2 - sorted.head._2
  }

  def read(source: Source): Instructions = {
    val lines = source.getLines().toList
    val insertionLines = lines.splitAt(2)._2
    val template = lines.head
    Instructions(
      template,
      insertionLines.map(_.split(" -> "))
        .map { s =>
          val fromPairStr = s(0)
          val replacement: Char = s(1)(0)
          PolymerPair(fromPairStr(0), fromPairStr(1)) -> replacement
        }.toMap,
    )
  }
}
