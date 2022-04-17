import scala.collection.mutable
import scala.io.Source

object Day10 {
  def read(source: Source): List[String] = source.getLines().toList

  sealed class Result
  case class Incomplete(completion: String) extends Result
  case class Corrupted(expected: Char, found: Char) extends Result

  def analyse(lines: List[String]): List[Result] = lines.map(analyseLine)

  def analyseLine(line: String): Result = {
    val chars = line.toCharArray
    val stack = new mutable.Stack[Char]
    for (c <- chars) {
      if (isClose(c)) {
        val open = stack.pop()
        val found = closeToOpenMap(c)
        if (open != found) {
          return Corrupted(openToCloseMap(open), openToCloseMap(found))
        }
      }
      if (isOpen(c)) stack.push(c)
    }
    Incomplete(stack.map(openToCloseMap(_)).mkString(""))
  }

  def scoreCorrupted(results: List[Result]): Int =
    results.collect {case r: Corrupted => r}.map(scoreCorrupted).sum

  def scoreCorrupted(corrupted: Corrupted): Int = pointsForCorrupted(corrupted.found)

  def scoreIncomplete(results: List[Result]): Long = {
    val scores = results
      .collect {case r: Incomplete => r}
      .map(scoreIncomplete)
      .sorted
    scores(scores.size / 2)
  }

  def scoreIncomplete(incomplete: Incomplete): Long = scoreCompletion(incomplete.completion)

  def scoreCompletion(completion: String): Long =
    completion.toCharArray.foldLeft(0L)((a, c) => a * 5 + pointsForIncomplete(c))

  private val openToCloseMap = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>',
  )
  private val closeToOpenMap = openToCloseMap.map { case (open, close) => (close, open) }
  private val openChars = openToCloseMap.keys.toSet
  private val closeChars = openToCloseMap.values.toSet
  val pointsForCorrupted = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137,
  )
  val pointsForIncomplete = Map(
    ')' -> 1L,
    ']' -> 2L,
    '}' -> 3L,
    '>' -> 4L,
  )
  private def isOpen(c: Char): Boolean = openChars.contains(c)
  private def isClose(c: Char): Boolean = closeChars.contains(c)
}
