import scala.io.Source

object Day08 {
  case class Entry(uniqueSignalPatterns: List[Set[Char]], outputValue: List[Set[Char]])
  def readAllEntries(source: Source): List[Entry] = source.getLines().map { line =>
    val segmentList = line.split('|')
    val uniqueSignalPatterns = parseEntryPart(segmentList(0))
    val outputValue = parseEntryPart(segmentList(1))
    Entry(uniqueSignalPatterns, outputValue)
  }.toList

  def countWithUniqueNumberOfSegments(entries: List[Entry]): Int =
    entries.map { _.outputValue.count(hasUniqueNumberOfSegments) }.sum

  def sumOfDecoded(entries: List[Entry]): Int =
    entries.map { entry => Integer.parseInt(decodeOutput(entry)) }.sum

  def decodeOutput(entry: Entry): String = {
    val mapping = mapEntry(entry)
    entry.outputValue.map(dig => mapping(dig)).map(segmentsToDigit).mkString("")
  }

  // acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab
  // 🕵️ mode:
  //
  // (unique)
  // ab = 1
  // dab = 7
  // eafb = 4
  // abcdefg = 8
  //
  // (2, 3, 5)
  // cdfbe & ab(1) = b
  // gcdfa & ab(1) = a
  // fbcad & ab(1) = ab => fbcad = 3
  //
  // (0, 6, 9)
  // cefabd & fbcad(3) = cfabd => cefabd = 9
  // cdfgeb & fbcad(3) = cdfb
  // cagedb & fbcad(3) = cadb
  //
  // (2, 5)
  // cdfbe & cefabd(9) = cdfbe => cdfbe = 5
  // gcdfa & cefabd(9) = cdfa => gcdfa = 2
  //
  // (0, 6)
  // cdfgeb & ab(1) = b => cdfgeb = 6
  // cagedb & ab(1) = ab => cagedb = 0
  //
  def mapEntry(entry: Entry): Map[Set[Char], Set[Char]] = {
    var map: Map[Set[Char], Set[Char]] = Map()

    val d1 = entry.uniqueSignalPatterns.find(_.size == 2).get
    map += (d1 -> digitToSegments(1))

    val d4 = entry.uniqueSignalPatterns.find(_.size == 4).get
    map += d4 -> digitToSegments(4)

    val d7 = entry.uniqueSignalPatterns.find(_.size == 3).get
    map += d7 -> digitToSegments(7)

    val d8 = entry.uniqueSignalPatterns.find(_.size == 7).get
    map += d8 -> digitToSegments(8)

    val d235 = entry.uniqueSignalPatterns.filter(_.size == 5)
    val d3 = d235.find(_.intersect(d1).size == 2).get
    map += d3 -> digitToSegments(3)

    val d069 = entry.uniqueSignalPatterns.filter(_.size == 6)
    val d9 = d069.find(_.intersect(d3).size == 5).get
    map += d9 -> digitToSegments(9)

    val d25 = d235.filter(_ != d3)
    val d5 = d25.find(_.intersect(d9).size == 5).get
    map += d5 -> digitToSegments(5)
    val d2 = d25.find(_.intersect(d9).size == 4).get
    map += d2 -> digitToSegments(2)

    val d06 = d069.filter(_ != d9)
    val d0 = d06.find(_.intersect(d1).size == 2).get
    map += d0 -> digitToSegments(0)
    val d6 = d06.find(_.intersect(d1).size == 1).get
    map += d6 -> digitToSegments(6)

    map
  }

  private def parseEntryPart(entryPart: String): List[Set[Char]] = parseDigitList(entryPart.trim.split(' '))
  private def parseDigitList(digitList: Array[String]): List[Set[Char]] = digitList.map { parseDigit }.toList
  private def parseDigit(digit: String): Set[Char] = Set(digit.toCharArray: _*)

  private val digitSegmentsList = List(
    "abcefg",  // 0
    "cf",      // 1 *
    "acdeg",   // 2
    "acdfg",   // 3
    "bcdf",    // 4 *
    "abdfg",   // 5
    "abdefg",  // 6
    "acf",     // 7 *
    "abcdefg", // 8
    "abcdfg"   // 9
  )

  def segmentsOfDigitString(s: String): Set[Char] = s.toCharArray.toSet

  val digitToSegments: Map[Int, Set[Char]] = digitSegmentsList
    .map {segmentsOfDigitString}
    .zipWithIndex
    .map {si => (si._2, si._1)}
    .toMap

  private val segmentsToDigit = digitSegmentsList
    .map {segmentsOfDigitString}
    .zipWithIndex
    .toMap

  private def hasUniqueNumberOfSegments(digit: Set[Char]): Boolean = digit.size match {
    case 2 => true // 1
    case 3 => true // 7
    case 4 => true // 4
    case 7 => true // 8
    case _ => false
  }
}
