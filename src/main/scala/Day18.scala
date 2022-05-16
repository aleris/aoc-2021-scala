import scala.annotation.tailrec
import scala.collection.Seq

object Day18 {

  sealed abstract class Element(var parent: Option[Pair]) {
    def addSimple(other: Element): Element = {
      val r = Pair(None, this, other)
      this.parent = None
      other.parent = Some(r)
      this.parent = Some(r)
      r
    }

    def depth: Int = {
      @tailrec
      def depth(e: Element, n: Int): Int = e.parent match {
        case Some(p) => depth(p, n + 1)
        case None => n
      }
      depth(this, 0)
    }

    def magnitude: Int

    def findNextLeafRight(): Option[Number] = {
      if (this.parent.isEmpty) return None
      val parentPair = this.parent.get
      val right = parentPair.right
      if (right != this) {
        Some(leftLeaf(right))
      } else {
        parentPair.findNextLeafRight()
      }
    }

    def findNextLeafLeft(): Option[Number] = {
      if (this.parent.isEmpty) return None
      val parentPair = this.parent.get
      val left = parentPair.left
      if (left != this) {
        Some(rightLeaf(left))
      } else {
        parentPair.findNextLeafLeft()
      }
    }

    def leftToRightSeq: Seq[Element] = {
      def leftToRightSeq(e: Element): Seq[Element] = e match {
        case n: Number => Seq(n)
        case p: Pair => leftToRightSeq(p.left) ++ Seq(e) ++ leftToRightSeq(p.right)
      }
      leftToRightSeq(this)
    }

    def leftToRightPairsSeq: Seq[Pair] = this.leftToRightSeq.collect { case p: Pair => p }
    def leftToRightNumbersSeq: Seq[Number] = this.leftToRightSeq.collect { case n: Number => n }

    @tailrec
    private def leftLeaf(element: Element): Number = element match {
      case number: Number => number
      case pair: Pair => leftLeaf(pair.left)
    }

    @tailrec
    private def rightLeaf(element: Element): Number = element match {
      case number: Number => number
      case pair: Pair => rightLeaf(pair.right)
    }

    def reduce(recursive: Boolean): Element = {
      val pairDepth4 = leftToRightPairsSeq.find(4 <= _.depth)
      if (pairDepth4.nonEmpty) {
        pairDepth4.get.explode()
        if (recursive) reduce(recursive)
        return this
      }
      val numberGreater10 = leftToRightNumbersSeq.find(10 <= _.value)
      if (numberGreater10.nonEmpty) {
        numberGreater10.get.split()
        if (recursive) reduce(recursive)
        return this
      }
      this
    }

    def +(other: Element): Element = (this addSimple other).reduce(true)
  }

  class Pair(p: Option[Pair], var left: Element, var right: Element) extends Element(p) {
    override def toString = s"[$left,$right]"

    def explode(): Unit = {
      val nextLeft = findNextLeafLeft()
      if (nextLeft.nonEmpty) {
        nextLeft.get.value += left.asInstanceOf[Number].value
      }
      val nextRight = findNextLeafRight()
      if (nextRight.nonEmpty) {
        nextRight.get.value += right.asInstanceOf[Number].value
      }
      val parentPair = this.parent.get
      val number = Number(Some(parentPair), 0)
      if (parentPair.left == this) {
        parentPair.left = number
      } else {
        parentPair.right = number
      }
    }

    override def magnitude: Int = 3 * left.magnitude + 2 * right.magnitude
  }

  object Pair {
    def apply(parent: Option[Pair], left: Element, right: Element): Pair = new Pair(parent, left, right)
  }

  class Number(p: Option[Pair], var value: Int) extends Element(p) {
    override def toString = s"$value"

    def split(): Unit = {
      val parentPair = parent.get
      val leftNumber = Number(None, value / 2)
      val rightNumber = Number(None, value - leftNumber.value)
      val splinted = Pair(parent, leftNumber, rightNumber)
      leftNumber.parent = Some(splinted)
      rightNumber.parent = Some(splinted)
      if (parentPair.left == this) {
        parentPair.left = splinted
      } else {
        parentPair.right = splinted
      }
    }

    override def magnitude: Int = value
  }

  object Number {
    def apply(parent: Option[Pair], value: Int): Number = new Number(parent, value)
  }

  object Parser {
    sealed abstract class ParsedElement {
      def asTreeElement(): Element
    }

    case class P(left: ParsedElement, right: ParsedElement) extends ParsedElement {
      def asTreeElement(): Element = {
        val l = left.asTreeElement()
        val r = right.asTreeElement()
        val p = Pair(None, l, r)
        p.left.parent = Some(p)
        p.right.parent = Some(p)
        p
      }
    }

    case class N(value: Int) extends ParsedElement {
      override def asTreeElement(): Element = Number(None, value)
    }

    def parse(line: String): ParsedElement =
      this.parse(line, 0).element

    private case class ResultElement(endIndex: Int, element: ParsedElement)

    private def parse(line: String, index: Int): ResultElement = {
      val c = line(index)
      if (c == '[') {
        val firstParsed = parse(line, index + 1)
        val secondParsed = parse(line, firstParsed.endIndex + 1)
        ResultElement(secondParsed.endIndex, P(firstParsed.element, secondParsed.element))
      } else if (c == ',') {
        parse(line, index + 1)
      } else if (c == ']') {
        parse(line, index + 2)
      } else {
        var endOfDigitsIndex = index
        while (line(endOfDigitsIndex).isDigit) endOfDigitsIndex += 1
        val number = N(line.substring(index, endOfDigitsIndex).toInt)
        ResultElement(endOfDigitsIndex, number)
      }
    }

    object Resolver {
      def addLines(lines: Iterator[String]): Element =
        lines
          .map { line => Parser.parse(line).asTreeElement() }
          .reduceLeft { (e1, e2) => e1 + e2 }

      def findMax(lines: Iterator[String]): Int = {
        val elements = lines.map { line => Parser.parse(line) }.toSeq
        val pairs = for (e1 <- elements; e2 <- elements) yield (e1, e2)
        pairs.foldLeft(0) { (m, e) =>
          val e1 = e._1.asTreeElement()
          val e2 = e._2.asTreeElement()
          Math.max(m, (e1 + e2).magnitude)
        }
      }
    }
  }
}
