import scala.io.Source

object Day13 {
  case class Paper(data: Array[Array[Boolean]]) {
    def height: Int = data.length
    def width: Int = data.head.length

    def apply(x: Int, y: Int): Boolean = data(y)(x)

    def update(x: Int, y: Int, value: Boolean): Unit = data(y)(x) = value

    override def toString: String =
      data.map(_.map {
        case true => '#'
        case false => '.'
      }.mkString("")).mkString("\n") + "\n"

    def visibleCount: Int = data.map(_.count(_ == true)).sum

    def fold(fold: Fold): Paper =
      fold match {
        case FoldUp(y) => foldUp(y)
        case FoldLeft(x) => foldLeft(x)
      }

    private def foldUp(foldLine: Int): Paper = {
      // fold line is allways half in the problem so no need to pad the folded paper
      val foldedPaper = Paper(Array.fill(foldLine, width) { false })
      for (y <- 0 until foldLine; x <- 0 until width) {
        foldedPaper(x, y) = this(x, y)
        val fy = height - y - 1
        if (this(x, fy)) foldedPaper(x, y) = true
      }
      foldedPaper
    }

    private def foldLeft(foldLine: Int): Paper = {
      val foldedPaper = Paper(Array.fill(height, foldLine) { false })
      for (y <- 0 until height; x <- 0 until foldLine) {
        foldedPaper(x, y) = this(x, y)
        val fx = width - x - 1
        if (this(fx, y)) foldedPaper(x, y) = true
      }
      foldedPaper
    }
  }

  object Paper {
    def apply(dotLines: Iterator[String]): Paper = {
      val dots = dotLines.map(_.split(","))
        .map { coordinates => (coordinates(0).toInt, coordinates(1).toInt)}
        .toList
      val width = dots.map { case (x, _) => x }.max + 1
      val height = dots.map { case (_, y) => y }.max + 1
      val data = Array.fill(height, width) { false }
      dots.foreach { case (x, y) => data(y)(x) = true }
      Paper(data)
    }
  }

  sealed class Fold

  object Fold {
    private val instructionPattern = "fold along (x|y)=(\\d+)".r
    def apply(foldInstruction: String): Fold = {
      val instructionPattern(fold, value) = foldInstruction
      fold match {
        case "x" => FoldLeft(value.toInt)
        case "y" => FoldUp(value.toInt)
      }
    }
  }

  case class FoldLeft(x: Int) extends Fold

  case class FoldUp(y: Int) extends Fold

  case class Sheet(paper: Paper, folds: List[Fold])

  object Sheet {
    def apply(source: Source): Sheet = {
      val lines = source.getLines()

      val paper = Paper(lines.takeWhile(_.nonEmpty))

      val folds = lines.filter(_.startsWith("fold along"))
        .map { foldInstruction => Fold(foldInstruction) }
        .toList

      Sheet(paper, folds)
    }
  }
}
