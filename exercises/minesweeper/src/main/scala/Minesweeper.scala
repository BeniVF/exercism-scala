object Minesweeper {
  type Matrix[T] = IndexedSeq[IndexedSeq[T]]
  type Board = Matrix[Square]

  def annotate(value: List[String]): List[String] =
    showMatrix(calculateMines(unsafeParse(value)))

  private def unsafeParse(value: List[String]): Board =
    value
      .map(_.map(Square.unsafeFrom).toVector)
      .toVector

  private def showMatrix[T](matrix: Matrix[T]): List[String] =
    matrix.map { _.mkString("") }.toList

  private def calculateMines(parsed: Board): Matrix[String] =
    parsed.indices.map { i =>
      parsed(0).indices.map { j =>
        parsed(i)(j) match {
          case Mine => "*"
          case Safe =>
            val r = calculateMinesNearby(i, j)(parsed)
            if (r > 0) r.toString else " "
        }
      }
    }

  private def calculateMinesNearby(i: Int, j: Int)(parsed: Board): Int = {
    val maxI = parsed.length - 1
    val maxJ = parsed(0).length - 1
    def minesIn(i: Int, j: Int): Int = parsed(i)(j) match {
      case Mine => 1
      case Safe => 0
    }
    def checkLimits(ni: Int, nj: Int): Boolean =
      ni >= 0 && ni <= maxI && nj >= 0 && nj <= maxJ

    val result = for {
      x <- -1 to 1
      y <- -1 to 1

      ni = i + x
      nj = j + y
      if checkLimits(ni, nj)
    } yield minesIn(ni, nj)

    result.sum
  }
}

sealed trait Square
case object Mine extends Square
case object Safe extends Square

object Square {

  def unsafeFrom(source: Char): Square = source match {
    case '*' => Mine
    case ' ' => Safe
  }

}
