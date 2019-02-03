trait Matrix {
  def saddlePoints: Set[Matrix.Coordinates]
}

object Matrix {
  type Coordinates = (Int, Int)
  def apply(values: List[List[Int]]): Matrix = new Matrix {
    private val matrix = values.map(_.toArray).toArray

    override def saddlePoints: Set[Coordinates] =
      if (matrix.nonEmpty)
        (for {
          i <- matrix.indices
          j <- matrix(0).indices
          if isSaddlePoint(i, j)
        } yield (i, j)).toSet
      else
        Set.empty

    private def isSaddlePoint(coordinates: Coordinates): Boolean =
      matrix(coordinates._1).max == valueOf(coordinates) &&
        matrix.map(x => x(coordinates._2)).min == valueOf(coordinates)

    private def valueOf(coordinates: Coordinates): Int =
      matrix(coordinates._1)(coordinates._2)
  }
}
