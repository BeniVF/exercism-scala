object Darts {
  import Math._
  type Coordinates = (Double, Double)
  private val bullseye: Coordinates = (0, 0)

  def score(x: Double, y: Double): Int = distance((x, y), bullseye) match {
    case d if d <= 1  => 10
    case d if d <= 5  => 5
    case d if d <= 10 => 1
    case _            => 0
  }

  private def distance(first: Coordinates, second: Coordinates): Double =
    sqrt(pow(abs(first._1 - second._1), 2) + pow(abs(first._2 - second._2), 2))
}
