object DifferenceOfSquares {
  import Math._

  def squareOfSum(n: Int): Int =
    pow(sumOfFirst(n), 2).toInt

  def sumOfSquares(n: Int): Int =
    ceil((pow(n, 3) / 3.0) + (pow(n, 2) / 2.0) + (n / 6.0)).toInt

  def differenceOfSquares(n: Int): Int =
    squareOfSum(n) - sumOfSquares(n)

  private def sumOfFirst(n: Int): Double =
    (n * (n + 1)) / 2.0

}
