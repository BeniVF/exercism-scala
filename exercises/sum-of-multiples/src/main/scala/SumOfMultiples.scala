object SumOfMultiples {

  def sum(factors: Set[Int], limit: Int): Int =
    (1 until limit)
      .filter(n => factors.exists(isDivisible(n, _)))
      .sum

  private def isDivisible(number: Int, divisor: Int): Boolean = number % divisor == 0

}
