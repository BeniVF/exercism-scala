import scala.annotation.tailrec

object SumOfMultiples extends App {

  def sum(factors: Set[Int], limit: Int): Int = {
    val max = limit - 1

    def sumFactors(factors: Set[Int]): Int =
      factors.map(sumOfMultiple(_, max)).sum

    val commonFactors = factors.toList.combinations(2).map(_.foldLeft(1)(lcm)).toSet

    sumFactors(factors) - sumFactors(commonFactors)

  }

  private def sumOfMultiple(factor: Int, number: Int): Int =
    factor * sumOfFirst(countDivisors(factor, number))

  private def sumOfFirst(number: Int): Int = (number * (number + 1)) / 2

  private def countDivisors(factor: Int, number: Int): Int = (number - number % factor) / factor
  @tailrec
  private def gcd(x: Int, y: Int): Int = if (y == 0) x else gcd(y, x % y)

  private def lcm(x: Int, y: Int): Int = x * (y / gcd(x, y))

}
