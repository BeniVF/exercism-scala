import scala.annotation.tailrec

object CollatzConjecture {

  def steps(n: Int): Option[Int] = {
    @tailrec
    def rec(next: Int, steps: Int): Int = {
      next match {
        case 1                 => steps
        case _ if isEven(next) => rec(next / 2, steps + 1)
        case _                 => rec(3 * next + 1, steps + 1)
      }
    }
    n match {
      case 0          => None
      case _ if n < 0 => None
      case _          => Some(rec(n, 0))
    }
  }

  private def isEven(n: Int): Boolean = n % 2 == 0

}