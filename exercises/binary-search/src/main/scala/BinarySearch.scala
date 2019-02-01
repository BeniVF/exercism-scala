import scala.annotation.tailrec

object BinarySearch {

  def find(xs: List[Int], target: Int): Option[Int] = {
    val array = xs.toArray

    @tailrec
    def rec(i: Int, j: Int): Option[Int] = {
      if (j < i || i < 0 || (j + 1) > array.length) None
      else {
        val k = (i + j) / 2
        array(k) match {
          case value if target == value => Some(k)
          case value if target < value  => rec(i, k - 1)
          case _                        => rec(k + 1, j)
        }
      }
    }
    rec(0, array.length - 1)
  }

}
