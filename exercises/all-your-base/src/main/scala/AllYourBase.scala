import scala.annotation.tailrec

object AllYourBase {
  def rebase(inputBase: Int, xs: List[Int], targetBase: Int): Option[List[Int]] =
    (inputBase, targetBase) match {
      case (input, target) if input < 2 || target < 2 => None
      case (_, 10)                                    => toBase10(inputBase, xs).map(_.toString.map(_.toString.toInt).toList)
      case (_, _)                                     => toBase10(inputBase, xs).map(convert(targetBase, _))
    }

  private def toBase10(base: Int, xs: List[Int]): Option[Int] =
    xs.zipWithIndex.foldLeft(Option(0)) {
      case (acc, (d, i)) =>
        acc.flatMap { x =>
          if (d >= 0 && d < base)
            Some(x + d * Math.pow(base, xs.size - i - 1).toInt)
          else
            None
        }
    }

  private def convert(base: Int, number: Int): List[Int] = {
    @tailrec
    def rec(rest: Int, acc: List[Int]): List[Int] = rest match {
      case 0 => acc
      case _ =>
        rec(rest / base, (rest % base) :: acc)
    }
    number match {
      case 0 => List(0)
      case _ => rec(number, List.empty)
    }
  }

}
