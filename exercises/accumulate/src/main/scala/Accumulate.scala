import scala.annotation.tailrec

class Accumulate {
  def accumulate[A, B](f: A => B, list : List[A]): List[B] = {
    @tailrec
    def rec(rest: List[A], acc: List[B]): List[B] = rest match {
      case Nil => acc
      case x::xs => rec(xs, f(x)::acc)
    }
    rec(list, List.empty).reverse
  }
}
