import scala.annotation.tailrec

object Strain {
  def keep[T](seq: Seq[T], f: T => Boolean): Seq[T] = filter(seq)(f)

  def discard[T](seq: Seq[T], f: T => Boolean): Seq[T] = filter(seq)(not(f))

  private def not[T]: (T => Boolean) => T => Boolean = f => x => !f(x)

  private def filter[T](seq: Seq[T])(f: T => Boolean): Seq[T] = {
    @tailrec
    def rec(rest: Seq[T], acc: Seq[T]): Seq[T] = rest match {
      case Nil             => acc
      case x :: xs if f(x) => rec(xs, x +: acc)
      case _ :: xs         => rec(xs, acc)
    }
    rec(seq.reverse, Seq.empty[T])
  }

}
