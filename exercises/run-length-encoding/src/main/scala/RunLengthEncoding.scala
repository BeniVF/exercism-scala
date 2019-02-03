import scala.annotation.tailrec

object RunLengthEncoding {

  def encode(value: String): String =
    EncodedChar.encodeFrom(value.foldLeft(List.empty[EncodedChar]) {
      case (Nil, n)                               => List(EncodedChar(n, 1))
      case (EncodedChar(c, i) :: xs, n) if c == n => EncodedChar(c, i + 1) :: xs
      case (xs, n)                                => EncodedChar(n, 1) :: xs
    })

  def decode(value: String): String = {
    @tailrec
    def rec(value: List[Char], acc: List[EncodedChar]): List[EncodedChar] = value match {
      case Nil                      => acc
      case x :: xs if x.isSpaceChar => rec(xs, EncodedChar(x, 1) :: acc)
      case x :: xs if x.isLetter    => rec(xs, EncodedChar(x, 1) :: acc)
      case x :: xs =>
        val number = (x :: xs).takeWhile(_.isDigit).mkString("").toInt
        val rest = xs.dropWhile(_.isDigit)
        rec(if (rest.nonEmpty) rest.tail else Nil, EncodedChar(rest.head, number) :: acc)

    }
    EncodedChar.decodeFrom(rec(value.toList, List.empty))
  }

  private final case class EncodedChar(char: Char, number: Int)

  private object EncodedChar {
    def encodeFrom(xs: List[EncodedChar]): String = xs.foldLeft("") {
      case (acc, EncodedChar(n, 1)) => s"$n$acc"
      case (acc, EncodedChar(n, i)) => s"$i$n$acc"
    }

    def decodeFrom(xs: List[EncodedChar]): String = xs.foldLeft("") {
      case (acc, EncodedChar(n, 1)) => s"$n$acc"
      case (acc, EncodedChar(n, i)) => (1 to i).map(_ => n).mkString("") ++ acc
    }
  }

}
