import scala.collection.immutable.HashSet

object BracketPush {
  private val brackets = HashSet('(', '{', '[', ')', '}', ']')

  def isPaired(value: String): Boolean =
    if (value.isEmpty) true
    else
      value
        .foldLeft(List.empty[Char]) {
          case ('(' :: xs, ')')                 => xs
          case ('{' :: xs, '}')                 => xs
          case ('[' :: xs, ']')                 => xs
          case (acc, n) if brackets.contains(n) => n :: acc
          case (acc, _)                         => acc
        }
        .isEmpty

}
