object FlattenArray {
  def flatten(anyList: List[Any]): List[Any] = {
    def rec(next: List[Any]): List[Any] =
      next match {
        case Nil                           => Nil
        case (x: List[Any]) :: tail        => rec(x) ++ rec(tail)
        case x :: xs if Option(x).nonEmpty => x :: rec(xs)
        case _ :: xs                       => rec(xs)
      }
    rec(anyList)
  }

}
