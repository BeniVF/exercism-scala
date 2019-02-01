trait Binary {
  def toDecimal: Int
}

object Binary {

  def apply(value: String): Binary =
    Binary(binaryFrom(value))

  private def binaryFrom(value: String): Option[List[Int]] =
    if (value.forall(x => x == '0' || x == '1'))
      Some(value.toList.map(_.toString.toInt))
    else
      None

  private def apply(value: Option[List[Int]]): Binary = new Binary {
    def toDecimal: Int = value.fold(0) { digits =>
      digits.zipWithIndex
        .map {
          case (d, i) =>
            d * Math.pow(2, digits.length - i - 1)
        }
        .sum
        .toInt
    }
  }

}
