object Hexadecimal {
  private val specialChars: Map[Char, Int] = ('A' to 'F').zipWithIndex.map {
    case (c, i) => c -> (i + 10)
  }.toMap

  import Math._
  def hexToInt(str: String): Int = {
    val normalizedInput = str.toUpperCase
    if (validateHexadecimal(normalizedInput)) {
      val size = normalizedInput.length
      normalizedInput.zipWithIndex.map {
        case (next, i) =>
          valueFrom(next) * pow(
            16,
            size - i - 1
          ).toInt
      }.sum
    } else
      0
  }

  private def valueFrom(next: Char): Int =
    specialChars.getOrElse(next, next.toString.toInt)

  private def validateHexadecimal(str: String): Boolean =
    str.forall(c => specialChars.contains(c) || c.isDigit)

}
