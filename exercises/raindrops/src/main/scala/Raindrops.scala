object Raindrops {
  private val divisorToOutput = Map(
    3 -> "Pling",
    5 -> "Plang",
    7 -> "Plong"
  )
  def convert(number: Int): String =
    divisorToOutput.keys
      .foldLeft[Option[String]](None) {
        case (acc, divisor) if isDivisible(number, divisor) =>
          val conversion = divisorToOutput.get(divisor)
          acc.fold(conversion)(x => conversion.map(x + _))
        case (acc, _) => acc
      }
      .getOrElse(number.toString)

  private def isDivisible(number: Int, divisor: Int): Boolean = number % divisor == 0
}
