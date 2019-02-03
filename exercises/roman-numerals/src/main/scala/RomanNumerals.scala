object RomanNumerals {

  private val numberToRoman = List((1000, "M"),
                                   (900, "CM"),
                                   (500, "D"),
                                   (400, "CD"),
                                   (100, "C"),
                                   (90, "XC"),
                                   (50, "L"),
                                   (40, "XL"),
                                   (10, "X"),
                                   (9, "IX"),
                                   (5, "V"),
                                   (4, "IV"),
                                   (1, "I"))
  def roman(number: Int): String = number match {
    case _ if number <= 0 => "" // it should be using option/either
    case _ =>
      numberToRoman
        .foldLeft(("", number)) {
          case ((acc, rest), (amount, roman)) =>
            (acc ++ (1 to (rest / amount)).map(_ => roman).mkString(""), rest % amount)
        }
        ._1
  }

}
