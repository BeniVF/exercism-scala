object Acronym {
  def abbreviate(phrase: String): String =
    phrase
      .split(" |-")
      .filter(_.nonEmpty)
      .map(_.head.toUpper)
      .mkString("")
}
