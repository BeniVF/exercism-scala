object Bob {
  def response(statement: String): String = statement.trim match {
    case x if isSilent(x)                   => "Fine. Be that way!"
    case x if isYelling(x) && isQuestion(x) => "Calm down, I know what I'm doing!"
    case x if isYelling(x)                  => "Whoa, chill out!"
    case x if isQuestion(x)                 => "Sure."
    case _                                  => "Whatever."
  }

  private def isSilent(statement: String): Boolean = statement.isEmpty

  private def isQuestion(statement: String): Boolean = statement.endsWith("?")

  private def isYelling(statement: String): Boolean = {
    val onlyLetters = statement.filter(_.isLetter)
    onlyLetters.length > 0 && onlyLetters.forall(_.isUpper)
  }
}
