trait WordCount {
  def countWords: Map[String, Int]
}

object WordCount {

  private val separators = " |\n|,"

  def apply(sentence: String): WordCount = new WordCount {
    override def countWords: Map[String, Int] =
      sentence
        .split(separators)
        .map(wordFrom)
        .filter(_.nonEmpty)
        .groupBy(identity)
        .mapValues(_.length)
  }

  private def wordFrom(value: String): String =
    removeQuotations(
      value
        .toLowerCase()
        .filter(c => c.isLetterOrDigit || c == '\''))

  private def removeQuotations(x: String): String =
    if (x.startsWith("'") || x.endsWith("'"))
      if (x.length > 2)
        x.init.tail
      else ""
    else x
}
