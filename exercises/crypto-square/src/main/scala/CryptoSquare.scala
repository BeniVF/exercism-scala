object CryptoSquare {
  type Rectangle = Vector[String]
  import Math._

  def ciphertext(value: String): String = value match {
    case _ if value.isEmpty => value
    case _ =>
      encrypt(buildRectangle(normalize(value)))
        .mkString(" ")
  }

  private def encrypt(value: Rectangle): Seq[String] =
    (0 until value.head.length).map { j =>
      value.map { x =>
        if (j < x.length)
          x(j)
        else " "
      }.mkString
    }

  private def buildRectangle(value: String): Rectangle = {
    val size = ceil(sqrt(value.length)).toInt
    value.grouped(size).toVector
  }

  private def normalize(value: String): String =
    value
      .toLowerCase()
      .filter(_.isLetterOrDigit)

}
