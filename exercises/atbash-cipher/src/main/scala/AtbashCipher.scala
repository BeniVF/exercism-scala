object AtbashCipher {

  private val alpha = {
    val plain = "abcdefghijklmnopqrstuvwxyz"
    val cipher = "zyxwvutsrqponmlkjihgfedcba"
    plain.zip(cipher).toMap
  }

  def encode(value: String): String = transpose(normalize(value)).grouped(5).mkString(" ")

  def decode(value: String): String = transpose(normalize(value))

  private def transpose(value: String): String =
    value.map(x => alpha.getOrElse(x, x))
  private def normalize(value: String): String =
    value.toLowerCase.filter(_.isLetterOrDigit)

}
