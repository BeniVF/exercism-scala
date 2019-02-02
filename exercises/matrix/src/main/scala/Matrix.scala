trait Matrix[T] {
  def row(i: Int): Vector[T]
  def column(j: Int): Vector[T]
}
object Matrix {

  def apply(value: String): Matrix[Int] = new Matrix[Int] {
    private val unsafeMatrix = value
      .split("\n")
      .map(
        _.split(" ")
          .map(_.toInt))

    private lazy val maxI = unsafeMatrix.length

    override def row(i: Int): Vector[Int] = unsafeMatrix(i).toVector

    override def column(j: Int): Vector[Int] =
      (0 until maxI).map { i =>
        unsafeMatrix(i)(j)
      }.toVector

  }

}
