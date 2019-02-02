object Etl {

  def transform(data: Map[Int, Seq[String]]): Map[String, Int] = data.flatMap {
    case (score, letters) =>
      letters.map(_.toLowerCase() -> score)
  }

}
