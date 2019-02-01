object BeerSong {

  def recite(from: Int, takeDown: Int): String =
    (from - takeDown + 1 to from)
      .foldLeft(List.empty[String]) {
        case (acc, n) =>
          beersFrom(n) :: acc
      }
      .mkString("\n")

  private def beersFrom(n: Int): String = n match {
    case 0 => """No more bottles of beer on the wall, no more bottles of beer.
                |Go to the store and buy some more, 99 bottles of beer on the wall.
                |""".stripMargin
    case 1 => """1 bottle of beer on the wall, 1 bottle of beer.
                |Take it down and pass it around, no more bottles of beer on the wall.
                |""".stripMargin
    case 2 => """2 bottles of beer on the wall, 2 bottles of beer.
                |Take one down and pass it around, 1 bottle of beer on the wall.
                |""".stripMargin
    case _ => s"""$n bottles of beer on the wall, $n bottles of beer.
                  |Take one down and pass it around, ${n - 1} bottles of beer on the wall.
                  |""".stripMargin
  }

}
