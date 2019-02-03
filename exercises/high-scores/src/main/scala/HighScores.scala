object HighScores {

  def latest(value: List[Int]): Int =
    if (value.nonEmpty)
      value.last
    else
      sys.error(s"It should not be called") // It should use option/either

  def personalBest(value: List[Int]): Int =
    if (value.nonEmpty)
      value.max
    else
      sys.error(s"It should not be called") // It should use option/either

  def personalTop(value: List[Int]): List[Int] =
    value
      .sorted(Ordering[Int].reverse)
      .take(3)

  def report(value: List[Int]): String = {
    val latestScore = latest(value)
    personalBest(value) - latestScore match {
      case 0    => s"Your latest score was $latestScore. That's your personal best!"
      case diff => s"Your latest score was $latestScore. That's $diff short of your personal best!"
    }

  }

}
