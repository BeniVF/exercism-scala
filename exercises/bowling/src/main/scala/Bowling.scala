import scala.annotation.tailrec

case class Bowling private (rolls: List[Int]) {
  import Error._

  def roll(pins: Int): Bowling = Bowling(rolls ++ List(pins))

  def score(): Either[Error, Int] = validateRolls.flatMap {
    calculatePoints(_, 1, Right(0))
  }

  private def validateRolls: Either[Error, List[Int]] = rolls match {
    case _ if rolls.exists(_ > 10) => Left(WrongNumberPins)
    case _ if rolls.exists(_ < 0)  => Left(WrongNumberPins)
    case _                         => Right(rolls)
  }

  @tailrec
  private def calculatePoints(rolls: List[Int],
                              frame: Int,
                              acc: Either[Error, Int]): Either[Error, Int] =
    (acc, rolls) match {
      case (Left(error), _) => Left(error)
      case (_, Nil) if frame < 10 =>
        Left(RequiresMoreRolls)

      case (_, Nil) => acc

      case (_, x :: Nil) if lastFrame(frame) && isStrike(x) =>
        Left(RequiresBonusRolls)

      case (_, x :: y :: Nil) if lastFrame(frame) && isSpare(x, y) =>
        Left(RequiresBonusRolls)

      case (_, x :: y :: xs) if lastFrame(frame) && !isSpare(x, y) && !isStrike(x) && xs.nonEmpty =>
        Left(AlreadyHasTenFrames)

      case (_, x :: y :: z :: Nil)
          if lastFrame(frame) && isStrike(x) && !isStrike(y) && y + z > 10 =>
        Left(WrongNumberPins)

      case (_, x :: y :: z :: Nil) if lastFrame(frame) && isStrike(x) && isStrike(y) =>
        acc.map(_ + x + y + z)

      case (_, x :: y :: z :: Nil) if lastFrame(frame) && (isStrike(x) || isSpare(x, y)) =>
        acc.map(_ + x + y + z)

      case (_, x :: y :: z :: xs) if !lastFrame(frame) && isStrike(x) =>
        calculatePoints(y :: z :: xs, frame + 1, acc.map(_ + x + y + z))

      case (_, x :: y :: z :: xs) if !lastFrame(frame) && isSpare(x, y) =>
        calculatePoints(z :: xs, frame + 1, acc.map(_ + x + y + z))

      case (_, x :: y :: _) if x + y > 10 =>
        Left(FrameGreaterThanTen(x, y))

      case (_, x :: y :: xs) =>
        calculatePoints(xs, frame + 1, acc.map(_ + x + y))
    }

  private def lastFrame(frame: Int): Boolean = frame == 10
  private def isStrike(pins: Int): Boolean = pins == 10
  private def isSpare(first: Int, second: Int): Boolean =
    (first > 0 && second > 0) && (first + second == 10)

}

object Bowling {
  def apply(): Bowling = Bowling(List.empty)
}

sealed trait Error
object Error {
  case object WrongNumberPins extends Error
  case class FrameGreaterThanTen(first: Int, second: Int) extends Error
  case object AlreadyHasTenFrames extends Error
  case object RequiresBonusRolls extends Error
  case object RequiresMoreRolls extends Error
}
