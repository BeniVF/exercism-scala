import Coordinates._
case class Robot(bearing: Bearing, coordinates: Coordinates) {
  def turnRight: Robot = this.copy(Bearing.rightFrom(bearing))

  def turnLeft: Robot = this.copy(Bearing.leftFrom(bearing))

  def advance: Robot =
    this.copy(coordinates = Coordinates.combine(coordinates, Coordinates.next(bearing)))

  def simulate(command: String): Robot = command.foldLeft(this) {
    case (acc, 'L') => acc.turnLeft
    case (acc, 'R') => acc.turnRight
    case (acc, 'A') => acc.advance
    case (acc, _)   => acc
  }
}

object Coordinates {
  type Coordinates = (Int, Int)

  def combine(first: Coordinates, second: Coordinates): Coordinates =
    (first._1 + second._1, first._2 + second._2)

  def next(bearing: Bearing): Coordinates = bearing match {
    case Bearing.North => (0, 1)
    case Bearing.South => (0, -1)
    case Bearing.East  => (1, 0)
    case Bearing.West  => (-1, 0)
  }
}

sealed trait Bearing
object Bearing {
  case object North extends Bearing
  case object South extends Bearing
  case object West extends Bearing
  case object East extends Bearing

  def leftFrom(direction: Bearing): Bearing = direction match {
    case North => West
    case South => East
    case West  => South
    case East  => North
  }

  def rightFrom(direction: Bearing): Bearing = direction match {
    case North => East
    case South => West
    case West  => North
    case East  => South
  }
}
