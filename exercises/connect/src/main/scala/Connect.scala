import scala.collection.immutable.HashSet

sealed trait Color
object Color {
  case object Black extends Color
  case object White extends Color
}

trait Connect {
  def winner: Option[Color]
}

object Connect {
  type Board = Vector[Vector[Option[Color]]]
  type Coordinates = (Int, Int)

  def apply(input: List[String]): Connect = new Connect {
    private val board: Board = buildBoard
    private val validDirections: List[Coordinates] =
      List((1, 0), (-1, 0), (0, 1), (0, -1), (-1, 1), (1, -1))
    private val maxX: Int = board.length - 1
    private val maxY: Int = board.head.length - 1

    def winner: Option[Color] =
      winnerFrom(Color.White,
                 startCoordinates = (0 to maxY)
                   .map(j => (0, j)))
        .fold(
          winnerFrom(Color.Black,
                     startCoordinates = (0 to maxX)
                       .map(i => (i, 0)))) {
          Some(_)
        }

    private def winnerFrom(color: Color, startCoordinates: Seq[Coordinates]): Option[Color] =
      startCoordinates
        .filter(colorFrom(_).contains(color))
        .find(isAbleToConnect(color, _, HashSet.empty))
        .map(_ => color)

    private def isAbleToConnect(color: Color,
                                coordinates: Coordinates,
                                visited: HashSet[Coordinates]): Boolean =
      (coordinates, possibleConnections(coordinates)) match {
        case ((x, _), _) if x == maxX && color == Color.White => true
        case ((_, y), _) if y == maxY && color == Color.Black => true
        case (_, Nil)                                         => false
        case (_, xs) =>
          xs.filterNot(visited.contains)
            .exists(isAbleToConnect(color, _, visited + coordinates))
      }

    private def possibleConnections(coordinates: Coordinates): Seq[Coordinates] = {
      def checkLimits(coordinates: Coordinates): Boolean =
        coordinates._1 >= 0 && coordinates._1 <= maxX && coordinates._2 >= 0 && coordinates._2 <= maxY
      val expectedColor = colorFrom(coordinates)
      validDirections
        .map {
          case (i, j) =>
            (coordinates._1 + i, coordinates._2 + j)
        }
        .filter(checkLimits)
        .filter(x => expectedColor == colorFrom(x))
    }

    private def colorFrom(coordinates: Coordinates): Option[Color] =
      board(coordinates._1)(coordinates._2)

    private def buildBoard: Board =
      input.map {
        _.flatMap {
          case 'X' => List(Some(Color.Black))
          case 'O' => List(Some(Color.White))
          case ' ' => List.empty
          case _   => List(None)
        }.toVector
      }.toVector
  }

}
