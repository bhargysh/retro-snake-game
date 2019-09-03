package snake

class SnakeGameWorld(snake: Snake, board: Board, food: Option[Food]) {

}

case class Snake(location: Location, length: Int)
case class Location(x: Int, y: Int)
case class Board(cell: Array[Cell], width: Int, height: Int)
sealed trait Cell
case object SnakePart extends Cell
case object Wall extends Cell
case class Food(location: Location) //expiry time
