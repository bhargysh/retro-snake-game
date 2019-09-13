package snake

class SnakeGameWorld(snake: Snake, board: Board, food: Option[Food]) {

}

case class Snake(location: Location, length: Int)
case class Location(x: Int, y: Int)
case class Board(cell: Array[Cell], width: Int, height: Int)
sealed trait Cell
case object SnakePart extends Cell
case object Wall extends Cell
case object EmptyCell extends Cell
case class Food(location: Location) //expiry time

object SnakeGameWorld {
  private val emptyCells: Array[Cell] = Array.fill(100)(EmptyCell)
  val board = Board(emptyCells, 10, 10)

  private val location = Location(5, 5)
  private val snake = Snake(location, 2)

  val food: Option[Food] = None

  def newSnakeGameWorld = {
    new SnakeGameWorld(snake, board, food)
  }
}