package snake

case class SnakeGameWorld(snake: Snake, board: Board, food: Option[Food]) {

}

case class Snake(location: List[Location], length: Int, direction: Direction)
case class Location(x: Int, y: Int)
case class Board(cell: Array[Cell], width: Int, height: Int)
sealed trait Cell
case object SnakePart extends Cell
case object Wall extends Cell
case object EmptyCell extends Cell
case class Food(location: Location) //expiry time
sealed trait Direction
case object Up extends Direction
case object Down extends Direction
case object Left extends Direction
case object Right extends Direction

object SnakeGameWorld {
  private val emptyCells: Array[Cell] = Array.tabulate(100){ index =>
    if (index % 10 == 0 || index % 10 == 9 ) {
      Wall
    }
    else if (index / 10 == 0 || index / 10 == 9){
      Wall
    }
    else {
      EmptyCell
    }
  }
  val board = Board(emptyCells, 10, 10)

  private val snake = Snake(List(Location(5, 5), Location(5,4)), 2, Up)

  val food: Option[Food] = None

  def newSnakeGameWorld = {
    new SnakeGameWorld(snake, board, food)
  }
}