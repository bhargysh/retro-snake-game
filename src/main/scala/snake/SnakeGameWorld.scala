package snake

case class SnakeGameWorld(snake: Snake, board: Board, food: Option[Food]) {
  def play(direction: Option[Direction]): SnakeGameWorld = {
    val newSnake = direction match {
      case Some(newDirection) if snake.validateDirection(newDirection) => snake.copy(direction = newDirection)
      case _ => snake
    }
    val moveForward: Snake = newSnake.forward()
    this.copy(snake = moveForward)
  }

}

case class Snake(location: List[Location], length: Int, direction: Direction) {
  def forward(): Snake = {
    val snakeHead = location.head
    val newHead = direction match {
      case Up => snakeHead.copy(y = snakeHead.y + 1)
      case Down => snakeHead.copy(y = snakeHead.y - 1)
      case Left => snakeHead.copy(x = snakeHead.x - 1)
      case Right => snakeHead.copy(x = snakeHead.x + 1)
    }
    val newSnakeLocation = newHead :: location
    this.copy(location = newSnakeLocation.take(length))
  }
  def validateDirection(newDirection: Direction): Boolean = direction match {
    case Up | Down => newDirection == Left || newDirection == Right
    case Left | Right => newDirection == Up || newDirection == Down
  }
}
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
object Direction {
  def fromStr(directionStr: String): Option[Direction] = directionStr match {
    case "Up" => Some(Up)
    case "Down" => Some(Down)
    case "Left" => Some(Left)
    case "Right" => Some(Right)
    case _ => None
  }
}

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