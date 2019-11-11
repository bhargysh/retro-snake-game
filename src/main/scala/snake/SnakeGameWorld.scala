package snake

import scala.util.Random

case class SnakeGameWorld(snake: Snake, board: Board, food: Food, isPlaying: Boolean, moveNumber: Int) {
  private val showFood: FoodGenerator = new FoodGenerator(new Random())
  def play(direction: Option[Direction]): SnakeGameWorld = {
    val turnedSnake = direction match {
      case Some(newDirection) if snake.validateDirection(newDirection) => snake.copy(direction = newDirection)
      case _ => snake
    }

    val movedSnake: Snake = turnedSnake.forward()
    val snakeHead = movedSnake.location.head
    def isWall(location: Location): Boolean = {
      board.cellAt(location) == Wall
    }
    val stillPlaying = !isWall(snakeHead)
    val (updatedFood, updatedSnake): (Food, Snake) = if (stillPlaying) {
      food match {
        case FoodPresent(location, expiryTime) => if (location == movedSnake.location.head) {
          (FoodAbsent(turns = moveNumber + 10), movedSnake.copy(length = movedSnake.length + 1))
        } else if (moveNumber == expiryTime) {
          (FoodAbsent(turns = moveNumber + 10), movedSnake)
        } else {
          (food, movedSnake)
        }
        case FoodAbsent(turns) => if(turns == moveNumber) {
          (showFood.apply(moveNumber, movedSnake, board), movedSnake)
        } else {
          (food, movedSnake)
        }
      }
    } else {
      (food, movedSnake)
    }

    this.copy(snake = updatedSnake, isPlaying = stillPlaying, moveNumber = moveNumber + 1, food = updatedFood)
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
case class Board(cell: Array[Cell], width: Int, height: Int) {
  def cellAt(location: Location): Cell = {
    cell(cellIndex(location.x, location.y))
  }
  def cellIndex(x: Int, y: Int) = {
    x + y * width
  }
  def locations: Set[Location] = {
    Range(0, height).flatMap { y =>
      Range(0, width).map { x =>
          (Location(x, y))
      }
    }.toSet
  }
}

sealed trait Cell
case object SnakePart extends Cell
case object Wall extends Cell
case object EmptyCell extends Cell

case object FoodCell extends Cell
sealed trait Food
case class FoodPresent(location: Location, expiryTime: Int) extends Food
case class FoodAbsent(turns: Int) extends Food

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

  private val snake = Snake(List(Location(5, 5), Location(5,4)), 4, Up) //

  val food: Food = FoodPresent(Location(2,3), 20)
  val isPlaying: Boolean = true

  def newSnakeGameWorld = {
    new SnakeGameWorld(snake, board, food, isPlaying, moveNumber = 0)
  }
}