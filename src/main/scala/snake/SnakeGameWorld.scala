package snake

import scala.util.Random

case class SnakeGameWorld(snake: Snake, board: Board, food: Food, isPlaying: Boolean, moveNumber: Int) {
  private val foodGenerator: FoodGenerator = new FoodGenerator(new Random())

  def play(direction: Option[Direction]): SnakeGameWorld = {
//    val turnedSnake = direction match {
//      case Some(newDirection) => snake.turn(newDirection)
//      case _ => snake
//    }
//    val movedSnake: Snake = turnedSnake.forward()
//    val snakeHead = movedSnake.location.head
//    val stillPlaying = !board.isWall(snakeHead)

    val vectorAction: Vector[FoodAction] = snake.move(direction)
    val vectorFoodAction = food.eat(snake.location.head, moveNumber) //TODO: should be updated snake not 'snake', same for line 21
    val actions = vectorAction ++ vectorFoodAction
//    fourth action

    val (stillPlaying, updatedFood, updatedSnake): (Boolean, Food, Snake) = actions.foldLeft((isPlaying, food, snake)) { (state, action) =>
        val (p, f, s, a) = state
//      if the action is movedsnake, do food.eat()?
        if (p) {
          action match {
            case AddFood => (p, FoodAbsent(turns = moveNumber + 10), s)
            case FoodReady => (p, foodGenerator.apply(moveNumber, snake, board), s)
            case GrowSnake => (p, f, s.copy(length = s.length + 1))
            case MovedSnake(updatedSnake) => (isPlayingCurrently(updatedSnake), f, updatedSnake)
          }
        }
        else {
          state
        }
      }

    this.copy(
      snake = updatedSnake,
      isPlaying = stillPlaying,
      moveNumber = moveNumber + 1,
      food = updatedFood)
  }

  def isPlayingCurrently(snake: Snake): Boolean = {
    val snakeHead = snake.location.head
    !board.isWall(snakeHead)
  }

  //TODO: think about play method, logic is a bit hidden
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

  def turn(newDirection: Direction): Snake = {
    if (validateDirection(newDirection)) this.copy(direction = newDirection)
    else this
  }

  private def validateDirection(newDirection: Direction): Boolean = direction match {
    case Up | Down => newDirection == Left || newDirection == Right
    case Left | Right => newDirection == Up || newDirection == Down
  }

  def move(direction: Option[Direction]): Vector[FoodAction] = {
    val turnedSnake = direction match {
      case Some(newDirection) => turn(newDirection)
      case _ => this
    }
    val movedSnake: Snake = turnedSnake.forward()
    Vector(MovedSnake(movedSnake))
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
  def isWall(location: Location): Boolean = {
    this.cellAt(location) == Wall
  }
}

sealed trait Cell
case object SnakePart extends Cell
case object Wall extends Cell
case object EmptyCell extends Cell
case object FoodCell extends Cell

sealed trait Food {
  def eat(snakeHead: Location, moveNumber: Int): Vector[FoodAction]
}
case class FoodPresent(location: Location, expiryTime: Int) extends Food {
  override def eat(snakeHead: Location, moveNumber: Int): Vector[FoodAction] = {
    if (location == snakeHead) {
      Vector(AddFood, GrowSnake)
    } else if (moveNumber == expiryTime) {
      Vector(AddFood)
    } else {
      Vector.empty
    }
  }
}
case class FoodAbsent(turns: Int) extends Food {
  override def eat(snakeHead: Location, moveNumber: Int): Vector[FoodAction] = {
    if(turns == moveNumber) { //TODO: revisit this logic, similar to FoodPresent eat()
      Vector(FoodReady)
    } else {
      Vector.empty
    }
  }
}

sealed trait FoodAction
case object GrowSnake extends FoodAction
case object AddFood extends FoodAction
case object FoodReady extends FoodAction
case class MovedSnake(newSnake: Snake) extends FoodAction

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

  private val snake = Snake(List(Location(5, 5), Location(5,4)), 4, Up)

  val food: Food = FoodPresent(Location(2,3), 20)
  val isPlaying: Boolean = true

  def newSnakeGameWorld = {
    new SnakeGameWorld(snake, board, food, isPlaying, moveNumber = 0)
  }
}