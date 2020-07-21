package snake

import cats.data.State

import scala.util.Random

case class PlayState(actions: Vector[FoodAction], playing: Boolean, food: Food, snake: Snake)

case class MoveNumber(number: Int) {
  def +(increment: Int): MoveNumber = MoveNumber(number + increment)
  def -(other: MoveNumber): Int = number - other.number
}
// TODO: use the reader

object MoveNumber {
  implicit val orderingMoveNumber: Ordering[MoveNumber] = Ordering[Int].on((n: MoveNumber) => n.number)
}

case class SnakeGameWorld(snake: Snake, board: Board, food: Food, isPlaying: Boolean, moveNumber: MoveNumber) {
  private val foodGenerator: FoodGenerator = new FoodGenerator(new Random())

  def play(direction: Option[Direction]): SnakeGameWorld = {
    type Play[A] = State[PlayState, A] //State[(PlayState, movenumber), A]
    val playInterpreter = new Interpreter[Wrap, Play] { //what we want to do is Wrap and hwo we execute is Play
      override def interpret[A](wrappedA: Wrap[A]): Play[A] = {
        def help(foodAction: FoodAction): Play[Unit] = {
          foodAction match {
            case GrowSnake => State.modify[PlayState](playState => playState.copy(snake = playState.snake.copy(length = playState.snake.length + 1)))
            case AddFood => State.modify[PlayState](_.copy(food = FoodAbsent(turns = moveNumber + 10)))
            case FoodReady => State.modify[PlayState](playState => playState.copy(food = foodGenerator.apply(moveNumber, playState.snake, board)))
            case MovedSnake(newSnake) => for {
              oldActions <- State.inspect[PlayState, Vector[FoodAction]](_.actions)
              newActions = food.eat(newSnake.location.head, moveNumber)
              playing <- State.modify[PlayState](playState => playState.copy(playing = isPlayingCurrently(newSnake), snake = newSnake, actions = oldActions ++ newActions))
            } yield ()
          }
        }
        wrappedA match {
          case FoodActionWrapper(foodAction) => help(foodAction)
          case FoodActionWrapper2(wrapOfA, f) => interpret(wrapOfA).map(f)
        }
      }
    }
    val vectorAction: Vector[FoodAction] = snake.move(direction)
    val initialPlayState = PlayState(vectorAction, isPlaying, food, snake)

    def popAction(): Play[Option[FoodAction]] = for {
      oldActions <- State.inspect[PlayState, Vector[FoodAction]](_.actions)
      x <- oldActions match {
        case head+:tail => State.modify[PlayState](playState => playState.copy(actions = tail)).map(_ => Some(head))
        case _ => State.pure[PlayState, Option[FoodAction]](None) //empty vector of food action
      }
    } yield x

    def go(): Play[Unit] = {
      popAction().flatMap {
        case None => State.pure[PlayState, Unit](())
        case Some(foodAction) =>
          playInterpreter.interpret(FoodActionWrapper(foodAction)).flatMap(_ => go())
      }
    }

    val (evaluatedState, _) = go().run(initialPlayState).value
    SnakeGameWorld(evaluatedState.snake, board, evaluatedState.food, evaluatedState.playing, moveNumber + 1)

    // TODO: try to run the program without the interpreter
    // TODO: test the play function
  }

  def isPlayingCurrently(snake: Snake): Boolean = {
    val snakeHead = snake.location.head
    !board.isWall(snakeHead)
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
  def cellIndex(x: Int, y: Int): Int = {
    x + y * width
  }
  def locations: Set[Location] = {
    Range(0, height).flatMap { y =>
      Range(0, width).map { x =>
          Location(x, y)
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
  def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[FoodAction]
}
case class FoodPresent(location: Location, expiryTime: MoveNumber) extends Food {
  override def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[FoodAction] = {
    if (location == snakeHead) {
      Vector(AddFood, GrowSnake)
    } else if (moveNumber == expiryTime) {
      Vector(AddFood)
    } else {
      Vector.empty
    }
  }
}
case class FoodAbsent(turns: MoveNumber) extends Food {
  override def eat(snakeHead: Location, moveNumber: MoveNumber): Vector[FoodAction] = {
    if(turns == moveNumber) { //TODO: revisit this logic, similar to FoodPresent eat()
      Vector(FoodReady)
    } else {
      Vector.empty
    }
  }
}

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
  val board: Board = Board(emptyCells, 10, 10)

  private val snake = Snake(List(Location(5, 5), Location(5,4)), 4, Up)

  val food: Food = FoodPresent(Location(2,3), MoveNumber(20))
  val isPlaying: Boolean = true

  def newSnakeGameWorld: SnakeGameWorld = {
    new SnakeGameWorld(snake, board, food, isPlaying, MoveNumber(0))
  }
}