package snake

import cats.data.{ReaderT, State}
import cats.implicits._

import scala.util.Random

case class PlayState(actions: Vector[FoodAction], playing: Boolean, food: Food, snake: Snake)

case class MoveNumber(number: Int) {
  def +(increment: Int): MoveNumber = MoveNumber(number + increment)
  def -(other: MoveNumber): Int = number - other.number
}
// TODO: *really* use the reader

object MoveNumber {
  implicit val orderingMoveNumber: Ordering[MoveNumber] = Ordering[Int].on((n: MoveNumber) => n.number)
}

case class SnakeGameWorld(snake: Snake, board: Board, food: Food, isPlaying: Boolean, moveNumber: MoveNumber) {
  private val foodGenerator: FoodGenerator = new FoodGenerator(new Random())

  def play(direction: Option[Direction]): SnakeGameWorld = {
    type Play[A] = ReaderT[P, Board, A]
    type P[A] = State[PlayState, A]

    def modifyState(f: PlayState => PlayState): Play[Unit] = {
      ReaderT.liftF[P, Board, Unit](State.modify[PlayState](f))
//          F[_] -> underlying monad
//          A -> extram params it needs
//          B -> return type
    }

    def inspectState[S](f: PlayState => S): Play[S] = {
      ReaderT.liftF[P, Board, S](State.inspect(f))
    }

    val playInterpreter = new Interpreter[Wrap, Play] { //what we want to do is Wrap and hwo we execute is Play
      override def interpret[A](wrappedA: Wrap[A]): Play[A] = {
        def help(foodAction: FoodAction): Play[Unit] = {
          foodAction match {
            case GrowSnake => modifyState(playState => playState.copy(snake = playState.snake.copy(length = playState.snake.length + 1)))
            case AddFood => modifyState(_.copy(food = FoodAbsent(turns = moveNumber + 10)))
            case FoodReady => modifyState(playState => playState.copy(food = foodGenerator.apply(moveNumber, playState.snake, board)))
            case MovedSnake(newSnake) => for {
              oldActions <- inspectState(_.actions)
              newActions = food.eat(newSnake.location.head, moveNumber)
              playing <- modifyState(playState => playState.copy(playing = isPlayingCurrently(newSnake), snake = newSnake, actions = oldActions ++ newActions))
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

    def popAction(): Play[Option[FoodAction]] = for {
      oldActions <- inspectState(_.actions)
      x <- oldActions match {
        case head+:tail => modifyState(playState => playState.copy(actions = tail)).map(_ => Some(head))
        case _ => ReaderT.pure[P, Board, Option[FoodAction]](None) //empty vector of food action
      }
    } yield x

    def go(): Play[Unit] = {
      popAction().flatMap {
        case None => ReaderT.pure[P, Board, Unit](())
        case Some(foodAction) =>
          playInterpreter.interpret(FoodActionWrapper(foodAction)).flatMap(_ => go())
      }
    }

    val initialPlayState = (PlayState(vectorAction, isPlaying, food, snake), moveNumber)

    def toNewGlobalState(oldGlobalState: (PlayState, MoveNumber), finalLocalState: PlayState): (PlayState, MoveNumber) = {
      (finalLocalState, oldGlobalState._2)
    }

    val updateGameState =
      go().run(board).transformS[(PlayState, MoveNumber)](_._1, toNewGlobalState)
    val ((playState, move), ()) = updateGameState.run(initialPlayState).value
    SnakeGameWorld(playState.snake, board, playState.food, playState.playing, move + 1)

    // TODO: try to run the program without the interpreter
    // TODO: test the play function
  }

  def isPlayingCurrently(snake: Snake): Boolean = {
    val snakeHead = snake.location.head
    !board.isWall(snakeHead)
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