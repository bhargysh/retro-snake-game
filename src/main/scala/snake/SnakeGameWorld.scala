package snake

import cats.data.State
import cats.implicits._

import scala.util.Random

case class PlayState(playing: Boolean, food: Food, snake: Snake, foodGenerator: FoodGenerator)

case class MoveNumber(number: Int) {
  def +(increment: Int): MoveNumber = MoveNumber(number + increment)
  def -(other: MoveNumber): Int = number - other.number
}

object MoveNumber {
  implicit val orderingMoveNumber: Ordering[MoveNumber] = Ordering[Int].on((n: MoveNumber) => n.number)
}

case class SnakeGameWorld(snake: Snake, board: Board, food: Food, isPlaying: Boolean, moveNumber: MoveNumber) {
  private val foodGenerator: RandomFoodGenerator = new RandomFoodGenerator(new Random())

  def play(direction: Option[Direction]): SnakeGameWorld = {
    import FoodAction._

    val initialPlayState = (PlayState(isPlaying, food, snake, foodGenerator), moveNumber)

    def toNewGlobalState(oldGlobalState: (PlayState, MoveNumber), finalLocalState: PlayState): (PlayState, MoveNumber) = {
      (finalLocalState, oldGlobalState._2)
    }

    val actionRunner = new ActionRunner[FoodAction, Play](_.execute)

    val movedSnake: Snake = snake.move(direction)

    val updateGameState: State[(PlayState, MoveNumber), Unit] =
      actionRunner
        .go(Vector(MovedSnake(movedSnake)))
        .run((board, moveNumber))
        .transformS[(PlayState, MoveNumber)](_._1, toNewGlobalState) //TODO: can we make this nicer?

    val ((playState, move), ()) =
      updateGameState
        .run(initialPlayState)
        .value

    SnakeGameWorld(playState.snake, board, playState.food, playState.playing, move + 1)


    // TODO: test the play function
//    TODO: why can't we use action runner to run everything?!
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