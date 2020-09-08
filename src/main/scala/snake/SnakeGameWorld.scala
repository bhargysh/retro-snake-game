package snake

import cats.data.ReaderT
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

    val vectorAction: Vector[FoodAction] = snake.move(direction)

    def go(foodActions: Vector[FoodAction]): Play[Unit] = {
      foodActions match {
        case Vector() => ReaderT.pure[P, E, Unit](())
        case foodAction +: oldActions => for {
          newActions <- foodAction.execute
          _ <- go(oldActions ++ newActions)
        } yield ()
      }
    }

    val initialPlayState = (PlayState(isPlaying, food, snake, foodGenerator), moveNumber)

    def toNewGlobalState(oldGlobalState: (PlayState, MoveNumber), finalLocalState: PlayState): (PlayState, MoveNumber) = {
      (finalLocalState, oldGlobalState._2)
    }

    val updateGameState =
      go(vectorAction).run((board, moveNumber)).transformS[(PlayState, MoveNumber)](_._1, toNewGlobalState)

    val ((playState, move), ()) = updateGameState.run(initialPlayState).value
    SnakeGameWorld(playState.snake, board, playState.food, playState.playing, move + 1)


    // TODO: test the play function
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