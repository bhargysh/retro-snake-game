package snake

import cats.Monad
import cats.implicits._

case class PlayState(playing: Boolean, food: Food, snake: Snake, obstacles: Set[Location]) //TODO: rename to TurnState

case class MoveNumber(number: Int) {
  def +(increment: Int): MoveNumber = MoveNumber(number + increment)
  def -(other: MoveNumber): Int = number - other.number
}

object MoveNumber {
  implicit val orderingMoveNumber: Ordering[MoveNumber] = Ordering[Int].on((n: MoveNumber) => n.number)
}

case class SnakeGameWorld(snake: Snake,
                          obstacles: Set[Location],
                          board: Board,
                          food: Food,
                          isPlaying: Boolean,
                          moveNumber: MoveNumber
                         ) {
  // TODO: do we still need to do this in SnakeGameWorld, maybe the move increment can be moved to its own MoveCounter?
  def play[F[_]: Monad](direction: Option[Direction])(implicit boardActionStateReader: BoardActionStateReader[F], actionRunner: ActionRunner[F, BoardAction]): F[SnakeGameWorld] = {
    for {
      _ <- actionRunner.runActions(Vector(StartTurn(direction)))
      board <- boardActionStateReader.askForBoard
      moveNumber <- boardActionStateReader.askForMoveNumber
      newWorld <- boardActionStateReader.inspectState { ps =>
        SnakeGameWorld(ps.snake, ps.obstacles, board, ps.food, ps.playing, moveNumber + 1) //TODO: copy instead
      }
    } yield newWorld

    /*
    val initialPlayState = PlayState(isPlaying, food, snake, obstacles, obstacleGenerator)
    .run((board, moveNumber))
        .run(initialPlayState)
        .map {
          case (playState, _) => SnakeGameWorld(
            playState.snake, playState.obstacles, board, playState.food, playState.playing, moveNumber + 1, obstacleGenerator
          )
        }
     */
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
  val obstacles = Set.empty[Location]

  def newSnakeGameWorld: SnakeGameWorld = {
    new SnakeGameWorld(snake, obstacles, board, food, isPlaying, MoveNumber(0))
  }
}