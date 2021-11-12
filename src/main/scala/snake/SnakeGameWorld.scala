package snake

import cats.Monad
import cats.implicits._

case class TurnState(playing: Boolean, food: Food, snake: Snake, obstacles: Set[Location])

case class MoveNumber(number: Int) {
  def +(increment: Int): MoveNumber = MoveNumber(number + increment)
  def -(other: MoveNumber): Int = number - other.number
}

//SNG provides data to the renderer and calls action runner --> code smell, two responsibilities
//TODO: Can we refactor play() to be a part of toGameSTate() instead i.e. pass direction to toGameSTate()?
case class SnakeGameWorld(board: Board,
                          moveNumber: MoveNumber,
                          turnState: TurnState) {
  def play[F[_]: Monad](direction: Option[Direction])(implicit actionRunner: ActionRunner[F, BoardAction]): F[Unit] = {
    actionRunner.runActions(Vector(StartTurn(direction)))
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
    new SnakeGameWorld(board, MoveNumber(0), TurnState(isPlaying, food, snake, obstacles))
  }
}