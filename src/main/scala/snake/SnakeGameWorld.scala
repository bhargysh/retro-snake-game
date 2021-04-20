package snake

import cats.data.State
import cats.implicits._
import BoardAction._
import cats.Id

import scala.util.Random

case class PlayState(playing: Boolean, food: Food, snake: Snake, obstacles: Set[Location], foodGenerator: FoodGenerator[Id], obstacleGenerator: ObstacleGenerator) //TODO: foodgenerator on its own?

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
                          moveNumber: MoveNumber,
                          actionRunner: ActionRunner[BoardAction, Play],
                          obstacleGenerator: ObstacleGenerator
                         ) {

  def play(direction: Option[Direction])(implicit foodGenerator: FoodGenerator[Id]): SnakeGameWorld = {

    val initialPlayState = PlayState(isPlaying, food, snake, obstacles, foodGenerator, obstacleGenerator)

    val updateGameState: State[PlayState, Unit] =
      actionRunner
        .go(Vector(StartTurn(direction)))
        .run((board, moveNumber))

    val (playState, ()) =
      updateGameState
        .run(initialPlayState)
        .value

    SnakeGameWorld(
      playState.snake, playState.obstacles, board, playState.food, playState.playing, moveNumber + 1, actionRunner, obstacleGenerator
    ) //TODO: think about this model
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
    val actionRunner = new ActionRunner[BoardAction, Play](_.execute)
    val obstacleGenerator: RandomObstacleGenerator = new RandomObstacleGenerator(new Random())

    new SnakeGameWorld(snake, obstacles, board, food, isPlaying, MoveNumber(0), actionRunner, obstacleGenerator)
  }
}