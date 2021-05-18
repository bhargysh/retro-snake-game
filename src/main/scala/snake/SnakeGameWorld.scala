package snake

import cats.Monad
import cats.implicits._

import scala.util.Random

case class PlayState[F[_]](playing: Boolean, food: Food, snake: Snake, obstacles: Set[Location], foodGenerator: FoodGenerator[F], obstacleGenerator: ObstacleGenerator) //TODO: foodgenerator on its own, maybe in the reader instead?

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
                          obstacleGenerator: ObstacleGenerator
                         ) {

  def play[F[_]: Monad](direction: Option[Direction])(implicit foodGenerator: FoodGenerator[F]): F[SnakeGameWorld] = { //TODO: work on passing food generator another way...
    val boardActionHelper = new BoardActionHelper[F]

    import boardActionHelper._

    val initialPlayState = PlayState[F](isPlaying, food, snake, obstacles, foodGenerator, obstacleGenerator)
    val actionRunner: ActionRunner[BoardAction, Play] = new ActionRunner[BoardAction, Play](_.execute)
    val updateGameState: F[SnakeGameWorld] =
      actionRunner
        .go(Vector(StartTurn(direction)))
        .run((board, moveNumber))
        .run(initialPlayState)
        .map {
          case (playState, _) => SnakeGameWorld(
            playState.snake, playState.obstacles, board, playState.food, playState.playing, moveNumber + 1, obstacleGenerator
          )
        }
    updateGameState
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
    val obstacleGenerator: RandomObstacleGenerator = new RandomObstacleGenerator(new Random())

    new SnakeGameWorld(snake, obstacles, board, food, isPlaying, MoveNumber(0), obstacleGenerator)
  }
}