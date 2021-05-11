package snake

import cats.Monad
import cats.data.{ReaderT, StateT}
import cats.implicits._

class BoardActionHelper[F[_]: Monad]() {
  type E = (Board, MoveNumber)
  type P[A] = StateT[F, PlayState[F], A] //ReaderT[State[PlayState, A], (Board, MoveNumber), A] where A -> Unit
  type Play[A] = ReaderT[P, E, A]

  def modifyState(f: PlayState[F] => PlayState[F]): Play[Unit] = {
    ReaderT.liftF[P, E, Unit](StateT.modify[F, PlayState[F]](f))
    //          F[_] -> underlying monad
    //          A -> extram params it needs
    //          B -> return type
  }

  def inspectState[S](f: PlayState[F] => S): Play[S] = {
    ReaderT.liftF[P, E, S](StateT.inspect(f))
  }

  def modifyStateInF(f: PlayState[F] => F[PlayState[F]]): Play[Unit] = ??? //TODO: implement this lol

  def askForBoard: ReaderT[P, E, Board] = for {
    x <- ReaderT.ask[P, E]
    (board, _) = x
  } yield board

  def askForMoveNumber: ReaderT[P, E, MoveNumber] = for {
    x <- ReaderT.ask[P, E]
    (_, moveNumber) = x
  } yield moveNumber

  sealed trait BoardAction {
    def execute: Play[Vector[BoardAction]]
  }

  case object GrowSnake extends BoardAction {
    def execute: Play[Vector[BoardAction]] = for {
      _ <- modifyState(playState => playState.copy(snake = playState.snake.copy(length = playState.snake.length + 1)))
    } yield Vector.empty[BoardAction]
  }
  case object AddFood extends BoardAction {
    def execute: Play[Vector[BoardAction]] = for {
      moveNumber <- askForMoveNumber
      _ <- modifyState(_.copy(food = FoodAbsent(turns = moveNumber + 10)))
    } yield Vector.empty[BoardAction]
  }
  case object FoodReady extends BoardAction {
    def execute: Play[Vector[BoardAction]] = for {
      board <- askForBoard
      moveNumber <- askForMoveNumber
      _ <- modifyStateInF(playState => for {
        newFood <- playState.foodGenerator.apply(moveNumber, playState.snake, board, playState.obstacles)
      } yield playState.copy(food = newFood)
      )
    } yield Vector.empty[BoardAction]
  }
  case class MovedSnake(newSnake: Snake) extends BoardAction {
    private def isPlayingCurrently(snake: Snake): Play[Boolean] = for {
      board <- askForBoard
      obstacles <- inspectState(_.obstacles)
      notWall = !board.isWall(snake.location.head)
      notObstacle = !obstacles.contains(snake.location.head)
    } yield notWall && notObstacle

    private def eatFoodPresent(location: Location, expiryTime: MoveNumber, moveNumber: MoveNumber): Vector[BoardAction] = {
      if (location == newSnake.location.head) {
        Vector(AddFood, GrowSnake)
      } else if (moveNumber == expiryTime) {
        Vector(AddFood, AddObstacle)
      } else {
        Vector.empty
      }
    }

    private def foodAbsent(turns: MoveNumber, moveNumber: MoveNumber): Vector[BoardAction] = {
      if(turns == moveNumber) {
        Vector(FoodReady)
      } else {
        Vector.empty
      }
    }

    def execute: Play[Vector[BoardAction]] = for {
      playing <- isPlayingCurrently(newSnake)
      moveNumber <- askForMoveNumber
      food <- inspectState(_.food)
      newActions = food match {
        case FoodPresent(location, expiryTime) => eatFoodPresent(location, expiryTime, moveNumber)
        case FoodAbsent(turns) => foodAbsent(turns, moveNumber)
      }
      _ <- modifyState(playState => playState.copy(playing = playing, snake = newSnake))
    } yield newActions
  }

  case class StartTurn(direction: Option[Direction]) extends BoardAction {
    def execute: Play[Vector[BoardAction]] = for {
      oldSnake <- inspectState(_.snake)
      movedSnake = oldSnake.move(direction)
    } yield Vector(MovedSnake(movedSnake))
  }

  case object AddObstacle extends BoardAction { //add obstalce if they miss food
    def execute: Play[Vector[BoardAction]] = for {
      board <- askForBoard
      _ <- modifyState { playState =>
        val newObstacle = playState.obstacleGenerator.apply(playState.food, playState.snake, board, playState.obstacles)
        playState.copy(obstacles = playState.obstacles ++ Set(newObstacle))
      }
    } yield Vector.empty[BoardAction]
  }
}