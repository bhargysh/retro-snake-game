package snake

import cats.Monad
import cats.implicits._

sealed trait BoardAction {
  def execute[F[_]: Monad](implicit F: BoardActionStateReader[F]): F[Vector[BoardAction]]
}

case object GrowSnake extends BoardAction {
  def execute[F[_]: Monad](implicit F: BoardActionStateReader[F]): F[Vector[BoardAction]] = for {
    _ <- F.modifyState(playState => playState.copy(snake = playState.snake.copy(length = playState.snake.length + 1)))
  } yield Vector.empty[BoardAction]
}

case object AddFood extends BoardAction {
  def execute[F[_]: Monad](implicit F: BoardActionStateReader[F]): F[Vector[BoardAction]] = for {
    moveNumber <- F.askForMoveNumber
    _ <- F.modifyState(_.copy(food = FoodAbsent(turns = moveNumber + 10)))
  } yield Vector.empty[BoardAction]
}

case object FoodReady extends BoardAction {
  def execute[F[_]: Monad](implicit F: BoardActionStateReader[F]) = for {
    newFood <- F.generateFood
    _ <- F.modifyState(playState => playState.copy(food = newFood))
  } yield Vector.empty[BoardAction]
}
  case class MovedSnake(newSnake: Snake) extends BoardAction {
    def execute[F[_]: Monad](implicit F: BoardActionStateReader[F]) = {
    def isPlayingCurrently(snake: Snake): F[Boolean] = for {
      board <- F.askForBoard
      obstacles <- F.inspectState(_.obstacles)
      notWall = !board.isWall(snake.location.head)
      notObstacle = !obstacles.contains(snake.location.head)
    } yield notWall && notObstacle

    def eatFoodPresent(location: Location, expiryTime: MoveNumber, moveNumber: MoveNumber): Vector[BoardAction] = {
      if (location == newSnake.location.head) {
        Vector(AddFood, GrowSnake)
      } else if (moveNumber == expiryTime) {
        Vector(AddFood, AddObstacle)
      } else {
        Vector.empty
      }
    }

    def foodAbsent(turns: MoveNumber, moveNumber: MoveNumber): Vector[BoardAction] = {
      if(turns == moveNumber) {
        Vector(FoodReady)
      } else {
        Vector.empty
      }
    }

      for {
        playing <- isPlayingCurrently(newSnake)
        moveNumber <- F.askForMoveNumber
        food <- F.inspectState(_.food)
        newActions = food match {
          case FoodPresent(location, expiryTime) => eatFoodPresent(location, expiryTime, moveNumber)
          case FoodAbsent(turns) => foodAbsent(turns, moveNumber)
        }
        _ <- F.modifyState(playState => playState.copy(playing = playing, snake = newSnake))
      } yield newActions
    }
  }

case class StartTurn(direction: Option[Direction]) extends BoardAction {
  def execute[F[_]: Monad](implicit F: BoardActionStateReader[F]): F[Vector[BoardAction]] = for {
    oldSnake <- F.inspectState(_.snake)
    movedSnake = oldSnake.move(direction)
  } yield Vector(MovedSnake(movedSnake))
}

case object AddObstacle extends BoardAction {
  def execute[F[_]: Monad](implicit F: BoardActionStateReader[F]): F[Vector[BoardAction]] = for {
    newObstacle <- F.generateObstacle
    _ <- F.modifyState { playState =>
      playState.copy(obstacles = playState.obstacles ++ Set(newObstacle))
    }
  } yield Vector.empty[BoardAction]
}