package snake

import cats.data.{ReaderT, State}
import snake.BoardAction._


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
    _ <- modifyState(playState => playState.copy(food = playState.foodGenerator.apply(moveNumber, playState.snake, board)))
  } yield Vector.empty[BoardAction]
}
case class MovedSnake(newSnake: Snake) extends BoardAction {
  private def isPlayingCurrently(snake: Snake): Play[Boolean] = for {
    board <- askForBoard
  } yield !board.isWall(snake.location.head)

  def execute: Play[Vector[BoardAction]] = for {
    playing <- isPlayingCurrently(newSnake)
    moveNumber <- askForMoveNumber
    food <- inspectState(_.food)
    newActions = food.eat(newSnake.location.head, moveNumber)
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
      println(newObstacle, playState.obstacles)
      playState.copy(obstacles = playState.obstacles ++ Set(newObstacle))
    }
  } yield Vector.empty[BoardAction]
}

object BoardAction {
  type E = (Board, MoveNumber)
  type Play[A] = ReaderT[P, E, A]
  type P[A] = State[PlayState, A] //ReaderT[State[PlayState, A], (Board, MoveNumber), A] where A -> Unit

  def modifyState(f: PlayState => PlayState): Play[Unit] = {
    ReaderT.liftF[P, E, Unit](State.modify[PlayState](f))
    //          F[_] -> underlying monad
    //          A -> extram params it needs
    //          B -> return type
  }

  def inspectState[S](f: PlayState => S): Play[S] = {
    ReaderT.liftF[P, E, S](State.inspect(f))
  }

  def askForBoard: ReaderT[P, E, Board] = for {
    x <- ReaderT.ask[P, E]
    (board, _) = x
  } yield board

  def askForMoveNumber: ReaderT[P, E, MoveNumber] = for {
    x <- ReaderT.ask[P, E]
    (_, moveNumber) = x
  } yield moveNumber
}