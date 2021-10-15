package snake

import cats.data.{ReaderT, StateT}
import cats.effect.IO

trait BoardActionStateReader[F[_]] {

  def modifyState(f: TurnState => TurnState): F[Unit]

  def inspectState[S](f: TurnState => S): F[S]

  def askForBoard: F[Board]

  def askForMoveNumber: F[MoveNumber]

  def generateFood: F[FoodPresent]

  def generateObstacle: F[Location]

}

class BoardActionStateReaderImpl(foodGenerator: FoodGenerator[IO], obstacleGenerator: ObstacleGenerator[IO])
  extends BoardActionStateReader[ReaderT[StateT[IO, TurnState, *], (Board, MoveNumber), *]] {

  def modifyState(f: TurnState => TurnState): Play[Unit] = {
    ReaderT.liftF[P, PlayEnv, Unit](StateT.modify[IO, TurnState](f))
  }

  def inspectState[S](f: TurnState => S): Play[S] = {
    ReaderT.liftF[P, PlayEnv, S](StateT.inspect(f))
  }

  def askForBoard: Play[Board] = for {
    x <- ReaderT.ask[P, PlayEnv]
    (board, _) = x
  } yield board

  def askForMoveNumber: Play[MoveNumber] = for {
    x <- ReaderT.ask[P, PlayEnv]
    (_, moveNumber) = x
  } yield moveNumber

  def generateFood: Play[FoodPresent] = for {
    moveNumber <- askForMoveNumber
    snake <- inspectState(_.snake)
    board <- askForBoard
    obstacles <- inspectState(_.obstacles)
    newFood <- ReaderT.liftF[P, PlayEnv, FoodPresent](StateT.liftF[IO, TurnState, FoodPresent](foodGenerator.apply(moveNumber, snake, board, obstacles)))
  } yield newFood

  def generateObstacle: Play[Location] = for {
    snake <- inspectState(_.snake)
    board <- askForBoard
    food <- inspectState(_.food)
    currentObstacles <- inspectState(_.obstacles)
    newObstacle <- ReaderT.liftF[P, PlayEnv, Location](StateT.liftF[IO, TurnState, Location](obstacleGenerator.apply(food, snake, board, currentObstacles)))
  } yield newObstacle
}