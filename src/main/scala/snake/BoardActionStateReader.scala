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

  def modifyState(f: TurnState => TurnState): Turn[Unit] = {
    ReaderT.liftF[P, TurnEnv, Unit](StateT.modify[IO, TurnState](f))
  }

  def inspectState[S](f: TurnState => S): Turn[S] = {
    ReaderT.liftF[P, TurnEnv, S](StateT.inspect(f))
  }

  def askForBoard: Turn[Board] = for {
    x <- ReaderT.ask[P, TurnEnv]
    (board, _) = x
  } yield board

  def askForMoveNumber: Turn[MoveNumber] = for {
    x <- ReaderT.ask[P, TurnEnv]
    (_, moveNumber) = x
  } yield moveNumber

  def generateFood: Turn[FoodPresent] = for {
    moveNumber <- askForMoveNumber
    snake <- inspectState(_.snake)
    board <- askForBoard
    obstacles <- inspectState(_.obstacles)
    newFood <- ReaderT.liftF[P, TurnEnv, FoodPresent](StateT.liftF[IO, TurnState, FoodPresent](foodGenerator.apply(moveNumber, snake, board, obstacles)))
  } yield newFood

  def generateObstacle: Turn[Location] = for {
    snake <- inspectState(_.snake)
    board <- askForBoard
    food <- inspectState(_.food)
    currentObstacles <- inspectState(_.obstacles)
    newObstacle <- ReaderT.liftF[P, TurnEnv, Location](StateT.liftF[IO, TurnState, Location](obstacleGenerator.apply(food, snake, board, currentObstacles)))
  } yield newObstacle
}