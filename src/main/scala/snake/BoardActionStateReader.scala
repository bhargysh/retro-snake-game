package snake

import cats.data.{ReaderT, StateT}
import cats.effect.IO

trait BoardActionStateReader[F[_]] {

  def modifyState(f: PlayState => PlayState): F[Unit]

  def inspectState[S](f: PlayState => S): F[S]

  def askForBoard: F[Board]

  def askForMoveNumber: F[MoveNumber]

  def generateFood: F[FoodPresent]

}

class BoardActionStateReaderImpl(foodGenerator: FoodGenerator[IO])
  extends BoardActionStateReader[ReaderT[StateT[IO, PlayState, *], (Board, MoveNumber), *]] {

  def modifyState(f: PlayState => PlayState): Play[Unit] = {
    ReaderT.liftF[P, PlayEnv, Unit](StateT.modify[IO, PlayState](f))
  }

  def inspectState[S](f: PlayState => S): Play[S] = {
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
    newFood <- ReaderT.liftF[P, PlayEnv, FoodPresent](StateT.liftF[IO, PlayState, FoodPresent](foodGenerator.apply(moveNumber, snake, board, obstacles)))
  } yield newFood
}