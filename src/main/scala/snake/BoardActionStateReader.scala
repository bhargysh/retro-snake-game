package snake

import cats.Monad
import cats.data.{ReaderT, StateT}

trait BoardActionStateReader[F[_]] {

  def modifyState(f: PlayState => PlayState): F[Unit]

  def inspectState[S](f: PlayState => S): F[S]

  def askForBoard: F[Board]

  def askForMoveNumber: F[MoveNumber]

  def generateFood: F[FoodPresent]

}

class BoardActionStateReaderImpl[F[_]: Monad](foodGenerator: FoodGenerator[F])
  extends BoardActionStateReader[ReaderT[StateT[F, PlayState, *], (Board, MoveNumber), *]] {

  type E = (Board, MoveNumber)
  type P[A] = StateT[F, PlayState, A]
  type Play[A] = ReaderT[P, E, A]

  def modifyState(f: PlayState => PlayState): Play[Unit] = {
    ReaderT.liftF[P, E, Unit](StateT.modify[F, PlayState](f))
  }

  def inspectState[S](f: PlayState => S): Play[S] = {
    ReaderT.liftF[P, E, S](StateT.inspect(f))
  }

  def askForBoard: Play[Board] = for {
    x <- ReaderT.ask[P, E]
    (board, _) = x
  } yield board

  def askForMoveNumber: Play[MoveNumber] = for {
    x <- ReaderT.ask[P, E]
    (_, moveNumber) = x
  } yield moveNumber

  def generateFood: Play[FoodPresent] = for {
    moveNumber <- askForMoveNumber
    snake <- inspectState(_.snake)
    board <- askForBoard
    obstacles <- inspectState(_.obstacles)
    newFood <- ReaderT.liftF[P, E, FoodPresent](StateT.liftF[F, PlayState, FoodPresent](foodGenerator.apply(moveNumber, snake, board, obstacles)))
  } yield newFood
}