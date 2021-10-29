package snake

import cats.effect.Sync
import cats.implicits._

class GameStep[F[_]: Sync](
  getInput: F[Option[Direction]],
  renderView: SnakeGameWorld => F[Unit],
  playTurn: (SnakeGameWorld, Option[Direction]) => F[SnakeGameWorld]) {

  def updateGame(snakeGameWorld: SnakeGameWorld): F[Option[SnakeGameWorld]] = for {
    maybeDirection    <- getInput
    newSnakeGameWorld <- playTurn(snakeGameWorld, maybeDirection)
    _                 <- renderView(newSnakeGameWorld)
    result =
      if (newSnakeGameWorld.turnState.playing) {
        Some(newSnakeGameWorld)
      } else {
        None
      }
  } yield result
}
