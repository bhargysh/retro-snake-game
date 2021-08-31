package snake

import cats.effect.Sync
import cats.implicits._

class GameStep[F[_]: Sync]( //TODO: what is F, Play or Game?
  getInput: F[Option[Direction]],
  renderView: SnakeGameWorld => F[Unit],
  playTurn: (SnakeGameWorld, Option[Direction]) => F[SnakeGameWorld]) {

  def updateGame(snakeGameWorld: SnakeGameWorld): F[Option[SnakeGameWorld]] = for {
    maybeDirection    <- getInput
    _                 <- Sync[F].delay(println(s"update game-------${snakeGameWorld.food}"))
    newSnakeGameWorld <- playTurn(snakeGameWorld, maybeDirection)
    _                 <- renderView(newSnakeGameWorld)
    result =
      if (newSnakeGameWorld.isPlaying) {
        Some(newSnakeGameWorld)
      } else {
        None
      }
    _ <- Sync[F].delay(println(s"after updated game-------${result.map(_.food)}"))
  } yield result
}
