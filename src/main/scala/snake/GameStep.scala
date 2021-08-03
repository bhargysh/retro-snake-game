package snake

import cats.effect.Sync
import cats.implicits._

class GameStep[F[_]: Sync: BoardActionStateReader](getInput: F[Option[Direction]], renderView: SnakeGameWorld => F[Unit]) {

  def updateGame(snakeGameWorld: SnakeGameWorld): F[Option[SnakeGameWorld]] = for {
    maybeDirection <- getInput
    _ <- Sync[F].delay(println(s"update game dir-------$maybeDirection"))
    newSnakeGameWorld <- snakeGameWorld.play[F](maybeDirection)
    _ <- renderView(newSnakeGameWorld)
    result = if (newSnakeGameWorld.isPlaying) {
      Some(newSnakeGameWorld)
    } else {
      None
    }
  } yield result
}
