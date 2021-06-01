package snake

import cats.Monad
import cats.implicits._

class GameStep[F[_]: Monad: BoardActionStateReader](getInput: F[Option[Direction]], renderView: SnakeGameWorld => F[Unit]) {

  def updateGame(snakeGameWorld: SnakeGameWorld): F[Option[SnakeGameWorld]] = for {
    maybeDirection <- getInput
    newSnakeGameWorld <- snakeGameWorld.play[F](maybeDirection)
    _ <- renderView(newSnakeGameWorld)
    result = if (newSnakeGameWorld.isPlaying) {
      Some(newSnakeGameWorld)
    } else {
      None
    }
  } yield result
}
