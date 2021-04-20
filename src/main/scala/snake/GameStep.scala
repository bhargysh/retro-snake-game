package snake

import cats.Id
import cats.effect.IO

class GameStep(getInput: IO[Option[Direction]], renderView: SnakeGameWorld => IO[Unit])(implicit foodGenerator: FoodGenerator[Id]) {

  def updateGame(snakeGameWorld: SnakeGameWorld): IO[Option[SnakeGameWorld]] = for {
    maybeDirection <- getInput
    newSnakeGameWorld = snakeGameWorld.play(maybeDirection)
    _ <- renderView(newSnakeGameWorld)
    result = if (newSnakeGameWorld.isPlaying) {
      Some(newSnakeGameWorld)
    } else {
      None
    }
  } yield result
}
