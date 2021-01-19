package snake

import cats.effect.IO
import org.scalajs.dom.Node

class GameStep(getInput: IO[Option[Direction]], renderView: SnakeGameWorld => IO[Unit]) {

//  TODO: Test GameStep somehow...
//  TODO: Change state to only contain the SnakeGameWorld

  def updateGame(oldState: (Node, SnakeGameWorld)): IO[Option[(Node, SnakeGameWorld)]] = for {
    maybeDirection <- getInput
    (oldRenderedWorld, world) = oldState
    newSnakeGameWorld = world.play(maybeDirection)
    _ = renderView(newSnakeGameWorld)
//    result = if (newSnakeGameWorld.isPlaying) {
//      Some((newRenderedWorld, newSnakeGameWorld))
//    } else {
//      None
//    }
    result = ???
  } yield result
}
