package snake.components

import snake.{PlayState, SnakeGameWorld}

trait PlayStateFixtures {
  def modifyPlayState(f: PlayState => PlayState): SnakeGameWorld = SnakeGameWorld.newSnakeGameWorld.copy(playState = f(SnakeGameWorld.newSnakeGameWorld.playState))
}
