package snake.components

import snake.{TurnState, SnakeGameWorld}

trait PlayStateFixtures {
  def modifyPlayState(f: TurnState => TurnState): SnakeGameWorld = SnakeGameWorld.newSnakeGameWorld.copy(playState = f(SnakeGameWorld.newSnakeGameWorld.playState))
}
