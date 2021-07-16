package snake

import org.specs2.mutable.Specification

class StartTurnSpec extends Specification with BoardActionFixtures {

  "Start turn action" should {
    "returns a moved snake" in {
      val initialLocation = List(Location(8, 8))
      val initialState = initialPlayState.copy(snake = Snake(initialLocation, 3, Left))
      val direction = Some(Down)
      val newSnake = Snake(List(Location(8, 7), Location(8, 8)), 3, Down)

      val (playState, foodActions) = StartTurn(direction)
        .execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .unsafeRunSync()

      val movedSnake: BoardAction = MovedSnake(newSnake)

      foodActions should contain(exactly(movedSnake))
      playState shouldEqual initialState
    }
  }
}
