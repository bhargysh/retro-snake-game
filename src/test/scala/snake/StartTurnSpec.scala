package snake

import org.specs2.mutable.Specification

import scala.util.Random

class StartTurnSpec extends Specification {
  "Start turn action" should {
    "returns a moved snake" in {
      val initialLocation = List(Location(8, 8))
      val initialState = PlayState(
        playing = true,
        FoodPresent(Location(2,3), MoveNumber(10)),
        Snake(initialLocation, 3, Left),
        SnakeGameWorld.obstacles,
        new RandomFoodGenerator(new Random)
      )
      val direction = Some(Down)
      val newSnake = Snake(List(Location(8, 7), Location(8, 8)), 3, Down)

      val (playState, foodActions) = StartTurn(direction)
        .execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value
      val movedSnake: BoardAction = MovedSnake(newSnake)

      foodActions should contain(exactly(movedSnake))
      playState shouldEqual initialState
    }
  }
}
