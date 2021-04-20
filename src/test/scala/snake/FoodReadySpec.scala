package snake

import org.specs2.mutable.Specification

class FoodReadySpec extends Specification with BoardActionFixtures {

  "FoodReady action" should {
    "modify food to be randomly generated in PlayState" in {
      val foodGenerator = new FoodGenerator {
        def apply(moveNumber: MoveNumber, snake: Snake, board: Board, obstacles: Set[Location]): FoodPresent = FoodPresent(Location(2,3), MoveNumber(10))
      }

      val initialState = initialPlayState.copy(foodGenerator = foodGenerator)

      val (playState, foodActions) = FoodReady.execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      playState.food shouldEqual FoodPresent(Location(2,3), MoveNumber(10))
      foodActions should beEmpty
    }
  }
}
