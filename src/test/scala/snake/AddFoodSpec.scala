package snake

import org.specs2.mutable.Specification

import scala.util.Random

class AddFoodSpec extends Specification {

  "AddFood action" should {
    "modify food in PlayState" in {
      val initialState = PlayState(
        playing = true,
        FoodPresent(Location(2,3), MoveNumber(10)),
        Snake(List(Location(2,3), Location(2,4)), 3, Down),
        SnakeGameWorld.obstacles,
        new RandomFoodGenerator(new Random),
        new RandomObstacleGenerator(new Random)
      )
      val (playState, foodActions) = AddFood.execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      playState.food shouldEqual FoodAbsent(MoveNumber(19))
      foodActions should beEmpty
    }
  }

}
