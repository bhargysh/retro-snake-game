package snake

import org.specs2.mutable.Specification

import scala.util.Random

class AddObstacleSpec extends Specification {
  "Add obstacle" should {
    "modify obstacle state" in {
      val initialState = PlayState(
        playing = true,
        FoodAbsent(MoveNumber(3)),
        Snake(List(Location(2,3), Location(2,4)), 3, Down),
        SnakeGameWorld.obstacles,
        new RandomFoodGenerator(new Random),
        new RandomObstacleGenerator(new Random)
      )
      val (playState, boardActions) = AddObstacle.execute
        .run(SnakeGameWorld.board, MoveNumber(2))
        .run(initialState)
        .value

      playState.obstacles should haveLength(1)
      boardActions should beEmpty
    } //TODO: add property based testing here
  }
}
