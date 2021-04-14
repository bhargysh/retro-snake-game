package snake

import snake.Generators._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

import scala.util.Random

class AddObstacleSpec extends Specification with ScalaCheck {
  "Add obstacle" should {
    "modify obstacle state" >> prop { (food: Food, obstacles: Set[Location]) =>
      val initialState = PlayState(
        playing = true,
        food,
        Snake(List(Location(2,3), Location(2,4)), 3, Down),
        obstacles,
        new RandomFoodGenerator(new Random),
        new RandomObstacleGenerator(new Random)
      )
      val (playState, boardActions) = AddObstacle.execute
        .run(SnakeGameWorld.board, MoveNumber(2))
        .run(initialState)
        .value

      playState.obstacles should haveLength(initialState.obstacles.size + 1)
      playState.obstacles should containAllOf(obstacles.toSeq)
      boardActions should beEmpty
    }
  }
}
