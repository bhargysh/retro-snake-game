package snake

import snake.Generators._
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification

class AddObstacleSpec extends Specification with ScalaCheck with BoardActionFixtures {
  import helper._

  "Add obstacle" should {
    "modify obstacle state" >> prop { (food: Food, obstacles: Set[Location]) =>
      val initialState = initialPlayState.copy(food = food, obstacles = obstacles)
      val (playState, boardActions) = AddObstacle.execute
        .run(SnakeGameWorld.board, MoveNumber(2))
        .run(initialState)

      playState.obstacles should haveLength(initialState.obstacles.size + 1)
      playState.obstacles should containAllOf(obstacles.toSeq)
      boardActions should beEmpty
    }
  }
}
