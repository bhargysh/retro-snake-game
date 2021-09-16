package snake

import cats.effect.IO
import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import snake.Generators._

import scala.util.Random

class AddObstacleSpec extends Specification with ScalaCheck with BoardActionFixtures {
  override def obstacleGenerator(): ObstacleGenerator[IO] = new RandomObstacleGenerator(new Random)
  "Add obstacle" should {
    "modify obstacle state" >> prop { (food: Food, obstacles: Set[Location]) =>
      val initialState = initialPlayState.copy(food = food, obstacles = obstacles)
      val (playState, boardActions) = AddObstacle.execute
        .run(SnakeGameWorld.board, MoveNumber(2))
        .run(initialState)
        .unsafeRunSync()

      playState.obstacles should haveLength(initialState.obstacles.size + 1)
      playState.obstacles should containAllOf(obstacles.toSeq)
      boardActions should beEmpty
    }
  }
}
