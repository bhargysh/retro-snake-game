package snake

import scala.util.Random

trait BoardActionFixtures {
  def initialPlayState = PlayState(
    playing = true,
    FoodPresent(Location(2,3), MoveNumber(10)),
    Snake(List(Location(2,3), Location(2,4)), 3, Down),
    SnakeGameWorld.obstacles,
    new RandomFoodGenerator(new Random),
    new RandomObstacleGenerator(new Random)
  )
}
