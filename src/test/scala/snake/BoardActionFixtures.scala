package snake

import cats.effect.IO

import scala.util.Random

trait BoardActionFixtures {

  def foodGenerator(): FoodGenerator[IO] = (moveNumber: MoveNumber, snake: Snake, board: Board, obstacles: Set[Location]) => ???

  implicit val boardActionStateReader: BoardActionStateReader[Play] = new BoardActionStateReaderImpl(foodGenerator(), new RandomObstacleGenerator(new Random)) //TODO: confirm if we need the actual random obstacle gen

  def initialPlayState = PlayState(
    playing = true,
    FoodPresent(Location(2,3), MoveNumber(10)),
    Snake(List(Location(2,3), Location(2,4)), 3, Down),
    SnakeGameWorld.obstacles
  )
}
