package snake

import cats.effect.IO

trait BoardActionFixtures {

  def foodGenerator(): FoodGenerator[IO] = (moveNumber: MoveNumber, snake: Snake, board: Board, obstacles: Set[Location]) => ???
  def obstacleGenerator(): ObstacleGenerator[IO] = (food: Food, snake: Snake, board: Board, obstacles: Set[Location]) => ???

  implicit val boardActionStateReader: BoardActionStateReader[Turn] = new BoardActionStateReaderImpl(foodGenerator(), obstacleGenerator())

  def initialPlayState = TurnState(
    playing = true,
    FoodPresent(Location(2,3), MoveNumber(10)),
    Snake(List(Location(2,3), Location(2,4)), 3, Down),
    SnakeGameWorld.obstacles
  )
}
