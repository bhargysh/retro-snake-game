package snake

import cats.Id
import org.specs2.mutable.Specification

import scala.util.Random

class SnakeGameWorldSpec extends Specification {
  "play" should {
    implicit val foodGenerator: RandomFoodGenerator = new RandomFoodGenerator(new Random())
    val boardActionHelper = new BoardActionHelper[Id]
    import boardActionHelper._

    "return a new SnakeGameWorld" in {
      val modifiedSnake = Snake(
        List(Location(2, 1), Location(2, 2)),
        length = 2,
        direction = Left
      )
      val expectedObstacles = Set(Location(2, 3))
      val snake = Snake(
        List(Location(1, 1), Location(1, 2)),
        length = 2,
        direction = Up
      )
      val initialSNG = SnakeGameWorld(
        snake,
        SnakeGameWorld.obstacles,
        SnakeGameWorld.board,
        SnakeGameWorld.food,
        isPlaying = true,
        MoveNumber(1),
        (food: Food, snake: Snake, board: Board, obstacles: Set[Location]) => ???
      )
      val updatedSNG: Id[SnakeGameWorld] = initialSNG.play(direction = Some(Up))

      updatedSNG.moveNumber shouldEqual MoveNumber(2) //TODO: what should this test be testing?
    }
  }
}
