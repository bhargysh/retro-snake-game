package snake

import cats.Id
import org.specs2.mutable.Specification

import scala.util.Random

class SnakeGameWorldSpec extends Specification with BoardActionFixtures {
  "play" should {
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
      val updatedSNG =
        initialSNG.play(direction = Some(Up))
          .run(initialSNG.board, initialSNG.moveNumber)
          .run(initialPlayState)
          .unsafeRunSync()
          ._2

      updatedSNG.moveNumber shouldEqual MoveNumber(2) //TODO: what should this test be testing?
    }
  }
}
