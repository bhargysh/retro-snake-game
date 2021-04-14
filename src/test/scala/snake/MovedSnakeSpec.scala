package snake

import org.specs2.mutable.Specification

import scala.util.Random

class MovedSnakeSpec extends Specification with BoardActionFixtures {

  "MovedSnake action" should {
    val initialLocation = List(Location(8, 8), Location(2, 4))
    val snake = Snake(initialLocation, 3, Up)
    "modify playing state when snake is on the wall" in {
      val initialState = initialPlayState.copy(snake = snake)
      val newSnake = Snake(Location(8, 9) :: initialLocation, 3, Up)
      val (playState, foodActions) = MovedSnake(newSnake).execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      playState.playing should beFalse
      foodActions should beEmpty
    }

    "modify snake state" in {
      val initialState = initialPlayState.copy(snake = snake)
      val newSnake = Snake(Location(8, 9) :: initialLocation, 3, Up)
      val (playState, foodActions) = MovedSnake(newSnake).execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      playState.snake shouldEqual newSnake
      foodActions should beEmpty
    }

    "return new FoodAction" in {
      val initialState = initialPlayState.copy(food = FoodPresent(Location(8, 7), MoveNumber(10)),
        snake = Snake(initialLocation, 3, Down))
      val newSnake = Snake(Location(8, 7) :: initialLocation, 3, Up)
      val (_, boardActions) = MovedSnake(newSnake).execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      boardActions should contain(AddFood, GrowSnake)
    }

    "modify play state when snake is on an obstacle" in {
      val initialLocation = List(Location(1, 1), Location(1, 2))
      val initialState = initialPlayState.copy(
        snake = Snake(initialLocation, 3, Down),
        obstacles = Set(Location(1, 3))
      )
      val newSnake = Snake(Location(1, 3) :: initialLocation, 3, Up)
      val (playState, boardActions) = MovedSnake(newSnake).execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      playState.playing mustEqual false
      boardActions must beEmpty
    }

  }

}
