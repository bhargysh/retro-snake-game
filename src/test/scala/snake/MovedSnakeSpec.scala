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
      val initialLocation = List(Location(8, 8), Location(2, 4))
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
      val initialLocation = List(Location(8, 8), Location(2, 4))
      val initialState = initialPlayState.copy(food = FoodPresent(Location(8, 7), MoveNumber(10)),
        snake = Snake(initialLocation, 3, Down))
      val newSnake = Snake(Location(8, 7) :: initialLocation, 3, Up)
      val (playState, foodActions) = MovedSnake(newSnake).execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      foodActions should contain(AddFood, GrowSnake)
    }

  }

}
