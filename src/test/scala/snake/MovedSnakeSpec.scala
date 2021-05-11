package snake

import org.specs2.mutable.Specification

class MovedSnakeSpec extends Specification with BoardActionFixtures {
  import helper._

  "MovedSnake action" should {
    val initialLocation = List(Location(8, 8), Location(2, 4))
    val snake = Snake(initialLocation, 3, Up)
    "modify playing state when snake is on the wall" in {
      val initialState = initialPlayState.copy(snake = snake)
      val newSnake = Snake(Location(8, 9) :: initialLocation, 3, Up)
      val (playState, foodActions) = MovedSnake(newSnake).execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)

      playState.playing should beFalse
      foodActions should beEmpty
    }

    "modify snake state" in {
      val initialState = initialPlayState.copy(snake = snake)
      val newSnake = Snake(Location(8, 9) :: initialLocation, 3, Up)
      val (playState, foodActions) = MovedSnake(newSnake).execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)

      playState.snake shouldEqual newSnake
      foodActions should beEmpty
    }

    "return new FoodAction when food is present" >> {
      "return board action if snake head is at food location" in {
        val initialState = initialPlayState.copy(food = FoodPresent(Location(8, 7), MoveNumber(10)),
          snake = Snake(initialLocation, 3, Down))
        val newSnake = Snake(Location(8, 7) :: initialLocation, 3, Up)
        val (_, boardActions) = MovedSnake(newSnake).execute
          .run(SnakeGameWorld.board, MoveNumber(9))
          .run(initialState)

        boardActions should contain(AddFood, GrowSnake)
      }
      "return board action if move number is same as expiry time" in {
        val initialState = initialPlayState.copy(food = FoodPresent(Location(8, 7), MoveNumber(10)),
          snake = Snake(initialLocation, 3, Down))
        val newSnake = Snake(Location(7, 8) :: initialLocation, 3, Left)
        val (_, boardActions) = MovedSnake(newSnake).execute
          .run(SnakeGameWorld.board, MoveNumber(10))
          .run(initialState)

        boardActions should contain(AddFood, AddObstacle)
      }

      "return no board action if snake not at location and expiry time is ongoing" in {
        val initialState = initialPlayState.copy(food = FoodPresent(Location(8, 7), MoveNumber(10)),
          snake = Snake(initialLocation, 3, Down))
        val newSnake = Snake(Location(7, 8) :: initialLocation, 3, Left)
        val (_, boardActions) = MovedSnake(newSnake).execute
          .run(SnakeGameWorld.board, MoveNumber(12))
          .run(initialState)

        boardActions should beEmpty
      }
    }

    "return new FoodAction when food is absent" >> {
      "return board action if turns is same as move number" in {
        val initialState = initialPlayState.copy(food = FoodAbsent(MoveNumber(4)),
          snake = Snake(initialLocation, 3, Down))
        val newSnake = Snake(Location(8, 7) :: initialLocation, 3, Up)
        val (_, boardActions) = MovedSnake(newSnake).execute
          .run(SnakeGameWorld.board, MoveNumber(4))
          .run(initialState)

        boardActions should contain(FoodReady)
      }

      "return no board action if turns is not same as move number" in {
        val initialState = initialPlayState.copy(food = FoodAbsent(MoveNumber(4)),
          snake = Snake(initialLocation, 3, Down))
        val newSnake = Snake(Location(8, 7) :: initialLocation, 3, Up)
        val (_, boardActions) = MovedSnake(newSnake).execute
          .run(SnakeGameWorld.board, MoveNumber(3))
          .run(initialState)

        boardActions should beEmpty
      }

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

      playState.playing mustEqual false
      boardActions must beEmpty
    }

  }

}
