package snake

import org.specs2.mutable.Specification

import scala.util.Random

class GrowSnakeSpec extends Specification {

  "Grow snake food action" should {
    "modify snake length in PlayState" in {
      val initialState = PlayState(
        playing = true,
        FoodPresent(Location(2,3), MoveNumber(10)),
        Snake(List(Location(2,3), Location(2,4)), 3, Down),
        new FoodGenerator(new Random)
      )
      val (playState, foodActions) = GrowSnake.execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      playState.snake.length shouldEqual 4
      foodActions should beEmpty
    }
  }

}
