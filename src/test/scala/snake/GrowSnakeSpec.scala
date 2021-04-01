package snake

import org.specs2.mutable.Specification

import scala.util.Random

class GrowSnakeSpec extends Specification with BoardActionFixtures {

  "Grow snake food action" should {
    "modify snake length in PlayState" in {
      val initialState = initialPlayState
      val (playState, foodActions) = GrowSnake.execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialState)
        .value

      playState.snake.length shouldEqual 4
      foodActions should beEmpty
    }
  }

}
