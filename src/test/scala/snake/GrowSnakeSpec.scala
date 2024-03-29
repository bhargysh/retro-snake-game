package snake

import org.specs2.mutable.Specification

class GrowSnakeSpec extends Specification with BoardActionFixtures {
  "Grow snake food action" should {
    "modify snake length in PlayState" in {
      val (playState, foodActions) = GrowSnake.execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialPlayState)
        .unsafeRunSync()

      playState.snake.length shouldEqual 4
      foodActions should beEmpty
    }
  }

}
