package snake

import cats.effect.IO
import org.specs2.mutable.Specification

class FoodReadySpec extends Specification with BoardActionFixtures {
  override def foodGenerator(): FoodGenerator[IO] = (moveNumber: MoveNumber, snake: Snake, board: Board, obstacles: Set[Location]) => IO(FoodPresent(Location(2,3), MoveNumber(10)))

  "FoodReady action" should {
    "modify food to be randomly generated in PlayState" in {

      val (playState, foodActions) = FoodReady.execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialPlayState)
        .unsafeRunSync()

      playState.food shouldEqual FoodPresent(Location(2,3), MoveNumber(10))
      foodActions should beEmpty
    }
  }
}
