package snake

import org.specs2.mutable.Specification

class AddFoodSpec extends Specification with BoardActionFixtures {

  "AddFood action" should {
    "modify food in PlayState" in {
      val (playState, boardActions) = AddFood.execute
        .run(SnakeGameWorld.board, MoveNumber(9))
        .run(initialPlayState)
        .unsafeRunSync()

      playState.food shouldEqual FoodAbsent(MoveNumber(19))
      boardActions should beEmpty
    }
  }

}
