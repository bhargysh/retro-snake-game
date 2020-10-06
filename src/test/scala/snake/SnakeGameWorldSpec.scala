package snake

import org.specs2.mutable.Specification
import FoodAction._
import cats.implicits._
import org.specs2.matcher.MatchResult

class SnakeGameWorldSpec extends Specification {
  "play" should {
    "return a new SnakeGameWorld" in {
      val vectorFoodAction = Vector.newBuilder[(PlayState, FoodAction)]
      val actionRunner: ActionRunner[FoodAction, Play] = new ActionRunner[FoodAction, Play]({foodAction: FoodAction =>
        for {
          state <- FoodAction.inspectState[PlayState](s => s)
          _ = vectorFoodAction +=(state -> foodAction)
        } yield Vector.empty[FoodAction]
//        modify state and observe the modification in updatedSNG
      })
      val snake = Snake(
        List(Location(1, 1), Location(1, 2)),
        length = 2,
        direction = Up
      )
      val initialSNG = SnakeGameWorld(
        snake,
        SnakeGameWorld.board,
        SnakeGameWorld.food,
        isPlaying = true,
        MoveNumber(1),
        actionRunner,
        (moveNumber: MoveNumber, snake: Snake, board: Board) => ???
      )
      val updatedSNG = initialSNG.play(direction = Some(Up))
      def expectedPlayState(playState: PlayState, foodAction: FoodAction) = {
        playState.snake shouldEqual snake
        playState.playing shouldEqual true
        playState.food shouldEqual SnakeGameWorld.food
        foodAction shouldEqual StartTurn(Some(Up))
      }
      vectorFoodAction.result() should contain((expectedPlayState _).tupled)
      //TODO: test the updatedSNG
    }
  }
}
