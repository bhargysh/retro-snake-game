package snake

import org.specs2.mutable.Specification
import snake.BoardAction._

class SnakeGameWorldSpec extends Specification {
  "play" should {
    "return a new SnakeGameWorld" in {
      val vectorFoodAction = Vector.newBuilder[(PlayState, BoardAction)]
      val modifiedSnake = Snake(
        List(Location(2, 1), Location(2, 2)),
        length = 2,
        direction = Left
      )
      val expectedObstacles = Set(Location(2, 3))
      val actionRunner: ActionRunner[BoardAction, Play] = new ActionRunner[BoardAction, Play]({ foodAction: BoardAction =>
        for {
          state <- BoardAction.inspectState[PlayState](s => s)
          _ <- BoardAction.modifyState(s => s.copy(playing = false, snake = modifiedSnake, obstacles = expectedObstacles))
          _ = vectorFoodAction +=(state -> foodAction)
        } yield Vector.empty[BoardAction]
      })
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
        actionRunner,
        (moveNumber: MoveNumber, snake: Snake, board: Board) => ???,
        (food: Food, snake: Snake, board: Board, obstacles: Set[Location]) => ???
      )
      val updatedSNG = initialSNG.play(direction = Some(Up))

      def expectedPlayState(playState: PlayState, foodAction: BoardAction) = {
        playState.snake shouldEqual snake
        playState.playing shouldEqual true
        playState.food shouldEqual SnakeGameWorld.food
        foodAction shouldEqual StartTurn(Some(Up))
      }
      vectorFoodAction.result() should contain((expectedPlayState _).tupled)
      updatedSNG.isPlaying shouldEqual false
      updatedSNG.moveNumber shouldEqual MoveNumber(2)
      updatedSNG.snake shouldEqual modifiedSnake
      updatedSNG.obstacles shouldEqual expectedObstacles
    }
  }
}
