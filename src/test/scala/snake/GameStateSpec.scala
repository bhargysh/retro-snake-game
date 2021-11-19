package snake

import cats.data.{ReaderT, StateT}
import cats.effect.IO
import org.specs2.mutable.Specification

class GameStateSpec extends Specification with BoardActionFixtures {
  "toGameState" should {
    "return Game with the correct SnakeGameWorld" in {
      val board = SnakeGameWorld.board
      val moveNumber = MoveNumber(10)
      val initialTurnState = TurnState(
        playing = true,
        FoodPresent(Location(2,3), MoveNumber(10)),
        Snake(List(Location(2,3), Location(2,4)), 3, Down),
        SnakeGameWorld.obstacles
      )
      val turn: Turn[Unit] = ReaderT[P, TurnEnv, Unit]((turnEnv: TurnEnv) => StateT((state: TurnState) => IO(initialTurnState, ())))
      val initialSNG = SnakeGameWorld(board, moveNumber, initialTurnState)
      val expectedSNG = SnakeGameWorld(board, MoveNumber(11), initialTurnState)

      GameState.toGameState(turn)
        .runA(initialSNG)
        .unsafeRunSync() should_===(expectedSNG)
    }
  }
}
