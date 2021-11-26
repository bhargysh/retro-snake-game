package snake

import cats.data.{ReaderT, StateT}
import cats.effect.IO
import org.specs2.mutable.Specification

class GameStateSpec extends Specification {
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

      var actualActions: Vector[BoardAction] = Vector.empty[BoardAction]
      val runner = new ActionRunner[Turn, BoardAction] {
        def runActions(actions: Vector[BoardAction]): Turn[Unit] =
          ReaderT.liftF[P, TurnEnv, Unit](StateT.liftF[IO, TurnState, Unit](IO {
            actualActions = actualActions ++ actions
            ()
          }))
      }
      val result = for {
        sng <- new GameState(runner).playTurn(direction = Some(Left))
      } yield sng

      val initialSNG = SnakeGameWorld(board, moveNumber, initialTurnState)
      val expectedSNG = SnakeGameWorld(board, MoveNumber(11), initialTurnState)
      val actualSNG = result
        .runA(initialSNG)
        .unsafeRunSync()
      val expectedBoardActions: Vector[BoardAction] = Vector(StartTurn(Some(Left)))

      actualSNG should_=== expectedSNG
      actualActions should contain(expectedBoardActions)
    }
  }
}
