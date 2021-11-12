package snake

import cats.effect.concurrent.Ref
import cats.implicits._
import org.specs2.mutable.Specification

class SnakeGameWorldSpec extends Specification with BoardActionFixtures {
  "play" should {
    "return a new SnakeGameWorld" in {
      val snake = Snake(
        List(Location(1, 1), Location(1, 2)),
        length = 2,
        direction = Up
      )
      val initialSNG = SnakeGameWorld(
        SnakeGameWorld.board,
        MoveNumber(1),
        TurnState(playing = true,
        SnakeGameWorld.food,
          snake,
        SnakeGameWorld.obstacles)
      )

      val expectedBoardActions: Vector[BoardAction] = Vector(StartTurn(Some(Up)))

      val result: Turn[Vector[Vector[BoardAction]]] = for {
        inputActions <- Ref[Turn].of[Vector[Vector[BoardAction]]](Vector.empty)
        _ <- {
          implicit val runner: ActionRunner[Turn, BoardAction] =
            (actions: Vector[BoardAction]) => inputActions.update(initialActions => initialActions :+ actions)
          initialSNG.play[Turn](direction = Some(Up))
        }
        calledActions <- inputActions.get
      } yield calledActions


      val (boardActions) = result
        .run(initialSNG.board, initialSNG.moveNumber)
        .runA(initialPlayState)
        .unsafeRunSync()

      boardActions should contain(exactly(expectedBoardActions))
    }
  }
}
