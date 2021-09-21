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
        snake,
        SnakeGameWorld.obstacles,
        SnakeGameWorld.board,
        SnakeGameWorld.food,
        isPlaying = true,
        MoveNumber(1)
      )

      val expectedBoardActions: Vector[BoardAction] = Vector(StartTurn(Some(Up)))

      val result: Play[(Vector[Vector[BoardAction]], SnakeGameWorld)] = for {
        inputActions <- Ref[Play].of[Vector[Vector[BoardAction]]](Vector.empty)
        newSNG <- {
          implicit val runner: ActionRunner[Play, BoardAction] =
            (actions: Vector[BoardAction]) => inputActions.update(initialActions => initialActions :+ actions)
          initialSNG.play[Play](direction = Some(Up))
        }
        calledActions <- inputActions.get
      } yield (calledActions, newSNG)


      val (boardActions, updatedSNG) = result
        .run(initialSNG.board, initialSNG.moveNumber)
        .runA(initialPlayState)
        .unsafeRunSync()

      boardActions should contain(exactly(expectedBoardActions))
      updatedSNG.moveNumber shouldEqual MoveNumber(2) //TODO: add a check for obstacles too
    }
  }
}
