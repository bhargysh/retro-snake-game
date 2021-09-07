package snake

import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.implicits._
import org.specs2.mutable.Specification

class GameStepSpec extends Specification with BoardActionFixtures {

  "updateGame" should {
    def getDirection(direction: Option[Direction]): Play[Option[Direction]] = direction.pure[Play]
    def playTurn(sng: SnakeGameWorld, dir: Option[Direction]): Play[SnakeGameWorld] = sng.play[Play](dir)

    "return new SnakeGameWorld when game is continuing" in {
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      def renderView(ref: Ref[Play, Boolean])(snakeGameWorld: SnakeGameWorld): Play[Unit] = {
        ref.set(true)
      }

      val result: Kleisli[P, (Board, MoveNumber), (Option[SnakeGameWorld], Boolean)] = for {
        called <- Ref[Play].of(false)
        gameStep = new GameStep[Play](getDirection(None), renderView(called), playTurn) //TODO: don't test snake game world, make this simpler
        newWorld <- gameStep.updateGame(snakeGameWorld)
        calledValue <- called.get
      } yield (newWorld, calledValue)

      result
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState)
        .unsafeRunSync() should beLike[(Option[SnakeGameWorld], Boolean)] { case (actualSnakeGameWorld, renderedValue) =>
        actualSnakeGameWorld.map(_.moveNumber) should beSome(MoveNumber(1))
        renderedValue shouldEqual true
      }
    }

    "return None when game is over" in {
      val snake = Snake(List(Location(8,8), Location(8,7)), 2, Up)
      val immovableSnakeGameWorld = SnakeGameWorld.newSnakeGameWorld.copy(snake = snake)
      val none: Option[Direction] = None
      val gameStep = new GameStep(none.pure[Play], _ => ().pure[Play], playTurn)

      gameStep.updateGame(immovableSnakeGameWorld)
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState.copy(snake = snake))
        .unsafeRunSync() shouldEqual None
    }

    "pass direction input to produce new SnakeGameWorld" in { //TODO: fix this and think about is this easier to test .run.run.unsaferunsync errywhere
      val direction: Option[Direction] = Some(Left)
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      val gameStep = new GameStep[Play](direction.pure[Play], _ => ().pure[Play], playTurn)

      gameStep.updateGame(snakeGameWorld)
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState)
        .unsafeRunSync() should beLike[Option[SnakeGameWorld]] { case actualSnakeGameWorld =>
        actualSnakeGameWorld.map(_.snake.direction) should beSome(Left)
      }
    }
  }
}