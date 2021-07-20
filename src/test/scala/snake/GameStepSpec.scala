package snake

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._
import org.specs2.mutable.Specification

class GameStepSpec extends Specification with BoardActionFixtures {

  "updateGame" should {
    implicit val foodGenerator: RandomFoodGenerator = new RandomFoodGenerator(new Random())
    "return new SnakeGameWorld when game is continuing" in {
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      def renderView(ref: Ref[Play, Boolean])(snakeGameWorld: SnakeGameWorld): Play[Unit] = {
        ref.set(true)
      }

      val result = for {
        called <- Ref[Play].of(false)
        gameStep = new GameStep(getDirection(None), renderView(called))
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
      val gameStep = new GameStep(none.pure[Play], _ => ().pure[Play])
      gameStep.updateGame(immovableSnakeGameWorld)
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState)
        .unsafeRunSync() shouldEqual None
    }

    "pass direction input to produce new SnakeGameWorld" in { //TODO: fix this and think about is this easier to test .run.run.unsaferunsync errywhere
      val direction: Option[Direction] = Some(Left)
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      val gameStep = new GameStep[Play](direction.pure[Play], _ => ().pure[Play])
      gameStep.updateGame(snakeGameWorld)
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState)
        .unsafeRunSync() should beLike[Option[SnakeGameWorld]] { case actualSnakeGameWorld =>
        actualSnakeGameWorld.map(_.snake.direction) should beSome(Left)
      }
    }
  }
}