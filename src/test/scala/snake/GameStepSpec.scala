package snake

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.specs2.mutable.Specification

import scala.util.Random

class GameStepSpec extends Specification {
  "updateGame" should {
    implicit val foodGenerator: RandomFoodGenerator = new RandomFoodGenerator(new Random())
    "return new SnakeGameWorld when game is continuing" in {
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      def renderView(ref: Ref[IO, Boolean])(snakeGameWorld: SnakeGameWorld): IO[Unit] = {
        ref.set(true)
      }

      val result: IO[(Option[SnakeGameWorld], Boolean)] = for {
        called <- Ref[IO].of(false)
        gameStep = new GameStep(IO(None), renderView(called))
        newWorld <- gameStep.updateGame(snakeGameWorld)
        calledValue <- called.get
      } yield (newWorld, calledValue)

      result.unsafeRunSync() should beLike[(Option[SnakeGameWorld], Boolean)] { case (actualSnakeGameWorld, renderedValue) =>
        actualSnakeGameWorld.map(_.moveNumber) should beSome(MoveNumber(1))
        renderedValue shouldEqual true
      }
    }

    "return None when game is over" in {
      val snake = Snake(List(Location(8,8), Location(8,7)), 2, Up)
      val immovableSnakeGameWorld = SnakeGameWorld.newSnakeGameWorld.copy(snake = snake)
      val gameStep = new GameStep(IO(None), _ => IO.unit)
      gameStep.updateGame(immovableSnakeGameWorld).unsafeRunSync() shouldEqual None
    }

    "pass direction input to produce new SnakeGameWorld" in {
      val direction = IO(Some(Left))
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      val gameStep = new GameStep(direction, _ => IO.unit)
      gameStep.updateGame(snakeGameWorld).unsafeRunSync() should beLike[Option[SnakeGameWorld]] { case actualSnakeGameWorld =>
        actualSnakeGameWorld.map(_.snake.direction) should beSome(Left)
      }
    }
  }
}