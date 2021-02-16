package snake

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.specs2.mutable.Specification

class GameStepSpec extends Specification {
  "updateGame" should {
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
  }
}