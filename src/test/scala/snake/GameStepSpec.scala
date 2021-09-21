package snake

import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.implicits._
import org.specs2.mutable.Specification

class GameStepSpec extends Specification with BoardActionFixtures {

  "updateGame" should {
    implicit val actionRunner: ActionRunner[Play, BoardAction] = (actions: Vector[BoardAction]) => ().pure[Play]
    def getDirection(direction: Option[Direction]): Play[Option[Direction]] = direction.pure[Play]

    "return new SnakeGameWorld when game is continuing" in {
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      def renderView(ref: Ref[Play, Boolean])(snakeGameWorld: SnakeGameWorld): Play[Unit] = {
        ref.set(true)
      }
      def playTurn(sng: SnakeGameWorld, dir: Option[Direction]): Play[SnakeGameWorld] = {
        sng.copy(moveNumber = MoveNumber(1)).pure[Play]
      }

      val result: Play[(Option[SnakeGameWorld], Boolean)] = for {
        called <- Ref[Play].of(false)
        gameStep = new GameStep[Play](getDirection(None), renderView(called), playTurn)
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

      def playTurn(sng: SnakeGameWorld, dir: Option[Direction]): Play[SnakeGameWorld] = {
        sng.copy(isPlaying = false).pure[Play]
      }
      val gameStep = new GameStep(none.pure[Play], _ => ().pure[Play], playTurn)

      gameStep.updateGame(immovableSnakeGameWorld)
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState.copy(snake = snake))
        .unsafeRunSync() shouldEqual None
    }

    "pass direction input to produce new SnakeGameWorld" in { //TODO: think about is this easier to test .run.run.unsaferunsync errywhere
      def playTurn(ref: Ref[Play, Option[Direction]])(sng: SnakeGameWorld, dir: Option[Direction]): Play[SnakeGameWorld] = {
        ref.set(dir).map(_ => sng)
      }

      val inputDirection: Option[Direction] = Some(Left)
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      val result: Play[Option[Direction]] = for {
        dirRef <- Ref[Play].of[Option[Direction]](None)
        gameStep = new GameStep[Play](inputDirection.pure[Play], _ => ().pure[Play], playTurn(dirRef))
        _ <- gameStep.updateGame(snakeGameWorld)
        someDir <- dirRef.get
      } yield someDir


      implicit val actionRunner2: ActionRunner[Play, BoardAction] = new ActionRunner[Play, BoardAction] {
        def runActions(actions: Vector[BoardAction]): Play[Unit] = ???
      }

      result
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState)
        .unsafeRunSync() shouldEqual inputDirection
    }
  }
}