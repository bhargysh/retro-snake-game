package snake

import cats.data.Kleisli
import cats.effect.concurrent.Ref
import cats.implicits._
import org.specs2.mutable.Specification
import snake.components.PlayStateFixtures

class GameStepSpec extends Specification with BoardActionFixtures with PlayStateFixtures {

  "updateGame" should {
    implicit val actionRunner: ActionRunner[Turn, BoardAction] = (actions: Vector[BoardAction]) => ().pure[Turn]
    def getDirection(direction: Option[Direction]): Turn[Option[Direction]] = direction.pure[Turn]

    "return new SnakeGameWorld when game is continuing" in {
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      def renderView(ref: Ref[Turn, Boolean])(snakeGameWorld: SnakeGameWorld): Turn[Unit] = {
        ref.set(true)
      }
      def playTurn(sng: SnakeGameWorld, dir: Option[Direction]): Turn[SnakeGameWorld] = {
        sng.copy(moveNumber = MoveNumber(1)).pure[Turn]
      }

      val result: Turn[(Option[SnakeGameWorld], Boolean)] = for {
        called <- Ref[Turn].of(false)
        gameStep = new GameStep[Turn](getDirection(None), renderView(called), playTurn)
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
      val immovableSnakeGameWorld = modifyPlayState(_.copy(snake = snake))
      val none: Option[Direction] = None

      def playTurn(sng: SnakeGameWorld, dir: Option[Direction]): Turn[SnakeGameWorld] = {
        sng.copy(playState = sng.playState.copy(playing = false)).pure[Turn]
      }
      val gameStep = new GameStep(none.pure[Turn], _ => ().pure[Turn], playTurn)

      gameStep.updateGame(immovableSnakeGameWorld)
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState.copy(snake = snake))
        .unsafeRunSync() shouldEqual None
    }

    "pass direction input to produce new SnakeGameWorld" in {
      def playTurn(ref: Ref[Turn, Option[Direction]])(sng: SnakeGameWorld, dir: Option[Direction]): Turn[SnakeGameWorld] = {
        ref.set(dir).map(_ => sng)
      }

      val inputDirection: Option[Direction] = Some(Left)
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld
      val result: Turn[Option[Direction]] = for {
        dirRef <- Ref[Turn].of[Option[Direction]](None)
        gameStep = new GameStep[Turn](inputDirection.pure[Turn], _ => ().pure[Turn], playTurn(dirRef))
        _ <- gameStep.updateGame(snakeGameWorld)
        someDir <- dirRef.get
      } yield someDir


      implicit val actionRunner2: ActionRunner[Turn, BoardAction] = new ActionRunner[Turn, BoardAction] {
        def runActions(actions: Vector[BoardAction]): Turn[Unit] = ???
      }

      result
        .run(SnakeGameWorld.board, MoveNumber(0))
        .runA(initialPlayState)
        .unsafeRunSync() shouldEqual inputDirection
    }
  }
}