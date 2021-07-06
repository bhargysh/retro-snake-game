package snake
import cats.arrow.FunctionK
import cats.{ApplicativeError, Monad, ~>}
import cats.data.{IndexedStateT, Kleisli, ReaderT, StateT}
import cats.effect.{Bracket, ExitCase, ExitCode, IO, IOApp, Sync}
import org.scalajs.dom.raw.{Element, KeyboardEvent}
import org.scalajs.dom.{Node, document}
import cats.implicits._

import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.Random

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val foodGenerator: RandomFoodGenerator = new RandomFoodGenerator(new Random())
    val obstacleGenerator: RandomObstacleGenerator = new RandomObstacleGenerator(new Random())
    implicit val boardActionStateReader: BoardActionStateReaderImpl[IO] = new BoardActionStateReaderImpl[IO](foodGenerator)
    type Play[A] = boardActionStateReader.Play[A] // cool Scala feature, path dependent type
    type P[A] = boardActionStateReader.P[A]

    implicit def readerTforStuff: Sync[Play] = new Sync[Play] {
      def suspend[A](thunk: => Play[A]): Play[A] = ReaderT
        .liftF[StateT[IO, PlayState, *], (Board, MoveNumber), Play[A]](StateT.liftF[IO, PlayState, Play[A]](IO.pure[Play[A]](thunk))).flatMap(identity)

      def bracketCase[A, B](acquire: Play[A])(use: A => Play[B])(release: (A, ExitCase[Throwable]) => Play[Unit]): Play[B] = {
        Bracket.catsKleisliBracket[P, (Board, MoveNumber), Throwable](Sync[P]).bracketCase(acquire)(use)(release)
      }

      def raiseError[A](e: Throwable): Play[A] = ReaderT
        .liftF[StateT[IO, PlayState, *], (Board, MoveNumber), A](StateT.liftF[IO, PlayState, A](IO.raiseError(e)))

      def handleErrorWith[A](fa: Play[A])(f: Throwable => Play[A]): Play[A] = Kleisli { e =>
        val pOfA: boardActionStateReader.P[A] = fa.run(e)
        ApplicativeError[P, Throwable].handleErrorWith(pOfA)(throwable => f(throwable).run(e))
      }

      //A -> IO[B], get to StateT IO[B]

      def pure[A](x: A): Play[A] = ReaderT.liftF[StateT[IO, PlayState, *], (Board, MoveNumber), A](StateT.liftF[IO, PlayState, A](IO.pure[A](x)))

      def flatMap[A, B](fa: Play[A])(f: A => Play[B]): Play[B] = fa.flatMap(f)

      def tailRecM[A, B](a: A)(f: A => Play[Either[A, B]]): Play[B] = {
        Kleisli.catsDataMonadForKleisli[P, (Board, MoveNumber)](IndexedStateT.catsDataMonadForIndexedStateT[IO, PlayState](Monad[IO])).tailRecM(a)(f)
      }
    }

    val liftPlay = new FunctionK[IO, Play] {
      def apply[A](fa: IO[A]): Play[A] = ReaderT
        .liftF[StateT[IO, PlayState, *], (Board, MoveNumber), A](StateT.liftF[IO, PlayState, A](fa))
    }

    val playExit = for {
      world <- Sync[Play].pure(SnakeGameWorld.newSnakeGameWorld)
      html = new SnakeGameHtml(document)
      renderedWorld: Node = html.render(world)
      boardUI <- appendBoardToDocument[Play](renderedWorld)
      renderer <- Renderer[Play](boardUI, html.render, renderedWorld)
//      actionRunner = new ActionRunner[BoardAction, Play](_.execute) TODO: ideally we would initialise the runner once, here
      gameStep = new GameStep[Play](getInput(boardUI), renderer.renderView)
      _ <- liftPlay(actionOnKeyboardEvent(boardUI))
      _ <- loop[SnakeGameWorld, Play](gameStep.updateGame, liftPlay)(world)
    } yield ExitCode.Success

    val initialPlayState = PlayState(
      playing = SnakeGameWorld.isPlaying,
      food = SnakeGameWorld.food,
      snake = SnakeGameWorld.newSnakeGameWorld.snake,
      obstacles = SnakeGameWorld.obstacles,
      obstacleGenerator = obstacleGenerator
    )

    playExit
      .run((SnakeGameWorld.board, SnakeGameWorld.newSnakeGameWorld.moveNumber))
      .runA(initialPlayState)
  }

  private def loop[A, F[_]: Monad](work: A => F[Option[A]], lift: IO ~> F)(old: A): F[Unit] = Monad[F].tailRecM(old) { old =>
    for {
      _ <- lift(timer.sleep(Duration(1, SECONDS)))
      newRenderedNode <- work(old)
    } yield newRenderedNode.toLeft(())
  }

  private def getInput[F[_]: Sync](boardUI: Element): F[Option[Direction]] = for {
    maybeDirectionData <- Sync[F].delay(Option(boardUI.getAttribute("data-direction")))
    maybeDirection = maybeDirectionData.flatMap(Direction.fromStr)
  } yield maybeDirection

  private def actionOnKeyboardEvent(boardUI: Element): IO[Unit] = IO {
    document.addEventListener("keydown", (event: KeyboardEvent) => {
      val maybeDirection = Keyboard.stringToDirection(event.key)
      maybeDirection match {
        case Some(value) => boardUI.setAttribute("data-direction", value.toString)
        case None => None
      }
    })
  }

  private def appendBoardToDocument[F[_]: Sync](renderedWorld: Node): F[Element] = Sync[F].delay {
    val boardUI: Element = document.createElement("div")
    document.body.appendChild(boardUI)
    boardUI.appendChild(renderedWorld)
    boardUI
  }
}

// extension ideas (obstacles, poisonous food (increase speed, mess with directions), rendering of DOM if we really want to)

// Nothing -> 0 value, doesn't return
// Unit -> 1 value
// Boolean -> true, false -> sum type
// Option[Unit] -> Some(()), None -> sum type
// Option[Boolean] -> Some(true), Some(false), None -> sum type
// (Boolean, Byte) -> plenty of combinations -> product type -> 2*256 = 512 options
// Boolean => Boolean -> 4 possible funcs (2^2)
// (Nothing, Nothing) -> 0 values
// (Option[Nothing], Nothing) -> (1+1*0 * 0) = 0 values