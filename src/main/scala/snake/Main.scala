package snake
import cats.arrow.FunctionK
import cats.data.StateT
import cats.effect.{ExitCode, IO, IOApp, Sync}
import cats.implicits._
import cats.{Monad, ~>}
import org.scalajs.dom.raw.{Element, KeyboardEvent}
import org.scalajs.dom.{Node, document}

import scala.concurrent.duration.{Duration, SECONDS}
import scala.util.Random

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    val foodGenerator: RandomFoodGenerator = new RandomFoodGenerator(new Random())
    val obstacleGenerator: RandomObstacleGenerator = new RandomObstacleGenerator(new Random())
    implicit val boardActionStateReader: BoardActionStateReaderImpl = new BoardActionStateReaderImpl(foodGenerator)

    val liftGame = new FunctionK[IO, Game] {
      def apply[A](fa: IO[A]): Game[A] = StateT.liftF[IO, SnakeGameWorld, A](fa)
    }

    val playExit: Game[ExitCode] = for {
      world <- StateT.get[IO, SnakeGameWorld]
      html = new SnakeGameHtml(document)
      renderedWorld: Node = html.render(world)
      boardUI <- appendBoardToDocument[Game](renderedWorld)
      renderer <- Renderer[Game](boardUI, html.render, renderedWorld)
//      actionRunner = new ActionRunner[BoardAction, Play](_.execute) TODO: ideally we would initialise the runner once, here
      _ <- liftGame(actionOnKeyboardEvent(boardUI))
      gameStep =
        new GameStep[Game](
          getInput[Game](boardUI),
          renderer.renderView,
          (sng: SnakeGameWorld, maybeDirection: Option[Direction]) => toGameState(obstacleGenerator)(sng.play[Play](maybeDirection))
        )
      _ <- loop[SnakeGameWorld, Game](gameStep.updateGame, liftGame)(world)
    } yield ExitCode.Success

    playExit
      .runA(SnakeGameWorld.newSnakeGameWorld)
  }

  def toGameState(obstacleGenerator: ObstacleGenerator)(play: Play[SnakeGameWorld]): Game[SnakeGameWorld] = {
    StateT.modifyF { (oldSNG: SnakeGameWorld) =>
      val ps = PlayState(
        playing = oldSNG.isPlaying,
        food = oldSNG.food,
        snake = oldSNG.snake,
        obstacles = oldSNG.obstacles,
        obstacleGenerator = obstacleGenerator
      )
      play
        .run((oldSNG.board, oldSNG.moveNumber))
        .runA(ps)
    }.get
  }

  private def loop[A, F[_]: Monad](work: A => F[Option[A]], lift: IO ~> F)(initial: A): F[Unit] =
    Monad[F].tailRecM(initial) { old =>
      for {
        _ <- lift(timer.sleep(Duration(1, SECONDS)))
        optionNew <- work(old)
      } yield optionNew.toLeft(())
  }

  private def getInput[F[_]: Sync](boardUI: Element): F[Option[Direction]] = for {
    maybeDirectionData <- Sync[F].delay {
      Option(boardUI.getAttribute("data-direction"))
    }
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