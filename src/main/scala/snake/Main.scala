package snake
import cats.Monad
import cats.effect.{ExitCode, IO, IOApp}
import org.scalajs.dom.raw.{Element, KeyboardEvent}
import org.scalajs.dom.{Node, document}


import scala.concurrent.duration.{Duration, SECONDS}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      world <- IO.pure(SnakeGameWorld.newSnakeGameWorld)
      html = new SnakeGameHtml(document)
      renderedWorld: Node = html.render(world)
      boardUI <- appendBoardToDocument(renderedWorld)
      renderer <- Renderer(boardUI, html.render, renderedWorld)
      gameStep = new GameStep(getInput(boardUI), renderer.renderView)
      _ <- actionOnKeyboardEvent(boardUI)
      _ <- loop[(Node, SnakeGameWorld)](gameStep.updateGame)((renderedWorld, world))
    } yield ExitCode.Success

  private def loop[A](work: A => IO[Option[A]])(old: A): IO[Unit] = Monad[IO].tailRecM(old) { old =>
    for {
      _ <- timer.sleep(Duration(1, SECONDS))
      newRenderedNode <- work(old)
    } yield newRenderedNode.toLeft(())
  }

  private def getInput(boardUI: Element): IO[Option[Direction]] = for {
    maybeDirectionData <- IO(Option(boardUI.getAttribute("data-direction")))
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

  private def appendBoardToDocument(renderedWorld: Node): IO[Element] = IO {
    val boardUI: Element = document.createElement("div")
    document.body.appendChild(boardUI)
    boardUI.appendChild(renderedWorld)
    boardUI
  }
}

// Nothing -> 0 value, doesn't return
// Unit -> 1 value
// Boolean -> true, false -> sum type
// Option[Unit] -> Some(()), None -> sum type
// Option[Boolean] -> Some(true), Some(false), None -> sum type
// (Boolean, Byte) -> plenty of combinations -> product type -> 2*256 = 512 options
// Boolean => Boolean -> 4 possible funcs (2^2)
// (Nothing, Nothing) -> 0 values
// (Option[Nothing], Nothing) -> (1+1*0 * 0) = 0 values