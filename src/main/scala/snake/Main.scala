package snake
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
      _ <- actionOnKeyboardEvent(boardUI)
      _ <- loop[(Node, SnakeGameWorld)](updateGame(html, boardUI))((renderedWorld, world))
    } yield ExitCode.Success

//  TODO: Simplify this for next time
  private def loop[A](work: A => IO[Option[A]])(old: A): IO[Unit] = for {
    _ <- timer.sleep(Duration(1, SECONDS))
    newRenderedNode <- work(old)
    result <- newRenderedNode match {
      case Some(newA) => loop(work)(newA)
      case None => IO.unit
    }
  } yield result

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

  private def updateGame(html: SnakeGameHtml, boardUI: Element)(oldState: (Node, SnakeGameWorld)): IO[Option[(Node, SnakeGameWorld)]] = for {
    maybeDirectionData <- IO(Option(boardUI.getAttribute("data-direction")))
    maybeDirection = maybeDirectionData.flatMap(Direction.fromStr)
    (oldRenderedWorld, world) = oldState
    newWorld = world.play(maybeDirection)
    newRenderedWorld = html.render(newWorld)
    _ <- IO(boardUI.replaceChild(newRenderedWorld, oldRenderedWorld))
    result = if (newWorld.isPlaying) {
      Some((newRenderedWorld, newWorld))
    } else {
      None
    }
  } yield result
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