package snake
import cats.effect.{ExitCode, IO, IOApp, Timer}
import org.scalajs.dom
import dom.{Node, console, document, window}
import org.scalajs.dom.raw.{Element, KeyboardEvent}

import scala.concurrent.duration.{Duration, SECONDS}
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] =
    for {
      world <- IO.pure(SnakeGameWorld.newSnakeGameWorld)
      html = new SnakeGameHtml(document)
      renderedWorld: Node = html.render(world)
      boardUI <- appendBoardToDocument(renderedWorld)
      _ <- loop[Node](updateGame(world, html, boardUI))(renderedWorld)
      _ <- actionOnKeyboardEvent(boardUI)
    } yield ExitCode.Success

  private def loop[A](work: A => IO[A])(oldRenderedNode: A): IO[Nothing] = for {
    _ <- timer.sleep(Duration(1, SECONDS))
    _ = IO(console.log("-------", oldRenderedNode))
    newRenderedNode <- work(oldRenderedNode)
    _ = IO(console.log("-------", newRenderedNode)) //TODO: IO is not evaluating, work out why!
    result <- loop(work)(newRenderedNode)
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

//  private def setTimeout(world: SnakeGameWorld, html: SnakeGameHtml, renderedWorld: Node, boardUI: Element): IO[Int] = IO {
//    window.setTimeout(() => updateGame(world, html, renderedWorld, boardUI), 1000)
//  }

  private def appendBoardToDocument(renderedWorld: Node): IO[Element] = IO {
    val boardUI: Element = document.createElement("div")
    document.body.appendChild(boardUI)
    boardUI.appendChild(renderedWorld)
    boardUI
  }

  private def updateGame(world: SnakeGameWorld, html: SnakeGameHtml, boardUI: Element)(oldWorld: Node): IO[Node] = for {
    maybeDirectionData <- IO(Option(boardUI.getAttribute("data-direction")))
    maybeDirection = maybeDirectionData.flatMap(Direction.fromStr)
    newWorld = world.play(maybeDirection)
    newRenderedWorld = html.render(newWorld)
    _ <- IO(boardUI.replaceChild(newRenderedWorld, oldWorld))
//    y = if (newWorld.isPlaying) {
//      setTimeout(newWorld, html, newRenderedWorld, boardUI)
//    }
  } yield newRenderedWorld
}
