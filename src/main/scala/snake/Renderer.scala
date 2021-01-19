package snake

import cats.effect.IO
import cats.effect.concurrent.Ref
import org.scalajs.dom.Node
import org.scalajs.dom.raw.Element

class Renderer(boardUI: Element, render: SnakeGameWorld => Node, currentRenderedWorld: Ref[IO, Node]) {

  def renderView(newSnakeGameWorld: SnakeGameWorld): IO[Unit] =
    for {
      oldWorld <- currentRenderedWorld.get
      newRenderedWorld = render(newSnakeGameWorld)
      _ <- IO(boardUI.replaceChild(newRenderedWorld, oldWorld))
      _ <- currentRenderedWorld.set(newRenderedWorld)
    } yield ()

}

object Renderer {

  def apply(boardUI: Element, render: SnakeGameWorld => Node, initialRenderedWorld: Node): IO[Renderer] =
    for {
      ref <- Ref[IO].of(initialRenderedWorld)
    } yield new Renderer(boardUI, render, ref)
}
