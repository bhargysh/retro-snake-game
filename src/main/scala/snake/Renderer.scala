package snake

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import org.scalajs.dom.Node
import org.scalajs.dom.raw.Element

class Renderer[F[_]](boardUI: Element, render: SnakeGameWorld => Node, currentRenderedWorld: Ref[F, Node])(implicit F: Sync[F]) {

  def renderView(newSnakeGameWorld: SnakeGameWorld): F[Unit] =
    for {
      oldWorld <- currentRenderedWorld.get
      newRenderedWorld = render(newSnakeGameWorld)
      _ <- F.delay(boardUI.replaceChild(newRenderedWorld, oldWorld))
      _ <- currentRenderedWorld.set(newRenderedWorld)
    } yield ()

}

object Renderer {

  def apply[F[_]: Sync](boardUI: Element, render: SnakeGameWorld => Node, initialRenderedWorld: Node): F[Renderer[F]] =
    for {
      ref <- Ref[F].of(initialRenderedWorld)
    } yield new Renderer(boardUI, render, ref)
}
