package snake

import org.scalajs.dom.raw.Element
import org.specs2.mutable.Specification
import org.scalajs.dom.{Node, document}

class RendererSpec extends Specification {

  "render view" should {
    "replace child of boardUI with newly rendered world" in {
      val boardUI: Element = document.createElement("div")
      val expectedWorld: Node = document.createElement("div")
      val initialRenderedWorld = document.createElement("div")
      val ioRenderer = Renderer(boardUI, _ => expectedWorld, initialRenderedWorld)
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld

      boardUI.appendChild(initialRenderedWorld)
      ioRenderer.flatMap(renderer => renderer.renderView(snakeGameWorld)).unsafeRunSync()
      boardUI.firstElementChild should beTheSameAs(expectedWorld)
    }

    "update the current rendered world" in {
      val boardUI: Element = document.createElement("div")

      val initialRenderedWorld = document.createElement("div")
      val firstExpectedWorld: Element = document.createElement("div")
      val secondExpectedWorld: Element = document.createElement("div")
      val renderResults: Iterator[Element] = List(firstExpectedWorld, secondExpectedWorld).iterator

      def testRender(snakeGameWorld: SnakeGameWorld): Node = {
        renderResults.next()
      }

      val ioRenderer = Renderer(boardUI, testRender, initialRenderedWorld)
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld

      boardUI.appendChild(initialRenderedWorld)
      val result = for {
        renderer <- ioRenderer
        _ <- renderer.renderView(snakeGameWorld)
        _ <- renderer.renderView(snakeGameWorld)
      } yield ()

      result.unsafeRunSync()
      boardUI.firstElementChild should beTheSameAs(secondExpectedWorld)
    }
  }
}
