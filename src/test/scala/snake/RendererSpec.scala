package snake

import org.scalajs.dom.raw.Element
import org.specs2.mutable.Specification
import org.scalajs.dom.{Node, document}

class RendererSpec extends Specification {

  "render view" should {
    "replace child of boardUI with newly rendered world" in {
      val boardUI: Element = document.createElement("div")
      val oldWorld = document.createElement("div")
      val expectedWorld: Node = document.createElement("div")
      val initialRenderedWorld = document.createElement("div")
      val ioRenderer = Renderer(boardUI, _ => expectedWorld, initialRenderedWorld)
      val snakeGameWorld = SnakeGameWorld.newSnakeGameWorld

      boardUI.appendChild(oldWorld)
      ioRenderer.flatMap(renderer => renderer.renderView(snakeGameWorld)).unsafeRunSync()
//      renderer.renderView(snakeGameWorld).unsafeRunSync()
      boardUI.firstElementChild should beTheSameAs(expectedWorld)
    }
  }
}
