package snake

import org.scalajs.dom.{Node, document}
import org.scalatest.funspec.AnyFunSpec
import org.scalajs.dom.Window

class SnakeGameHtmlSpec extends AnyFunSpec  {
  def countText(node: Node, text: String): Int = {
    if (node.nodeType === Node.TEXT_NODE) {
      if (node.nodeValue === text) {
        1
      }
      else {
        0
      }
    }
    else {
      Range(0, node.childNodes.length).map { i =>
        countText(node.childNodes.item(i), text)
      }.sum
    }
  }
  val jsDom: JSDom = new JSDom

  describe("SnakeGameHtml") {
    it("renders snake game world") {
      val snakeHtml = new SnakeGameHtml(jsDom.window.document)
      val node = snakeHtml.render(SnakeGameWorld.newSnakeGameWorld)
      assert(countText(node, "🐍") === 2)
    }
  }

}
