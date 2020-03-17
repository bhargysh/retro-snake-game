package snake

import org.scalajs.dom.{Node, document}
import org.specs2.mutable.Specification

class SnakeGameHtmlSpec extends Specification {
  println("<<<<<<<<<<<<<<<<<<<<<<<<<")
  def countText(node: Node, text: String): Int = {
    if (node.nodeType == Node.TEXT_NODE) {
      if (node.nodeValue == text) {
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
  "SnakeGameHtml" should {
      "render the snake game world" in {
        val snakeHtml = new SnakeGameHtml(document)
        val node = snakeHtml.render(SnakeGameWorld.newSnakeGameWorld)
        countText(node, "🐍") must beEqualTo(2)
      }
  }
}
