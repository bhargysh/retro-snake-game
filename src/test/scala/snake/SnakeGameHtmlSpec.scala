package snake

import org.scalajs.dom.{Node, document}
import org.specs2.mutable.Specification
//import org.scalatest.funspec.AnyFunSpec
import org.scalajs.dom.Window

class SnakeGameHtmlSpec extends Specification  {
  println("<<<<<<<<<<<<<<<<<<<<<<<<<")
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

  "SnakeGameHtml" should {
    "render snake game world" in {
      val snakeHtml = new SnakeGameHtml(jsDom.window.document)
      val node = snakeHtml.render(SnakeGameWorld.newSnakeGameWorld)
      println(node)
      countText(node, "🐍") must beEqualTo(2)
    }
  }

}
