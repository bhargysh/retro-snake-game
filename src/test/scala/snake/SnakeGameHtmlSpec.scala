package snake

import org.scalajs.dom.{Node, document}
import org.specs2.mutable.Specification

class SnakeGameHtmlSpec extends Specification {
  def findNodesWithText(node: Node, text: String): Vector[Node] = {
    if (node.nodeType == Node.TEXT_NODE) {
      if (node.nodeValue == text) {
        Vector(node)
      }
      else {
        Vector.empty
      }
    }
    else {
      Range(0, node.childNodes.length).flatMap { i =>
        findNodesWithText(node.childNodes.item(i), text)
      }.toVector
    }
  }
  "SnakeGameHtml" should {
      "render the snake game world" in {
        val snakeHtml = new SnakeGameHtml(document)
        val node = snakeHtml.render(SnakeGameWorld.newSnakeGameWorld)
        findNodesWithText(node, "🐍") must haveLength(2)
      }
      "render the food if it should be visible" in {
        val snakeHtml = new SnakeGameHtml(document)
        val snakeGameWorldWithFood = SnakeGameWorld.newSnakeGameWorld.copy(food = FoodPresent(Location(1, 1), 200))
        val node = snakeHtml.render(snakeGameWorldWithFood)
        findNodesWithText(node, "🍕") must contain((n: Node) => {
          val parentNode = n.parentNode
          parentNode.attributes.getNamedItem("style").value mustEqual("grid-column: 2; grid-row: 9;")
        })
      }
    "render no food if it should not be visible" in {
      val snakeHtml = new SnakeGameHtml(document)
      val snakeGameWorldWithFood = SnakeGameWorld.newSnakeGameWorld.copy(food = FoodPresent(Location(1, 1), 1))
      val node = snakeHtml.render(snakeGameWorldWithFood)
      findNodesWithText(node, "🍕") must beEmpty
    }
    "render no food if it is not present" in {
      val snakeHtml = new SnakeGameHtml(document)
      val snakeGameWorldWithFood = SnakeGameWorld.newSnakeGameWorld.copy(food = FoodAbsent(1))
      val node = snakeHtml.render(snakeGameWorldWithFood)
      findNodesWithText(node, "🍕") must beEmpty
    }
  }
}
