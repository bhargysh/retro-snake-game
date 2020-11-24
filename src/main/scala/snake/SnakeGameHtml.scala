package snake

import org.scalajs.dom.raw.Element
import org.scalajs.dom.{Document, Node}
import snake.components.ConvertToHtml.convert
import snake.components.{GameBoard, SnakeGameContainer}

class SnakeGameHtml(document: Document) {
  def render(snakeGameWorld: SnakeGameWorld): Node = {
    val container = SnakeGameContainer(Vector(GameBoard(snakeGameWorld)))
    val boardElements: Vector[components.Element] = container.render()

    val boardHtmlElements: Vector[Element] = boardElements.map(convert(_, document))
    boardHtmlElements.head
  }

}
