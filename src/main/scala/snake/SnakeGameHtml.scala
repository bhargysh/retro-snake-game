package snake

import org.scalajs.dom.raw.Element
import org.scalajs.dom.{Document, Node}

class SnakeGameHtml(document: Document) {
  private def createElement(tag: String, parentElement: Element, classes: String*): Element = {
    val childElement = document.createElement(tag)
    classes.foreach { className =>
      childElement.classList.add(className)
    }
    parentElement.appendChild(childElement)
    childElement
  }

  private def createTextElement(data: String, tag: String, parentElement: Element, classes: String*): Element = {
    val text = document.createTextNode(data)
    val element = createElement(tag, parentElement, classes:_*)
    element.appendChild(text)
    element
  }

  def render(snakeGameWorld: SnakeGameWorld): Node = {
    val container = document.createElement("div")
    container.classList.add("container")

    val board = createElement("div", container, "board")

    if (!snakeGameWorld.isPlaying) {
      val gameOverElement = createElement("div", container,"gameover")
      createTextElement("GAME OVER", "span", gameOverElement, "gameover-span")
    }

    val newSnakeGameWorld = putSnakeOn(snakeGameWorld)
    Range(0, snakeGameWorld.board.height).map { y =>
      Range(0, snakeGameWorld.board.width).map { x =>
        val cell = createTextElement(
          cellEmoji(newSnakeGameWorld.board.cellAt(Location(x, y))), "div", board
        )
        cell.setAttribute("style", s"grid-column: ${x+1}; grid-row: ${snakeGameWorld.board.height - y};")
      }
    }
    container
  }

  def putSnakeOn(snakeGameWorld: SnakeGameWorld): SnakeGameWorld = {
    def getIndex(location: Location): Int = location match {
      case Location(x, y) => snakeGameWorld.board.cellIndex(x, y)

    }
    val currentSnakeLocation = snakeGameWorld.snake.location.map(getIndex)
    val currentFoodLocation = snakeGameWorld.food match {
      case FoodPresent(location, expiryTime) if foodVisible(expiryTime, snakeGameWorld.moveNumber) => Some(getIndex(location))
      case _ => None
    }

    val newCells = snakeGameWorld.board.cell.zipWithIndex.map {
      case (cell, index) =>
        if(currentSnakeLocation.contains(index)) SnakePart
        else if(currentFoodLocation.contains(index)) FoodCell
        else cell
    }
    snakeGameWorld.copy(board = snakeGameWorld.board.copy(cell = newCells))

  }

  def cellEmoji(cell: Cell): String = cell match {
    case SnakePart => "🐍"
    case Wall => "🛑"
    case EmptyCell => " "
    case FoodCell => "🍕"
  }

  private def foodVisible(expiryTime: Int, moveNumber: Int): Boolean = {
    val blinkTime = expiryTime - moveNumber
    if (blinkTime > 4) true
    else blinkTime % 2 == 0
  }

}
