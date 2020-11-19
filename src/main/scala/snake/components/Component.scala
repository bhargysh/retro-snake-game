package snake.components

import snake.{EmptyCell, FoodCell, FoodPresent, Location, MoveNumber, SnakeGameWorld, SnakePart, Wall}

sealed trait Node
case class TextNode(data: String) extends Node
case class ElementNode(data: Element) extends Node

case class Element(tag: String, classes: Vector[String], style: Option[String], children: Vector[Node])

trait Component {

  def render(): Vector[Element]
}

case class Cell(cell: snake.Cell, x: Int, y: Int, boardHeight: Int) extends Component {
  def render(): Vector[Element] = {
    val text = cell match {
      case SnakePart => "🐍"
      case Wall => "🛑"
      case EmptyCell => " "
      case FoodCell => "🍕"
    }
    Vector(
      Element("div",
      Vector.empty,
      Some(s"grid-column: ${x+1}; grid-row: ${boardHeight - y};"),
      Vector(TextNode(text))))
  }
}

case class GameOver(isPlaying: Boolean) extends Component {
  def render(): Vector[Element] = {
    if (!isPlaying) {
      val gameOverTextNode = Element("span", Vector("gameover-span"), None, Vector(TextNode("GAME OVER")))
      Vector(Element("div", Vector("gameover"), None, Vector(ElementNode(gameOverTextNode))))
    }
    else {
      Vector.empty[Element]
    }
  }
}

case class GameBoard(snakeGameWorld: SnakeGameWorld) extends Component {
  def render(): Vector[Element] = {
    def putSnakeOn(snakeGameWorld: SnakeGameWorld): SnakeGameWorld = { //TODO: move to where appropriate
      def getIndex(location: Location): Int = location match {
        case Location(x, y) => snakeGameWorld.board.cellIndex(x, y)

      }
      def foodVisible(expiryTime: MoveNumber, moveNumber: MoveNumber): Boolean = {
        val blinkTime = expiryTime - moveNumber
        if (blinkTime > 4) true
        else blinkTime % 2 == 0
      } //TODO: move to where appropriate
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

    val nodes = for {
      y <- Range(0, snakeGameWorld.board.height)
      x <- Range(0, snakeGameWorld.board.width)
      newSnakeGameWorld = putSnakeOn(snakeGameWorld)
      cell = Cell(newSnakeGameWorld.board.cellAt(Location(x, y)), x, y, snakeGameWorld.board.height)
      element <- cell.render()
      node = ElementNode(element)
    } yield node

    val children = nodes.toVector

    val gameOverElementNodes: Vector[ElementNode] = GameOver(snakeGameWorld.isPlaying).render().map(elem => ElementNode(elem))

    val board: ElementNode = ElementNode(Element("div", Vector("board"), None, children))
    val container = Element("div", Vector("container"), None, board +: gameOverElementNodes)
    Vector(container)
  }
}

case class SnakeGameContainer(children: Vector[Component]) extends Component {
  def render(): Vector[Element] = {
    val elementNodes: Vector[ElementNode] = for {
      element <- children.flatMap(_.render())
    } yield ElementNode(element)
    Vector(Element("div", Vector("container"), None, elementNodes))
  }
}

//TODO: create other components as mentioned above